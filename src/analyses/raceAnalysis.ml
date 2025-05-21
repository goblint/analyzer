(** Data race analysis ([race]). *)

open GoblintCil
open Analyses

(** Data race analysis with tries for offsets and type-based memory locations for open code.

    Accesses are to memory locations ({{!Access.Memo} memos}) which consist of a root and offset.
    {{!Access.MemoRoot} Root} can be:
    + variable, if access is to known global variable or alloc-variable;
    + type, if access is to unknown pointer.

    Accesses are (now) collected to sets for each corresponding memo,
    after points-to sets are resolved, during postsolving.

    Race checking is performed per-memo,
    except must additionally account for accesses to other memos (see diagram below):
    + access to [s.f] can race with access to a prefix like [s], which writes an entire struct at once;
    + access to [s.f] can race with type-based access like [(struct S).f];
    + access to [(struct S).f] can race with type-based access to a suffix like [(int)].
    + access to [(struct T).s.f] can race with type-based access like [(struct S)], which is a combination of the above.

    These are accounted for lazily (unlike in the past).

    Prefixes (a.k.a. inner distribution) are handled using a trie data structure enriched with lattice properties.
    Race checking starts at the root and passes accesses to ancestor nodes down to children.

    Type suffixes (a.k.a. outer distribution) are handled by computing successive immediate type suffixes transitively
    and accessing corresponding offsets from corresponding root tries in the global invariant.

    Type suffix prefixes (for the combination of the two) are handled by passing type suffix accesses down when traversing the prefix trie.

    Race checking happens at each trie node with the above three access sets at hand using {!Access.group_may_race}.
    All necessary combinations between the four classes are handled, but unnecessary repeated work is carefully avoided.
    E.g. accesses which are pairwise checked at some prefix are not re-checked pairwise at a node.
    Thus, races (with prefixes or type suffixes) are reported for most precise memos with actual accesses:
    at the longest prefix and longest type suffix.

    Additionally, accesses between prefix and type suffix intersecting at a node are checked.
    These races are reported at the unique memo at the intersection of the prefix and the type suffix.
    This requires an implementation hack to still eagerly do outer distribution, but only of empty access sets.
    It ensures that corresponding trie nodes exist for traversal later. *)

(** Given C declarations:
    {@c[
    struct S {
      int f;
    };

    struct T {
      struct S s;
    };

    struct T t;
    ]}

    Example structure of related memos for race checking:
    {v
     (int)   (S)     (T)
        \   /   \   /   \
          f       s       t
            \   /   \   /
              f       s
                \   /
                  f
    v}
    where:
    - [(int)] is a type-based memo root for the primitive [int] type;
    - [(S)] and [(T)] are short for [(struct S)] and [(struct T)], which are type-based memo roots;
    - prefix relations are indicated by [/], so access paths run diagonally from top-right to bottom-left;
    - type suffix relations are indicated by [\ ].

    All same-node races:
    - Race between [t.s.f] and [t.s.f] is checked/reported at [t.s.f].
    - Race between [t.s] and [t.s] is checked/reported at [t.s].
    - Race between [t] and [t] is checked/reported at [t].
    - Race between [(T).s.f] and [(T).s.f] is checked/reported at [(T).s.f].
    - Race between [(T).s] and [(T).s] is checked/reported at [(T).s].
    - Race between [(T)] and [(T)] is checked/reported at [(T)].
    - Race between [(S).f] and [(S).f] is checked/reported at [(S).f].
    - Race between [(S)] and [(S)] is checked/reported at [(S)].
    - Race between [(int)] and [(int)] is checked/reported at [(int)].

    All prefix races:
    - Race between [t.s.f] and [t.s] is checked/reported at [t.s.f].
    - Race between [t.s.f] and [t] is checked/reported at [t.s.f].
    - Race between [t.s] and [t] is checked/reported at [t.s].
    - Race between [(T).s.f] and [(T).s] is checked/reported at [(T).s.f].
    - Race between [(T).s.f] and [(T)] is checked/reported at [(T).s.f].
    - Race between [(T).s] and [(T)] is checked/reported at [(T).s].
    - Race between [(S).f] and [(S)] is checked/reported at [(S).f].

    All type suffix races:
    - Race between [t.s.f] and [(T).s.f] is checked/reported at [t.s.f].
    - Race between [t.s.f] and [(S).f] is checked/reported at [t.s.f].
    - Race between [t.s.f] and [(int)] is checked/reported at [t.s.f].
    - Race between [(T).s.f] and [(S).f] is checked/reported at [(T).s.f].
    - Race between [(T).s.f] and [(int)] is checked/reported at [(T).s.f].
    - Race between [(S).f] and [(int)] is checked/reported at [(S).f].
    - Race between [t.s] and [(T).s] is checked/reported at [t.s].
    - Race between [t.s] and [(S)] is checked/reported at [t.s].
    - Race between [(T).s] and [(S)] is checked/reported at [(T).s].
    - Race between [t] and [(T)] is checked/reported at [t].

    All type suffix prefix races:
    - Race between [t.s.f] and [(T).s] is checked/reported at [t.s.f].
    - Race between [t.s.f] and [(T)] is checked/reported at [t.s.f].
    - Race between [t.s.f] and [(S)] is checked/reported at [t.s.f].
    - Race between [(T).s.f] and [(S)] is checked/reported at [(T).s.f].
    - Race between [t.s] and [(T)] is checked/reported at [t.s].

    All prefix-type suffix races:
    - Race between [t.s] and [(T).s.f] is checked/reported at [t.s.f].
    - Race between [t.s] and [(S).f] is checked/reported at [t.s.f].
    - Race between [t.s] and [(int)] is checked/reported at [t.s.f].
    - Race between [t] and [(T).s.f] is checked/reported at [t.s.f].
    - Race between [t] and [(S).f] is checked/reported at [t.s.f].
    - Race between [t] and [(int)] is checked/reported at [t.s.f].
    - Race between [t] and [(T).s] is checked/reported at [t.s].
    - Race between [t] and [(S)] is checked/reported at [t.s].
    - Race between [(T).s] and [(S).f] is checked/reported at [(T).s.f].
    - Race between [(T).s] and [(int)] is checked/reported at [(T).s.f].
    - Race between [(T)] and [(S).f] is checked/reported at [(T).s.f].
    - Race between [(T)] and [(int)] is checked/reported at [(T).s.f].
    - Race between [(T)] and [(S)] is checked/reported at [(T).s].
    - Race between [(S)] and [(int)] is checked/reported at [(S).f]. *)


(** Data race analyzer without base --- this is the new standard *)
module Spec =
struct
  include UnitAnalysis.Spec

  let name () = "race"

  (* Two global invariants:
     1. memoroot -> (offset --trie--> accesses)  --  used for warnings
     2. varinfo -> set of memo  --  used for IterSysVars Global *)

  module V =
  struct
    include Printable.Either (Access.MemoRoot) (CilType.Varinfo)
    let name () = "race"
    let access x = `Left x
    let vars x = `Right x
    let is_write_only _ = true
  end

  module MemoSet = SetDomain.Make (Access.Memo)

  module OneOffset =
  struct
    include Printable.StdLeaf
    type t =
      | Field of CilType.Fieldinfo.t
      | Index
    [@@deriving eq, ord, hash, to_yojson]

    let name () = "oneoffset"

    let show = function
      | Field f -> CilType.Fieldinfo.show f
      | Index -> "?"

    include Printable.SimpleShow (struct
        type nonrec t = t
        let show = show
      end)

    let to_offset : t -> Offset.Unit.t = function
      | Field f -> `Field (f, `NoOffset)
      | Index -> `Index ((), `NoOffset)
  end

  module OffsetTrie =
  struct
    (* LiftBot such that add_distribute_outer can side-effect empty set to indicate
       all offsets that exist for prefix-type_suffix race checking.
       Otherwise, there are no trie nodes to traverse to where this check must happen. *)
    include TrieDomain.Make (OneOffset) (Lattice.LiftBot (Access.AS))

    let rec find (offset : Offset.Unit.t) ((accs, children) : t) : value =
      match offset with
      | `NoOffset -> accs
      | `Field (f, offset') -> find offset' (ChildMap.find (Field f) children)
      | `Index ((), offset') -> find offset' (ChildMap.find Index children)

    let rec singleton (offset : Offset.Unit.t) (value : value) : t =
      match offset with
      | `NoOffset -> (value, ChildMap.empty ())
      | `Field (f, offset') -> (`Bot, ChildMap.singleton (Field f) (singleton offset' value))
      | `Index ((), offset') -> (`Bot, ChildMap.singleton Index (singleton offset' value))
  end

  module G =
  struct
    include Lattice.Lift2Conf (struct include Printable.DefaultConf let expand1 = false let expand2 = false end) (OffsetTrie) (MemoSet)

    let access = function
      | `Bot -> OffsetTrie.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "Race.access"
    let vars = function
      | `Bot -> MemoSet.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "Race.vars"
    let create_access access = `Lifted1 access
    let create_vars vars = `Lifted2 vars
  end

  let safe       = ref 0
  let vulnerable = ref 0
  let unsafe     = ref 0

  let init marshal =
    safe := 0;
    vulnerable := 0;
    unsafe := 0

  let side_vars man memo =
    match memo with
    | (`Var v, _) ->
      if !AnalysisState.should_warn then
        man.sideg (V.vars v) (G.create_vars (MemoSet.singleton memo))
    | _ ->
      ()

  let side_access man acc ((memoroot, offset) as memo) =
    if !AnalysisState.should_warn then
      man.sideg (V.access memoroot) (G.create_access (OffsetTrie.singleton offset (`Lifted (Access.AS.singleton acc))));
    side_vars man memo

  (** Side-effect empty access set for prefix-type_suffix race checking. *)
  let side_access_empty man ((memoroot, offset) as memo) =
    if !AnalysisState.should_warn then
      man.sideg (V.access memoroot) (G.create_access (OffsetTrie.singleton offset (`Lifted (Access.AS.empty ()))));
    side_vars man memo

  (** Get immediate type_suffix memo. *)
  let type_suffix_memo ((root, offset) : Access.Memo.t) : Access.Memo.t option =
    (* No need to make ana.race.direct-arithmetic return None here,
       because (int) is empty anyway since Access.add_distribute_outer isn't called. *)
    match root, offset with
    | `Var v, _ -> Some (`Type (Cil.typeSig v.vtype), offset) (* global.foo.bar -> (struct S).foo.bar *) (* TODO: Alloc variables void type *)
    | _, `NoOffset -> None (* primitive type *)
    | _, `Field (f, offset') -> Some (`Type (Cil.typeSig f.ftype), offset') (* (struct S).foo.bar -> (struct T).bar *)
    | `Type (TSArray (ts, _, _)), `Index ((), offset') -> Some (`Type ts, offset') (* (int[])[*] -> int *)
    | _, `Index ((), offset') -> None (* TODO: why indexing on non-array? *)

  let rec find_type_suffix' man ((root, offset) as memo : Access.Memo.t) : Access.AS.t =
    let trie = G.access (man.global (V.access root)) in
    let accs =
      match OffsetTrie.find offset trie with
      | `Lifted accs -> accs
      | `Bot -> Access.AS.empty ()
    in
    let type_suffix = find_type_suffix man memo in
    Access.AS.union accs type_suffix

  (** Find accesses from all type_suffixes transitively. *)
  and find_type_suffix man (memo : Access.Memo.t) : Access.AS.t =
    match type_suffix_memo memo with
    | Some type_suffix_memo -> find_type_suffix' man type_suffix_memo
    | None -> Access.AS.empty ()

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' -> (* accesses *)
          (* Logs.debug "WarnGlobal %a" Access.MemoRoot.pretty g'; *)
          let trie = G.access (man.global g) in
          (** Distribute access to contained fields. *)
          let rec distribute_inner offset (accs, children) ~prefix ~type_suffix_prefix =
            let accs =
              match accs with
              | `Lifted accs -> accs
              | `Bot -> Access.AS.empty ()
            in
            let type_suffix = find_type_suffix man (g', offset) in
            if not (Access.AS.is_empty accs) || (not (Access.AS.is_empty prefix) && not (Access.AS.is_empty type_suffix)) then (
              let memo = (g', offset) in
              let mem_loc_str = GobPretty.sprint Access.Memo.pretty memo in
              Timing.wrap ~args:[("memory location", `String mem_loc_str)] "race" (Access.warn_global ~safe ~vulnerable ~unsafe {node=accs; prefix; type_suffix; type_suffix_prefix}) memo
            );

            (* Recurse to children. *)
            let prefix' = Access.AS.union prefix accs in
            let type_suffix_prefix' = Access.AS.union type_suffix_prefix type_suffix in
            OffsetTrie.ChildMap.iter (fun child_key child_trie ->
                distribute_inner (Offset.Unit.add_offset offset (OneOffset.to_offset child_key)) child_trie ~prefix:prefix' ~type_suffix_prefix:type_suffix_prefix'
              ) children;
          in
          distribute_inner `NoOffset trie ~prefix:(Access.AS.empty ()) ~type_suffix_prefix:(Access.AS.empty ())
        | `Right _ -> (* vars *)
          ()
      end
    | IterSysVars (Global g, vf) ->
      MemoSet.iter (fun v ->
          vf (Obj.repr (V.access v))
        ) (G.vars (man.global (V.vars g)))
    | _ -> Queries.Result.top q

  let event man e oman =
    match e with
    | Events.Access {exp; ad; kind; reach} when ThreadFlag.is_currently_multi (Analyses.ask_of_man man) -> (* threadflag query in post-threadspawn man *)
      (* must use original (pre-assign, etc) man queries *)
      let conf = 110 in
      let module AD = Queries.AD in
      let part_access (vo:varinfo option): MCPAccess.A.t =
        (*partitions & locks*)
        Obj.obj (oman.ask (PartAccess (Memory {exp; var_opt=vo; kind})))
      in
      let node = man.prev_node in
      let add_access conf voffs =
        let acc = part_access (Option.map fst voffs) in
        Access.add ~side:(side_access oman {conf; kind; node; exp; acc}) ~side_empty:(side_access_empty oman) exp voffs;
      in
      let add_access_struct conf ci =
        let acc = part_access None in
        Access.add_one ~side:(side_access oman {conf; kind; node; exp; acc}) (`Type (TSComp (ci.cstruct, ci.cname, [])), `NoOffset)
      in
      let has_escaped g = oman.ask (Queries.MayEscape g) in
      (* The following function adds accesses to the lval-set ls
         -- this is the common case if we have a sound points-to set. *)
      let on_ad ad includes_uk =
        let conf = if reach then conf - 20 else conf in
        let conf = if includes_uk then conf - 10 else conf in
        let f addr =
          match addr with
          | AD.Addr.Addr (g,o) when g.vglob || has_escaped g ->
            let coffs = ValueDomain.Offs.to_cil o in
            add_access conf (Some (g, coffs))
          | UnknownPtr -> add_access conf None
          | _ -> ()
        in
        AD.iter f ad
      in
      begin match ad with
        | ad when not (AD.is_top ad) ->
          (* the case where the points-to set is non top and does not contain unknown values *)
          on_ad ad false
        | ad ->
          (* the case where the points-to set is non top and contains unknown values *)
          let includes_uk = ref false in
          (* now we need to access all fields that might be pointed to: is this correct? *)
          begin match oman.ask (ReachableUkTypes exp) with
            | ts when Queries.TS.is_top ts ->
              includes_uk := true
            | ts ->
              if not (Queries.TS.is_empty ts) then
                includes_uk := true;
              let f t =
                match Cil.unrollType t with
                | TComp (ci, _) ->
                  add_access_struct (conf - 50) ci
                | _ -> ()
              in
              Queries.TS.iter f ts
          end;
          on_ad ad !includes_uk
          (* | _ ->
             add_access (conf - 60) None *) (* TODO: what about this case? *)
      end;
      man.local
    | _ ->
      man.local

  let special man (lvalOpt: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* perform shallow and deep invalidate according to Library descriptors *)
    let desc = LibraryFunctions.find f in
    if List.mem LibraryDesc.ThreadUnsafe desc.attrs && ThreadFlag.is_currently_multi (Analyses.ask_of_man man) then (
      let exp = Lval (Var f, NoOffset) in
      let conf = 110 in
      let kind = AccessKind.Call in
      let node = man.prev_node in
      let vo = Some f in
      let acc = Obj.obj (man.ask (PartAccess (Memory {exp; var_opt=vo; kind}))) in
      side_access man {conf; kind; node; exp; acc} ((`Var f), `NoOffset) ;
    );
    man.local

  let finalize () =
    let total = !safe + !unsafe + !vulnerable in
    if total > 0 then (
      M.msg_group Info ~category:Race "Memory locations race summary" [
        (Pretty.dprintf "safe: %d" !safe, None);
        (Pretty.dprintf "vulnerable: %d" !vulnerable, None);
        (Pretty.dprintf "unsafe: %d" !unsafe, None);
        (Pretty.dprintf "total memory locations: %d" total, None);
      ];
    )
end

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
