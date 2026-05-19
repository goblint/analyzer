(** Memory accesses and their manipulation. *)

open GoblintCil
open Pretty
open GobConfig

module M = Messages

(* Some helper functions to avoid flagging race warnings on atomic types, and
 * other irrelevant stuff, such as mutexes and functions. *)

let is_ignorable_comp_name = function
  | "__pthread_mutex_s" | "__pthread_rwlock_arch_t" | "__jmp_buf_tag" | "_pthread_cleanup_buffer" | "__pthread_cleanup_frame" | "__cancel_jmp_buf_tag" | "_IO_FILE" -> true
  | cname when String.starts_with ~prefix:"__anon" cname ->
    begin match Cilfacade.split_anoncomp_name cname with
      | (true, Some ("__once_flag" | "__pthread_unwind_buf_t" | "__cancel_jmp_buf"), _) -> true (* anonstruct *)
      | (false, Some ("pthread_mutexattr_t" | "pthread_condattr_t" | "pthread_barrierattr_t"), _) -> true (* anonunion *)
      | _ -> false
    end
  | "lock_class_key" -> true (* kernel? *)
  | _ -> false

let is_ignorable_attrs attrs =
  let is_ignorable_attr = function
    | Attr ("volatile", _) when not (get_bool "ana.race.volatile") -> true (* volatile & races on volatiles should not be reported *)
    | Attr ("atomic", _) -> true (* C11 _Atomic *)
    | _ -> false
  in
  List.exists is_ignorable_attr attrs

let rec is_ignorable_type (t: typ): bool =
  (* efficient pattern matching first *)
  match t with
  | TNamed ({ tname = "atomic_t" | "pthread_mutex_t" | "pthread_once_t" | "pthread_rwlock_t" | "pthread_spinlock_t" | "spinlock_t" | "pthread_cond_t" | "atomic_flag" | "FILE" | "__FILE"; _ }, _) -> true
  | TComp ({ cname; _}, _) when is_ignorable_comp_name cname -> true
  | TInt (IInt, attr) when hasAttribute "mutex" attr -> true (* kernel? *)
  | TFun _ -> true
  | _ ->
    if is_ignorable_attrs (typeAttrsOuter t) then (* only outer because we unroll TNamed ourselves *)
      true
    else (
      (* unroll TNamed once *)
      (* can't use unrollType because we want to check TNamed-s at all intermediate typedefs as well *)
      match t with
      | TNamed ({ttype; _}, attrs) -> is_ignorable_type (typeAddAttributes attrs ttype)
      | _ -> false
    )

let rec is_ignorable_type_offset (t: typ) (o: _ Offset.t): bool =
  (* similar to Cilfacade.typeOffset but we want to check types at all intermediate offsets as well *)
  if is_ignorable_type t then
    true (* type at offset so far ignorable, no need to recurse *)
  else (
    match o with
    | `NoOffset -> false (* already checked t *)
    | `Index (_, o') ->
      begin match unrollType t with
        | TArray (et, _, attrs) ->
          let t' = Cilfacade.typeBlendAttributes attrs et in
          is_ignorable_type_offset t' o'
        | _ -> false (* index on non-array *)
      end
    | `Field (f, o') ->
      begin match unrollType t with
        | TComp (_, attrs) ->
          let t' = Cilfacade.typeBlendAttributes attrs f.ftype in
          is_ignorable_type_offset t' o'
        | _ -> false (* field on non-compound *)
      end
  )

(** {!is_ignorable_type} for {!typsig}. *)
let is_ignorable_typsig (ts: typsig): bool =
  (* efficient pattern matching first *)
  match ts with
  | TSComp (_, cname, _) when is_ignorable_comp_name cname -> true
  | TSFun _ -> true
  | TSBase t -> is_ignorable_type t
  | _ -> is_ignorable_attrs (typeSigAttrs ts)

(** {!is_ignorable_type_offset} for {!typsig}. *)
let rec is_ignorable_typsig_offset (ts: typsig) (o: _ Offset.t): bool =
  if is_ignorable_typsig ts then
    true (* type at offset so far ignorable, no need to recurse *)
  else (
    match o with
    | `NoOffset -> false (* already checked t *)
    | `Index (_, o') ->
      begin match ts with
        | TSArray (ets, _, attrs) ->
          let ts' = Cilfacade.typeSigBlendAttributes attrs ets in
          is_ignorable_typsig_offset ts' o'
        | _ -> false (* index on non-array *)
      end
    | `Field (f, o') ->
      begin match ts with
        | TSComp (_, _, attrs) ->
          let t' = Cilfacade.typeBlendAttributes attrs f.ftype in
          is_ignorable_type_offset t' o' (* switch to type because it is more precise with TNamed *)
        | _ -> false (* field on non-compound *)
      end
  )

let is_ignorable_mval = function
  | ({vaddrof = false; vattr; _}, _) when hasAttribute "thread" vattr -> true (* Thread-Local Storage *)
  | (v, o) -> is_ignorable_type_offset v.vtype o (* can't use Cilfacade.typeOffset because we want to check types at all intermediate offsets as well *)

let is_ignorable_memo = function
  | (`Type ts, o) -> is_ignorable_typsig_offset ts o
  | (`Var v, o) -> is_ignorable_mval (v, o)

module TSH = Hashtbl.Make (CilType.Typsig)

let typeVar  = TSH.create 101
let typeIncl = TSH.create 101
let collect_direct_arithmetic = ref false

let init (f:file) =
  collect_direct_arithmetic := get_bool "ana.race.direct-arithmetic";
  let visited_vars = Hashtbl.create 100 in
  let add tsh t v =
    let rec add' ts =
      TSH.add tsh ts v;
      (* Account for aliasing to any level of array.
         See 06-symbeq/50-type_array_via_ptr_rc.c. *)
      match ts with
      | TSArray (ts', _, _) -> add' ts'
      | _ -> ()
    in
    if not (is_ignorable_type t) then
      add' (typeSig t)
  in
  let visit_field fi =
    (* TODO: is_ignorable_type? *)
    (* TODO: Direct ignoring doesn't really work since it doesn't account for pthread inner structs/unions being only reachable via ignorable types. *)
    add typeIncl fi.ftype fi
  in
  let visit_glob = function
    | GCompTag (c,_) ->
      if not (is_ignorable_type (TComp (c, []))) then
        List.iter visit_field c.cfields
    | GVarDecl (v,_) | GVar (v,_,_) ->
      if not (Hashtbl.mem visited_vars v.vid) then begin
        (* TODO: is_ignorable? *)
        add typeVar v.vtype v;
        (* ignore (printf "init adding %s : %a" v.vname d_typsig ((typeSig v.vtype))); *)
        Hashtbl.replace visited_vars v.vid true
      end
    | _ -> ()
  in
  List.iter visit_glob f.globals

let reset () =
  TSH.clear typeVar;
  TSH.clear typeIncl

type acc_typ = [ `Type of CilType.Typ.t | `Struct of CilType.Compinfo.t * Offset.Unit.t ] [@@deriving eq, ord, hash]
(** Old access type inferred from an expression. *)

module MemoRoot =
struct
  include Printable.StdLeaf
  type t = [`Var of CilType.Varinfo.t | `Type of CilType.Typsig.t] [@@deriving eq, ord, hash]

  let name () = "memoroot"

  let pretty () vt =
    (* Imitate old printing for now *)
    match vt with
    | `Var v -> CilType.Varinfo.pretty () v
    | `Type (TSComp (_, name, _)) -> Pretty.dprintf "(struct %s)" name
    | `Type t -> Pretty.dprintf "(%a)" Cilfacade.pretty_typsig_like_typ t

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

(** Memory location of an access. *)
module Memo =
struct
  include Printable.StdLeaf
  type t = MemoRoot.t * Offset.Unit.t [@@deriving eq, ord, hash]

  let name () = "memo"

  let pretty () (vt, o) =
    (* Imitate old printing for now *)
    match vt with
    | `Var v -> Pretty.dprintf "%a%a" CilType.Varinfo.pretty v Offset.Unit.pretty o
    | `Type (TSComp (_, name, _)) -> Pretty.dprintf "(struct %s)%a" name Offset.Unit.pretty o
    | `Type t -> Pretty.dprintf "(%a)%a" Cilfacade.pretty_typsig_like_typ t Offset.Unit.pretty o

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let of_ty (ty: acc_typ): t =
    match ty with
    | `Struct (c, o) -> (`Type (TSComp (c.cstruct, c.cname, [])), o)
    | `Type t -> (`Type (Cil.typeSig t), `NoOffset)

  let to_mval: t -> Mval.Unit.t option = function
    | (`Var v, o) -> Some (v, o)
    | (`Type _, _) -> None

  let add_offset ((vt, o): t) o2: t = (vt, Offset.Unit.add_offset o o2)
end

(* TODO: What is the logic for get_type? *)
let rec get_type (fb: typ Lazy.t) : exp -> acc_typ = function
  | AddrOf (h,o) | StartOf (h,o) ->
    let rec f htyp =
      match htyp with
      | TComp (ci,_) -> `Struct (ci, Offset.Unit.of_cil o)
      | TNamed (ti,_) -> f ti.ttype
      | _ -> `Type (Lazy.force fb) (* TODO: Why fb not htyp? *)
    in
    begin match o with
      | Field (f, on) -> `Struct (f.fcomp, Offset.Unit.of_cil o)
      | NoOffset | Index _ ->
        begin match h with
          | Var v -> f (v.vtype)
          | Mem e -> f (Lazy.force fb) (* TODO: type of Mem doesn't have to be the fallback type if offsets present? *)
        end
    end
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ | AddrOfLabel _  ->
    `Type (uintType) (* TODO: Correct types from typeOf? *)
  | UnOp (_,_,t) -> `Type t
  | BinOp (_,_,_,t) -> `Type t
  | CastE (_,t,e) ->
    begin match get_type fb e with
      | `Struct s -> `Struct s
      | _         -> `Type t
    end
  | Question (_,b,c,t) ->
    begin match get_type fb b, get_type fb c with
      | `Struct (s1,o1), `Struct (s2,o2)
        when CilType.Compinfo.equal s1 s2 && Offset.Unit.equal o1 o2 ->
        `Struct (s1, o1)
      | _ -> `Type t
    end
  | Const _
  | Lval _
  | Real _
  | Imag _ ->
    `Type (Lazy.force fb) (* TODO: is this right? *)

let get_type fb e =
  (* printf "e = %a\n" d_plainexp e; *)
  let r = get_type fb e in
  (* printf "result = %a\n" d_acct r; *)
  match r with
  | `Type t as x ->
    begin match Cil.unrollType t with
      | TPtr (t, a) ->  `Type t (* Why this special case? Almost always taken if not `Struct. *)
      | _ -> x
    end
  | x -> x (* Mostly for `Struct, but also rare cases with non-pointer `Type. Should they happen at all? *)

let get_val_type e: acc_typ =
  let fb = lazy (
    try Cilfacade.typeOf e
    with Cilfacade.TypeOfError _ -> voidType (* Why is this a suitable default? *)
  )
  in
  get_type fb e


(** Add access to {!Memo} after distributing. *)
let add_one ~side memo: unit =
  let ignorable = is_ignorable_memo memo in
  if M.tracing then M.trace "access" "add_one %a (ignorable = %B)" Memo.pretty memo ignorable;
  if not ignorable then
    side memo

(** Distribute empty access set for type-based access to variables and containing fields.
    Empty access sets are needed for prefix-type_suffix race checking. *)
let rec add_distribute_outer ~side ~side_empty (ts: typsig) (o: Offset.Unit.t) =
  let memo = (`Type ts, o) in
  if M.tracing then M.tracei "access" "add_distribute_outer %a" Memo.pretty memo;
  add_one ~side memo; (* Add actual access for non-recursive call, or empty access for recursive call when side is side_empty. *)

  (* distribute to variables of the type *)
  let vars = TSH.find_all typeVar ts in
  List.iter (fun v ->
      (* same offset, but on variable *)
      add_one ~side:side_empty (`Var v, o) (* Switch to side_empty. *)
    ) vars;

  (* recursively distribute to fields containing the type *)
  let fields = TSH.find_all typeIncl ts in
  List.iter (fun f ->
      (* prepend field and distribute to outer struct *)
      add_distribute_outer ~side:side_empty ~side_empty (TSComp (f.fcomp.cstruct, f.fcomp.cname, [])) (`Field (f, o)) (* Switch to side_empty. *)
    ) fields;

  if M.tracing then M.traceu "access" "add_distribute_outer"

(** Add access to known variable with offsets or unknown variable from expression. *)
let add ~side ~side_empty e voffs =
  begin match voffs with
    | Some (v, o) -> (* known variable *)
      if M.tracing then M.traceli "access" "add var %a%a" CilType.Varinfo.pretty v CilType.Offset.pretty o;
      let memo = (`Var v, Offset.Unit.of_cil o) in
      add_one ~side memo
    | None -> (* unknown variable *)
      if M.tracing then M.traceli "access" "add type %a" CilType.Exp.pretty e;
      let ty = get_val_type e in (* extract old acc_typ from expression *)
      let (t, o) = match ty with (* convert acc_typ to type-based Memo (components) *)
        | `Struct (c, o) -> (TComp (c, []), o)
        | `Type t -> (t, `NoOffset)
      in
      match o with
      | `NoOffset when not !collect_direct_arithmetic && isArithmeticType t -> ()
      | _ -> add_distribute_outer ~side ~side_empty (Cil.typeSig t) o (* distribute to variables and outer offsets *)
  end;
  if M.tracing then M.traceu "access" "add"


(** Distribute to {!AddrOf} of all read lvals in subexpressions. *)

let rec distribute_access_lval f lv =
  (* Use unoptimized AddrOf so RegionDomain.Reg.eval_exp knows about dereference *)
  (* f (mkAddrOf lv); *)
  f (AddrOf lv);
  distribute_access_lval_addr f lv

and distribute_access_lval_addr f lv =
  match lv with
  | (Var v, os) ->
    distribute_access_offset f os
  | (Mem e, os) ->
    distribute_access_offset f os;
    distribute_access_exp f e

and distribute_access_offset f = function
  | NoOffset -> ()
  | Field (_,os) ->
    distribute_access_offset f os
  | Index (e,os) ->
    distribute_access_exp f e;
    distribute_access_offset f os

and distribute_access_exp f = function
  (* Variables and address expressions *)
  | Lval lval ->
    distribute_access_lval f lval;

    (* Binary operators *)
  | BinOp (op,arg1,arg2,typ) ->
    distribute_access_exp f arg1;
    distribute_access_exp f arg2

  | UnOp (_,e,_)
  | Real e
  | Imag e
  | SizeOfE e
  | AlignOfE e ->
    distribute_access_exp f e

  (* The address operators, we just check the accesses under them *)
  | AddrOf lval | StartOf lval ->
    distribute_access_lval_addr f lval

  (* Most casts are currently just ignored, that's probably not a good idea! *)
  | CastE (_, t, exp) ->
    distribute_access_exp f exp
  | Question (b,t,e,_) ->
    distribute_access_exp f b;
    distribute_access_exp f t;
    distribute_access_exp f e

  | SizeOf t ->
    distribute_access_type f t

  | Const _
  | SizeOfStr _
  | AlignOf _
  | AddrOfLabel _ ->
    ()

and distribute_access_type f = function
  | TArray (et, len, _) ->
    Option.iter (distribute_access_exp f) len;
    distribute_access_type f et

  | TVoid _
  | TInt _
  | TFloat _
  | TPtr _
  | TFun _
  | TNamed _
  | TComp _
  | TEnum _
  | TBuiltin_va_list _ ->
    ()


(* Access table as Lattice. *)
(* (varinfo ->) offset -> type -> 2^(confidence, write, loc, e, acc) *)
module A =
struct
  include Printable.Std
  type t = {
    conf : int;
    kind : AccessKind.t;
    node : Node.t;
    exp : CilType.Exp.t;
    acc : MCPAccess.A.t;
  } [@@deriving eq, ord, hash]

  let name () = "access"

  let pretty () {conf; kind; node; exp; acc} =
    Pretty.dprintf "%d, %a, %a, %a, %a" conf AccessKind.pretty kind CilType.Location.pretty (Node.location node) CilType.Exp.pretty exp MCPAccess.A.pretty acc

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let relift {conf; kind; node; exp; acc} =
    {conf; kind; node; exp; acc = MCPAccess.A.relift acc}
end

module AS =
struct
  include SetDomain.Make (A)

  let max_conf accs =
    accs |> elements |> List.map (fun {A.conf; _} -> conf) |> (BatList.max ~cmp:Int.compare)
end


(** Check if two accesses may race. *)
let may_race A.{kind; acc; _} A.{kind=kind2; acc=acc2; _} =
  match kind, kind2 with
  | Read, Read -> false (* two read/read accesses do not race *)
  | Free, _
  | _, Free when not (get_bool "ana.race.free") -> false
  | Call, _
  | _, Call when not (get_bool "ana.race.call") -> false
  | _, _ -> MCPAccess.A.may_race acc acc2 (* analysis-specific information excludes race *)

(** Access sets for race detection and warnings. *)
module WarnAccs =
struct
  type t = {
    node: AS.t; (** Accesses for current memo. From accesses at trie node corresponding to memo offset. *)
    prefix: AS.t; (** Accesses for all prefixes. From accesses to trie node ancestors. *)
    type_suffix: AS.t; (** Accesses for all type suffixes. From offset suffixes in other tries. *)
    type_suffix_prefix: AS.t; (** Accesses to all prefixes of all type suffixes. *)
  }

  let diff w1 w2 = {
    node = AS.diff w1.node w2.node;
    prefix = AS.diff w1.prefix w2.prefix;
    type_suffix = AS.diff w1.type_suffix w2.type_suffix;
    type_suffix_prefix = AS.diff w1.type_suffix_prefix w2.type_suffix_prefix;
  }

  let union_all w =
    AS.union
      (AS.union w.node w.prefix)
      (AS.union w.type_suffix w.type_suffix_prefix)

  let is_empty w =
    AS.is_empty w.node && AS.is_empty w.prefix && AS.is_empty w.type_suffix && AS.is_empty w.type_suffix_prefix

  let empty () =
    {node=AS.empty (); prefix=AS.empty (); type_suffix=AS.empty (); type_suffix_prefix=AS.empty ()}

  let pretty () w =
    Pretty.dprintf "{node = %a; prefix = %a; type_suffix = %a; type_suffix_prefix = %a}"
      AS.pretty w.node AS.pretty w.prefix AS.pretty w.type_suffix AS.pretty w.type_suffix_prefix
end

let group_may_race (warn_accs:WarnAccs.t) =
  if M.tracing then M.tracei "access" "group_may_race %a" WarnAccs.pretty warn_accs;
  (* BFS to traverse one component with may_race edges *)
  let rec bfs' warn_accs ~todo ~visited =
    let todo_all = WarnAccs.union_all todo in
    let visited' = AS.union visited todo_all in (* Add all todo accesses to component. *)
    let warn_accs' = WarnAccs.diff warn_accs todo in (* Todo accesses don't need to be considered as step targets, because they're already in the component. *)

    let step_may_race ~todo ~accs = (* step from todo to accs if may_race *)
      AS.fold (fun acc todo' ->
          AS.fold (fun acc' todo' ->
              if may_race acc acc' then
                AS.add acc' todo'
              else
                todo'
            ) accs todo'
        ) todo (AS.empty ())
    in
    (* Undirected graph of may_race checks:

                type_suffix_prefix
                        |
                        |
          type_suffix --+-- prefix
                     \  |  /
                      \ | /
                       node
                       / \
                       \_/

       Each undirected edge is handled by two opposite step_may_race-s.
       All missing edges are checked at other nodes by other group_may_race calls. *)
    let todo' : WarnAccs.t = {
      node = step_may_race ~todo:todo_all ~accs:warn_accs'.node;
      prefix = step_may_race ~todo:(AS.union todo.node todo.type_suffix) ~accs:warn_accs'.prefix;
      type_suffix = step_may_race ~todo:(AS.union todo.node todo.prefix) ~accs:warn_accs'.type_suffix;
      type_suffix_prefix = step_may_race ~todo:todo.node ~accs:warn_accs'.type_suffix_prefix
    }
    in

    if WarnAccs.is_empty todo' then
      (warn_accs', visited')
    else
      (bfs' [@tailcall]) warn_accs' ~todo:todo' ~visited:visited'
  in
  let bfs warn_accs todo = bfs' warn_accs ~todo ~visited:(AS.empty ()) in
  (* repeat BFS to find all components starting from node accesses *)
  let rec components comps (warn_accs:WarnAccs.t) =
    if AS.is_empty warn_accs.node then
      (comps, warn_accs)
    else (
      let acc = AS.choose warn_accs.node in
      let (warn_accs', comp) = bfs warn_accs {(WarnAccs.empty ()) with node=AS.singleton acc} in
      let comps' = comp :: comps in
      components comps' warn_accs'
    )
  in
  let (comps, warn_accs) = components [] warn_accs in
  if M.tracing then M.trace "access" "components %a" WarnAccs.pretty warn_accs;
  (* repeat BFS to find all prefix-type_suffix-only components starting from prefix accesses (symmetric) *)
  let rec components_cross comps ~prefix ~type_suffix =
    if AS.is_empty prefix then
      comps
    else (
      let prefix_acc = AS.choose prefix in
      let (warn_accs', comp) = bfs {(WarnAccs.empty ()) with prefix; type_suffix} {(WarnAccs.empty ()) with prefix=AS.singleton prefix_acc} in
      if M.tracing then M.trace "access" "components_cross %a" WarnAccs.pretty warn_accs';
      let comps' =
        if AS.cardinal comp > 1 then
          comp :: comps
        else
          comps (* ignore self-race prefix_acc component, self-race checked at prefix's level *)
      in
      components_cross comps' ~prefix:warn_accs'.prefix ~type_suffix:warn_accs'.type_suffix
    )
  in
  let components_cross = components_cross comps ~prefix:warn_accs.prefix ~type_suffix:warn_accs.type_suffix in
  if M.tracing then M.traceu "access" "group_may_race";
  components_cross

let race_conf accs =
  assert (not (AS.is_empty accs)); (* group_may_race should only construct non-empty components *)
  if AS.cardinal accs = 1 then ( (* singleton component *)
    let acc = AS.choose accs in
    if may_race acc acc then (* self-race *)
      Some (acc.conf)
    else
      None
  )
  else
    Some (AS.max_conf accs)

let is_all_safe = ref true

(* Commenting your code is for the WEAK! *)
let incr_summary ~safe ~vulnerable ~unsafe grouped_accs =
  (* ignore(printf "Checking safety of %a:\n" d_memo (ty,lv)); *)
  let safety =
    grouped_accs
    |> List.filter_map race_conf
    |> (function
        | [] -> None
        | confs -> Some (BatList.max confs)
      )
  in
  match safety with
  | None -> incr safe
  | Some n when n >= 100 -> is_all_safe := false; incr unsafe
  | Some n -> is_all_safe := false; incr vulnerable

let print_accesses memo grouped_accs =
  let allglobs = get_bool "allglobs" in
  let race_threshold = get_int "warn.race-threshold" in
  let msgs race_accs =
    let h A.{conf; kind; node; exp; acc} =
      let doc = dprintf "%a with %a (conf. %d)  (exp: %a)" AccessKind.pretty kind MCPAccess.A.pretty acc conf d_exp exp in
      (doc, Some (Messages.Location.Node node))
    in
    AS.elements race_accs
    |> List.map h
  in
  let group_loc = match memo with
    | (`Var v, _) -> Some (M.Location.CilLocation v.vdecl) (* TODO: offset location *)
    | (`Type _, _) -> None (* TODO: type location *)
  in
  grouped_accs
  |> List.fold_left (fun safe_accs accs ->
      match race_conf accs with
      | None ->
        AS.union safe_accs accs (* group all safe accs together for allglobs *)
      | Some conf ->
        let severity: Messages.Severity.t =
          if conf >= race_threshold then
            Warning
          else
            Info
        in
        M.msg_group severity ?loc:group_loc ~category:Race "Memory location %a (race with conf. %d)" Memo.pretty memo conf (msgs accs);
        safe_accs
    ) (AS.empty ())
  |> (fun safe_accs ->
      if allglobs && not (AS.is_empty safe_accs) then
        M.msg_group Success ?loc:group_loc ~category:Race "Memory location %a (safe)" Memo.pretty memo (msgs safe_accs)
    )

let warn_global ~safe ~vulnerable ~unsafe warn_accs memo =
  let grouped_accs = group_may_race warn_accs in (* do expensive component finding only once *)
  incr_summary ~safe ~vulnerable ~unsafe grouped_accs;
  print_accesses memo grouped_accs
