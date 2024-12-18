(** MCP analysis specification. *)

open Batteries
open GoblintCil
open GobConfig
open Analyses

include MCPRegistry

module MCP2 : Analyses.Spec
  with module D = DomListLattice (LocalDomainListSpec)
   and module G = DomVariantLattice (GlobalDomainListSpec)
   and module C = DomListPrintable (ContextListSpec)
   and module V = DomVariantSysVar (VarListSpec)
   and module P = DomListRepresentative (PathListSpec) =
struct
  module D = DomListLattice (LocalDomainListSpec)
  module G = DomVariantLattice (GlobalDomainListSpec)
  module C = DomListPrintable (ContextListSpec)
  module V = DomVariantSysVar (VarListSpec)
  module P = DomListRepresentative (PathListSpec)

  open List
  let v_of n v = (n, Obj.repr v)
  let g_of n g = `Lifted (n, Obj.repr g)
  let g_to n = function
    | `Lifted (n', g) ->
      assert (n = n');
      g
    | `Bot ->
      let module S = (val GlobalDomainListSpec.assoc_dom n) in
      Obj.repr (S.bot ())
    | `Top ->
      failwith "MCP2.g_to: top"

  let name () = "MCP2"

  let path_sens = ref []
  let act_cont_sens = ref Set.empty
  let base_id   = ref (-1)


  let topo_sort deps circ_msg =
    let rec f res stack = function
      | []                     -> res
      | x::xs when mem x stack -> circ_msg x
      | x::xs when mem x res   -> f res stack xs
      | x::xs                  -> f (x :: f res (x::stack) (deps x)) stack xs
    in rev % f [] []

  let topo_sort_an xs =
    let msg (x,_) = failwith ("Analyses have circular dependencies, conflict for "^find_spec_name x^".") in
    let deps (y,_) = map (fun x -> x, assoc x xs) @@ map find_id @@ (find_spec y).dep in
    topo_sort deps msg xs

  let check_deps xs =
    let check_dep x yn =
      let y = find_id yn in
      if not (exists (fun (y',_) -> y=y') xs) then begin
        let xn = find_spec_name x in
        Logs.error "Activated analysis '%s' depends on '%s' and '%s' is not activated." xn yn yn;
        raise Stdlib.Exit
      end
    in
    let deps (x,_) = iter (check_dep x) @@ (find_spec x).dep in
    iter deps xs


  type marshal = Obj.t list
  let init marshal =
    let map' f =
      let f x =
        try f x
        with Not_found -> raise @@ ConfigError ("Analysis '"^x^"' not found. Abort!")
      in
      List.map f
    in
    let xs = get_string_list "ana.activated" in
    let xs = map' find_id xs in
    base_id := find_id "base";
    activated := map (fun s -> s, find_spec s) xs;
    path_sens := map' find_id @@ get_string_list "ana.path_sens";
    check_deps !activated;
    activated := topo_sort_an !activated;
    begin
      match get_string_list "ana.man_sens" with
      | [] -> (* use values of "ana.man_insens" (blacklist) *)
        let cont_inse = map' find_id @@ get_string_list "ana.man_insens" in
        activated_context_sens := List.filter (fun (n, _) -> not (List.mem n cont_inse)) !activated;
      | sens -> (* use values of "ana.man_sens" (whitelist) *)
        let cont_sens = map' find_id @@ sens in
        activated_context_sens := List.filter (fun (n, _) -> List.mem n cont_sens) !activated;
    end;
    act_cont_sens := Set.of_list (List.map (fun (n,p) -> n) !activated_context_sens);
    activated_path_sens := List.filter (fun (n, _) -> List.mem n !path_sens) !activated;
    match marshal with
    | Some marshal ->
      iter2 (fun (_,{spec=(module S:MCPSpec); _}) marshal -> S.init (Some (Obj.obj marshal))) !activated marshal
    | None ->
      iter (fun (_,{spec=(module S:MCPSpec); _}) -> S.init None) !activated

  let finalize () = map (fun (_,{spec=(module S:MCPSpec); _}) -> Obj.repr (S.finalize ())) !activated

  let spec x = (find_spec x).spec
  let spec_list xs =
    map (fun (n,x) -> (n,spec n,x)) xs
  let spec_list2 xs ys =
    map2 (fun (n,x) (n',y) -> assert (n = n'); (n,spec n,(x,y))) xs ys

  let map_deadcode f xs =
    let dead = ref false in
    let one_el xs (n,(module S:MCPSpec),d) = try f xs (n,(module S:MCPSpec),d) :: xs with Deadcode -> dead:=true; (n,Obj.repr @@ S.D.bot ()) :: xs in
    let ys = fold_left one_el [] xs in
    List.rev ys, !dead

  let exitstate  v = map (fun (n,{spec=(module S:MCPSpec); _}) -> n, Obj.repr @@ S.exitstate  v) !activated
  let startstate v = map (fun (n,{spec=(module S:MCPSpec); _}) -> n, Obj.repr @@ S.startstate v) !activated
  let morphstate v x = map (fun (n,(module S:MCPSpec),d) -> n, Obj.repr @@ S.morphstate v (Obj.obj d)) (spec_list x)

  let startcontext () =
    filter_map (fun (n,{spec=(module S:MCPSpec); _}) ->
        if Set.is_empty !act_cont_sens || not (Set.mem n !act_cont_sens) then (*n is insensitive*)
          None
        else
          Some (n, Obj.repr @@ S.startcontext ())
      ) !activated

  let rec assoc_replace (n,c) = function
    | [] -> failwith "assoc_replace"
    | (n',c')::xs -> if n=n' then (n,c)::xs else (n',c') :: assoc_replace (n,c) xs

  (** [assoc_split_eq (=) 1 [(1,a);(1,b);(2,x)] = ([a,b],[(2,x)])] *)
  let assoc_split_eq eq (k:'a) (xs:('a * 'b) list) : ('b list) * (('a * 'b) list) =
    let rec f a b = function
      | [] -> a, b
      | (k',v)::xs when eq k k' -> f (v::a) b xs
      | x::xs -> f a (x::b) xs
    in
    f [] [] xs

  (** [group_assoc_eq (=) [(1,a);(1,b);(2,x);(2,y)] = [(1,[a,b]),(2,[x,y])]] *)
  let group_assoc_eq eq (xs: ('a * 'b) list) : ('a * ('b list)) list  =
    let rec f a = function
      | [] -> a
      | (k,v)::xs ->
        let a', b = assoc_split_eq eq k xs in
        f ((k,v::a')::a) b
    in f [] xs

  let do_spawns man (xs:(varinfo * (lval option * exp list * bool)) list) =
    let spawn_one v d =
      List.iter (fun (lval, args, multiple) -> man.spawn ~multiple lval v args) d
    in
    if get_bool "exp.single-threaded" then
      M.msg_final Error ~category:Unsound "Thread not spawned"
    else
      iter (uncurry spawn_one) @@ group_assoc_eq Basetype.Variables.equal xs

  let do_sideg man (xs:(V.t * (WideningTokenLifter.TS.t * G.t)) list) =
    let side_one v dts =
      let side_one_ts ts d =
        (* Do side effects with the tokens that were active at the time.
           Transfer functions have exited the with_side_token wrappers by now. *)
        let old_side_tokens = !WideningTokenLifter.side_tokens in
        WideningTokenLifter.side_tokens := ts;
        Fun.protect (fun () ->
            man.sideg v @@ fold_left G.join (G.bot ()) d
          ) ~finally:(fun () ->
            WideningTokenLifter.side_tokens := old_side_tokens
          )
      in
      iter (uncurry side_one_ts) @@ group_assoc_eq WideningTokenLifter.TS.equal dts
    in
    iter (uncurry side_one) @@ group_assoc_eq V.equal xs

  let rec do_splits man pv (xs:(int * (Obj.t * Events.t list)) list) emits =
    let split_one n (d,emits') =
      let nv = assoc_replace (n,d) pv in
      (* Do split-specific emits before general emits.
         [emits] and [do_emits] are in reverse order.
         [emits'] is in normal order. *)
      man.split (do_emits man (emits @ List.rev emits') nv false) []
    in
    iter (uncurry split_one) xs

  and do_emits man emits xs dead =
    let oman = man in
    let man_with_local man local' =
      (* let rec man' =
         { man with
          local = local';
          ask = ask
         }
         and ask q = query man' q
         in
         man' *)
      {man with local = local'}
    in
    let do_emit man = function
      | Events.SplitBranch (exp, tv) ->
        man_with_local man (branch man exp tv)
      | Events.Assign {lval; exp} ->
        man_with_local man (assign man lval exp)
      | e ->
        let spawns = ref [] in
        let splits = ref [] in
        let sides  = ref [] in (* why do we need to collect these instead of calling man.sideg directly? *)
        let emits = ref [] in
        let man'' = outer_man "do_emits" ~spawns ~sides ~emits man in
        let oman'' = outer_man "do_emits" ~spawns ~sides ~emits oman in
        let f post_all (n,(module S:MCPSpec),(d,od)) =
          let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "do_emits" ~splits ~post_all man'' n d in
          let oman' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "do_emits" ~splits ~post_all oman'' n od in
          n, Obj.repr @@ S.event man' e oman'
        in
        if M.tracing then M.traceli "event" "%a\n  before: %a" Events.pretty e D.pretty man.local;
        let d, q = map_deadcode f @@ spec_list2 man.local oman.local in
        if M.tracing then M.traceu "event" "%a\n  after:%a" Events.pretty e D.pretty d;
        do_sideg man !sides;
        do_spawns man !spawns;
        do_splits man d !splits !emits;
        let d = do_emits man !emits d q in
        if q then raise Deadcode else man_with_local man d
    in
    if M.tracing then M.traceli "event" "do_emits:";
    let emits =
      if dead then
        List.filter Events.emit_on_deadcode emits
      else
        emits
    in
    (* [emits] is in reverse order. *)
    let man' = List.fold_left do_emit (man_with_local man xs) (List.rev emits) in
    if M.tracing then M.traceu "event" "";
    man'.local

  and context man fd x =
    let man'' = outer_man "context_computation" man in
    let x = spec_list x in
    filter_map (fun (n,(module S:MCPSpec),d) ->
        if Set.is_empty !act_cont_sens || not (Set.mem n !act_cont_sens) then (*n is insensitive*)
          None
        else
          let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "context_computation" man'' n d in
          Some (n, Obj.repr @@ S.context man' fd (Obj.obj d))
      ) x

  and branch (man:(D.t, G.t, C.t, V.t) man) (e:exp) (tv:bool) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in (* why do we need to collect these instead of calling man.sideg directly? *)
    let emits = ref [] in
    let man'' = outer_man "branch" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "branch" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.branch man' e tv
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  (* Explicitly polymorphic type required here for recursive GADT call in ask. *)
  and query': type a. querycache:Obj.t Queries.Hashtbl.t -> Queries.Set.t -> (D.t, G.t, C.t, V.t) man -> a Queries.t -> a Queries.result = fun ~querycache asked man q ->
    let anyq = Queries.Any q in
    if M.tracing then M.traceli "query" "query %a" Queries.Any.pretty anyq;
    let r = match Queries.Hashtbl.find_option querycache anyq with
      | Some r ->
        if M.tracing then M.trace "query" "cached";
        Obj.obj r
      | None ->
        let module Result = (val Queries.Result.lattice q) in
        if Queries.Set.mem anyq asked then (
          if M.tracing then M.trace "query" "cycle";
          Result.top () (* query cycle *)
        )
        else
          let asked' = Queries.Set.add anyq asked in
          let sides = ref [] in
          let man'' = outer_man "query" ~sides man in
          let f ~q a (n,(module S:MCPSpec),d) =
            let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "query" man'' n d in
            (* sideg is discouraged in query, because they would bypass sides grouping in other transfer functions.
               See https://github.com/goblint/analyzer/pull/214. *)
            let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man =
              { man' with
                ask    = (fun (type b) (q: b Queries.t) -> query' ~querycache asked' man q)
              }
            in
            (* meet results so that precision from all analyses is combined *)
            let res = S.query man' q in
            if M.tracing then M.trace "queryanswers" "analysis %s query %a -> answer %a" (S.name ()) Queries.Any.pretty anyq Result.pretty res;
            Result.meet a @@ res
          in
          match q with
          | Queries.WarnGlobal g ->
            (* WarnGlobal is special: it only goes to corresponding analysis and the argument variant is unlifted for it *)
            let (n, g): V.t = Obj.obj g in
            f ~q:(WarnGlobal (Obj.repr g)) (Result.top ()) (n, spec n, assoc n man.local)
          | Queries.InvariantGlobal g ->
            (* InvariantGlobal is special: it only goes to corresponding analysis and the argument variant is unlifted for it *)
            let (n, g): V.t = Obj.obj g in
            f ~q:(InvariantGlobal (Obj.repr g)) (Result.top ()) (n, spec n, assoc n man.local)
          | Queries.YamlEntryGlobal (g, task) ->
            (* YamlEntryGlobal is special: it only goes to corresponding analysis and the argument variant is unlifted for it *)
            let (n, g): V.t = Obj.obj g in
            f ~q:(YamlEntryGlobal (Obj.repr g, task)) (Result.top ()) (n, spec n, assoc n man.local)
          | Queries.PartAccess a ->
            Obj.repr (access man a)
          | Queries.IterSysVars (vq, fi) ->
            (* IterSysVars is special: argument function is lifted for each analysis *)
            iter (fun ((n,(module S:MCPSpec),d) as t) ->
                let fi' x = fi (Obj.repr (v_of n x)) in
                let q' = Queries.IterSysVars (vq, fi') in
                f ~q:q' () t
              ) @@ spec_list man.local
          (* | EvalInt e ->
             (* TODO: only query others that actually respond to EvalInt *)
             (* 2x speed difference on SV-COMP nla-digbench-scaling/ps6-ll_valuebound5.c *)
             f (Result.top ()) (!base_id, spec !base_id, assoc !base_id man.local) *)
          | Queries.DYojson ->
            `Lifted (D.to_yojson man.local)
          | Queries.GasExhausted f ->
            if (get_int "ana.context.gas_value" >= 0) then
              (* There is a lifter above this that will answer it, save to ask *)
              man.ask (Queries.GasExhausted f)
            else
              (* Abort to avoid infinite recursion *)
              false
          | _ ->
            let r = fold_left (f ~q) (Result.top ()) @@ spec_list man.local in
            do_sideg man !sides;
            Queries.Hashtbl.replace querycache anyq (Obj.repr r);
            r
    in
    if M.tracing then (
      let module Result = (val Queries.Result.lattice q) in
      M.traceu "query" "-> %a" Result.pretty r
    );
    r

  and query: type a. (D.t, G.t, C.t, V.t) man -> a Queries.t -> a Queries.result = fun man q ->
    let querycache = Queries.Hashtbl.create 13 in
    query' ~querycache Queries.Set.empty man q

  and access (man:(D.t, G.t, C.t, V.t) man) a: MCPAccess.A.t =
    let man'' = outer_man "access" man in
    let f (n, (module S: MCPSpec), d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "access" man'' n d in
      (n, Obj.repr (S.access man' a))
    in
    BatList.map f (spec_list man.local) (* map without deadcode *)

  and outer_man tfname ?spawns ?sides ?emits man =
    let spawn = match spawns with
      | Some spawns -> (fun ?(multiple=false) l v a  -> spawns := (v,(l,a,multiple)) :: !spawns)
      | None -> (fun ?(multiple=false) v d    -> failwith ("Cannot \"spawn\" in " ^ tfname ^ " context."))
    in
    let sideg = match sides with
      | Some sides -> (fun v g    -> sides  := (v, (!WideningTokenLifter.side_tokens, g)) :: !sides)
      | None -> (fun v g       -> failwith ("Cannot \"sideg\" in " ^ tfname ^ " context."))
    in
    let emit = match emits with
      | Some emits -> (fun e -> emits := e :: !emits) (* [emits] is in reverse order. *)
      | None -> (fun _ -> failwith ("Cannot \"emit\" in " ^ tfname ^ " context."))
    in
    let querycache = Queries.Hashtbl.create 13 in
    (* TODO: make rec? *)
    { man with
      ask    = (fun (type a) (q: a Queries.t) -> query' ~querycache Queries.Set.empty man q)
    ; emit
    ; spawn
    ; sideg
    }

  (* Explicitly polymorphic type required here for recursive call in branch. *)
  and inner_man: type d g c v. string -> ?splits:(int * (Obj.t * Events.t list)) list ref -> ?post_all:(int * Obj.t) list -> (D.t, G.t, C.t, V.t) man -> int -> Obj.t -> (d, g, c, v) man = fun tfname ?splits ?(post_all=[]) man n d ->
    let split = match splits with
      | Some splits -> (fun d es   -> splits := (n,(Obj.repr d,es)) :: !splits)
      | None -> (fun _ _    -> failwith ("Cannot \"split\" in " ^ tfname ^ " context."))
    in
    { man with
      local  = Obj.obj d
    ; context = (fun () -> man.context () |> assoc n |> Obj.obj)
    ; global = (fun v      -> man.global (v_of n v) |> g_to n |> Obj.obj)
    ; split
    ; sideg  = (fun v g    -> man.sideg (v_of n v) (g_of n g))
    }

  and assign (man:(D.t, G.t, C.t, V.t) man) l e =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "assign" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "assign" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.assign man' l e
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d


  let vdecl (man:(D.t, G.t, C.t, V.t) man) v =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "vdecl" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "vdecl" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.vdecl man' v
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let body (man:(D.t, G.t, C.t, V.t) man) f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "body" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "body" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.body man' f
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let return (man:(D.t, G.t, C.t, V.t) man) e f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "return" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "return" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.return man' e f
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d


  let asm (man:(D.t, G.t, C.t, V.t) man) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "asm" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "asm" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.asm man'
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let skip (man:(D.t, G.t, C.t, V.t) man) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "skip" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "skip" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.skip man'
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let special (man:(D.t, G.t, C.t, V.t) man) r f a =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "special" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "special" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.special man' r f a
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let sync (man:(D.t, G.t, C.t, V.t) man) reason =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "sync" ~spawns ~sides ~emits man in
    let f post_all (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "sync" ~splits ~post_all man'' n d in
      n, Obj.repr @@ S.sync man' reason
    in
    let d, q = map_deadcode f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    do_splits man d !splits !emits;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let enter (man:(D.t, G.t, C.t, V.t) man) r f a =
    let spawns = ref [] in
    let sides  = ref [] in
    let man'' = outer_man "enter" ~spawns ~sides man in
    let f (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "enter" man'' n d in
      map (fun (c,d) -> ((n, Obj.repr c), (n, Obj.repr d))) @@ S.enter man' r f a
    in
    let css = map f @@ spec_list man.local in
    do_sideg man !sides;
    do_spawns man !spawns;
    map (fun xs -> (topo_sort_an @@ map fst xs, topo_sort_an @@ map snd xs)) @@ n_cartesian_product css

  let combine_env (man:(D.t, G.t, C.t, V.t) man) r fe f a fc fd f_ask =
    let spawns = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "combine_env" ~spawns ~sides ~emits man in
    (* Like spec_list2 but for three lists. Tail recursion like map3_rev would have.
       Due to context-insensitivity, second list is optional and may only contain a subset of analyses
       in the same order, so some skipping needs to happen to align the three lists.
       See https://github.com/goblint/analyzer/pull/578/files#r794376508. *)
    let rec spec_list3_rev_acc acc l1 l2_opt l3 = match l1, l2_opt, l3 with
      | [], _, [] -> acc
      | (n, x) :: l1, Some ((n', y) :: l2), (n'', z) :: l3 when n = n' -> (* context-sensitive *)
        assert (n = n'');
        spec_list3_rev_acc ((n, spec n, (x, Some y, z)) :: acc) l1 (Some l2) l3
      | (n, x) :: l1, l2_opt, (n'', z) :: l3 -> (* context-insensitive *)
        assert (n = n'');
        spec_list3_rev_acc ((n, spec n, (x, None, z)) :: acc) l1 l2_opt l3
      | _, _, _ -> invalid_arg "MCP.spec_list3_rev_acc"
    in
    let f post_all (n,(module S:MCPSpec),(d,fc,fd)) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "combine_env" ~post_all man'' n d in
      n, Obj.repr @@ S.combine_env man' r fe f a (Option.map Obj.obj fc) (Obj.obj fd) f_ask
    in
    let d, q = map_deadcode f @@ List.rev @@ spec_list3_rev_acc [] man.local fc fd in
    do_sideg man !sides;
    do_spawns man !spawns;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let combine_assign (man:(D.t, G.t, C.t, V.t) man) r fe f a fc fd f_ask =
    let spawns = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "combine_assign" ~spawns ~sides ~emits man in
    (* Like spec_list2 but for three lists. Tail recursion like map3_rev would have.
       Due to context-insensitivity, second list is optional and may only contain a subset of analyses
       in the same order, so some skipping needs to happen to align the three lists.
       See https://github.com/goblint/analyzer/pull/578/files#r794376508. *)
    let rec spec_list3_rev_acc acc l1 l2_opt l3 = match l1, l2_opt, l3 with
      | [], _, [] -> acc
      | (n, x) :: l1, Some ((n', y) :: l2), (n'', z) :: l3 when n = n' -> (* context-sensitive *)
        assert (n = n'');
        spec_list3_rev_acc ((n, spec n, (x, Some y, z)) :: acc) l1 (Some l2) l3
      | (n, x) :: l1, l2_opt, (n'', z) :: l3 -> (* context-insensitive *)
        assert (n = n'');
        spec_list3_rev_acc ((n, spec n, (x, None, z)) :: acc) l1 l2_opt l3
      | _, _, _ -> invalid_arg "MCP.spec_list3_rev_acc"
    in
    let f post_all (n,(module S:MCPSpec),(d,fc,fd)) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "combine_assign" ~post_all man'' n d in
      n, Obj.repr @@ S.combine_assign man' r fe f a (Option.map Obj.obj fc) (Obj.obj fd) f_ask
    in
    let d, q = map_deadcode f @@ List.rev @@ spec_list3_rev_acc [] man.local fc fd in
    do_sideg man !sides;
    do_spawns man !spawns;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let threadenter (man:(D.t, G.t, C.t, V.t) man) ~multiple lval f a =
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "threadenter" ~sides ~emits man in
    let f (n,(module S:MCPSpec),d) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "threadenter" man'' n d in
      map (fun d -> (n, Obj.repr d)) @@ (S.threadenter ~multiple) man' lval f a
    in
    let css = map f @@ spec_list man.local in
    do_sideg man !sides;
    (* TODO: this do_emits is now different from everything else *)
    map (fun d -> do_emits man !emits d false) @@ map topo_sort_an @@ n_cartesian_product css

  let threadspawn (man:(D.t, G.t, C.t, V.t) man) ~multiple lval f a fman =
    let sides  = ref [] in
    let emits = ref [] in
    let man'' = outer_man "threadspawn" ~sides ~emits man in
    let fman'' = outer_man "threadspawn" ~sides ~emits fman in
    let f post_all (n,(module S:MCPSpec),(d,fd)) =
      let man' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "threadspawn" ~post_all man'' n d in
      let fman' : (S.D.t, S.G.t, S.C.t, S.V.t) man = inner_man "threadspawn" ~post_all fman'' n fd in
      n, Obj.repr @@ S.threadspawn ~multiple man' lval f a fman'
    in
    let d, q = map_deadcode f @@ spec_list2 man.local fman.local in
    do_sideg man !sides;
    let d = do_emits man !emits d q in
    if q then raise Deadcode else d

  let event (man:(D.t, G.t, C.t, V.t) man) e _ = do_emits man [e] man.local false

  (* Just to satisfy signature *)
  let paths_as_set man = [man.local]
end
