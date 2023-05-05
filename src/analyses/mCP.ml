(** Master Control Program *)

open Batteries
open GoblintCil
open GobConfig
open Analyses

include MCPRegistry

module MCP2 : Analyses.Spec
  with module D = DomListLattice (LocalDomainListSpec)
   and module G = DomVariantLattice (GlobalDomainListSpec)
   and module C = DomListPrintable (ContextListSpec)
   and module V = DomVariantSysVar (VarListSpec) =
struct
  module D = DomListLattice (LocalDomainListSpec)
  module G = DomVariantLattice (GlobalDomainListSpec)
  module C = DomListPrintable (ContextListSpec)
  module V = DomVariantSysVar (VarListSpec)

  open List open Obj
  let v_of n v = (n, repr v)
  let g_of n g = `Lifted (n, repr g)
  let g_to n = function
    | `Lifted (n', g) ->
      assert (n = n');
      g
    | `Bot ->
      let module S = (val GlobalDomainListSpec.assoc_dom n) in
      repr (S.bot ())
    | `Top ->
      failwith "MCP2.g_to: top"

  let name () = "MCP2"

  let path_sens = ref []
  let cont_inse = ref []
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
        Legacy.Printf.eprintf "Activated analysis '%s' depends on '%s' and '%s' is not activated.\n" xn yn yn;
        raise Exit
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
    cont_inse := map' find_id @@ get_string_list "ana.ctx_insens";
    check_deps !activated;
    activated := topo_sort_an !activated;
    activated_ctx_sens := List.filter (fun (n, _) -> not (List.mem n !cont_inse)) !activated;
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
    let one_el xs (n,(module S:MCPSpec),d) = try f xs (n,(module S:MCPSpec),d) :: xs with Deadcode -> dead:=true; (n,repr @@ S.D.bot ()) :: xs in
    let ys = fold_left one_el [] xs in
    List.rev ys, !dead

  let context fd x =
    let x = spec_list x in
    filter_map (fun (n,(module S:MCPSpec),d) ->
        if mem n !cont_inse then
          None
        else
          Some (n, repr @@ S.context fd (obj d))
      ) x

  let should_join x y =
    (* TODO: GobList.for_all3 *)
    let rec zip3 lst1 lst2 lst3 = match lst1,lst2,lst3 with
      | [],_, _ -> []
      | _,[], _ -> []
      | _,_ , []-> []
      | (x::xs),(y::ys), (z::zs) -> (x,y,z)::(zip3 xs ys zs)
    in
    let should_join ((_,(module S:Analyses.MCPSpec),_),(_,x),(_,y)) = S.should_join (obj x) (obj y) in
    (* obtain all analyses specs that are path sensitive and their values both in x and y *)
    let specs = filter (fun (x,_,_) -> mem x !path_sens) (spec_list x) in
    let xs = filter (fun (x,_) -> mem x !path_sens) x in
    let ys = filter (fun (x,_) -> mem x !path_sens) y in
    let zipped = zip3 specs xs ys in
    List.for_all should_join zipped

  let exitstate  v = map (fun (n,{spec=(module S:MCPSpec); _}) -> n, repr @@ S.exitstate  v) !activated
  let startstate v = map (fun (n,{spec=(module S:MCPSpec); _}) -> n, repr @@ S.startstate v) !activated
  let morphstate v x = map (fun (n,(module S:MCPSpec),d) -> n, repr @@ S.morphstate v (obj d)) (spec_list x)

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

  let do_spawns ctx (xs:(varinfo * (lval option * exp list)) list) =
    let spawn_one v d =
      List.iter (fun (lval, args) -> ctx.spawn lval v args) d
    in
    if not (get_bool "exp.single-threaded") then
      iter (uncurry spawn_one) @@ group_assoc_eq Basetype.Variables.equal xs

  let do_sideg ctx (xs:(V.t * (WideningTokens.TS.t * G.t)) list) =
    let side_one v dts =
      let side_one_ts ts d =
        (* Do side effects with the tokens that were active at the time.
           Transfer functions have exited the with_side_token wrappers by now. *)
        let old_side_tokens = !WideningTokens.side_tokens in
        WideningTokens.side_tokens := ts;
        Fun.protect (fun () ->
            ctx.sideg v @@ fold_left G.join (G.bot ()) d
          ) ~finally:(fun () ->
            WideningTokens.side_tokens := old_side_tokens
          )
      in
      iter (uncurry side_one_ts) @@ group_assoc_eq WideningTokens.TS.equal dts
    in
    iter (uncurry side_one) @@ group_assoc_eq V.equal xs

  let rec do_splits ctx pv (xs:(int * (Obj.t * Events.t list)) list) emits =
    let split_one n (d,emits') =
      let nv = assoc_replace (n,d) pv in
      (* Do split-specific emits before general emits.
         [emits] and [do_emits] are in reverse order.
         [emits'] is in normal order. *)
      ctx.split (do_emits ctx (emits @ List.rev emits') nv false) []
    in
    iter (uncurry split_one) xs

  and do_emits ctx emits xs dead =
    let octx = ctx in
    let ctx_with_local ctx local' =
      (* let rec ctx' =
        { ctx with
          local = local';
          ask = ask
        }
      and ask q = query ctx' q
      in
      ctx' *)
      {ctx with local = local'}
    in
    let do_emit ctx = function
      | Events.SplitBranch (exp, tv) ->
        ctx_with_local ctx (branch ctx exp tv)
      | Events.Assign {lval; exp} ->
        ctx_with_local ctx (assign ctx lval exp)
      | e ->
        let spawns = ref [] in
        let splits = ref [] in
        let sides  = ref [] in (* why do we need to collect these instead of calling ctx.sideg directly? *)
        let emits = ref [] in
        let ctx'' = outer_ctx "do_emits" ~spawns ~sides ~emits ctx in
        let octx'' = outer_ctx "do_emits" ~spawns ~sides ~emits octx in
        let f post_all (n,(module S:MCPSpec),(d,od)) =
          let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "do_emits" ~splits ~post_all ctx'' n d in
          let octx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "do_emits" ~splits ~post_all octx'' n od in
          n, repr @@ S.event ctx' e octx'
        in
        if M.tracing then M.traceli "event" "%a\n  before: %a\n" Events.pretty e D.pretty ctx.local;
        let d, q = map_deadcode f @@ spec_list2 ctx.local octx.local in
        if M.tracing then M.traceu "event" "%a\n  after:%a\n" Events.pretty e D.pretty d;
        do_sideg ctx !sides;
        do_spawns ctx !spawns;
        do_splits ctx d !splits !emits;
        let d = do_emits ctx !emits d q in
        if q then raise Deadcode else ctx_with_local ctx d
    in
    if M.tracing then M.traceli "event" "do_emits:\n";
    let emits =
      if dead then
        List.filter Events.emit_on_deadcode emits
      else
        emits
    in
    (* [emits] is in reverse order. *)
    let ctx' = List.fold_left do_emit (ctx_with_local ctx xs) (List.rev emits) in
    if M.tracing then M.traceu "event" "\n";
    ctx'.local

  and branch (ctx:(D.t, G.t, C.t, V.t) ctx) (e:exp) (tv:bool) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in (* why do we need to collect these instead of calling ctx.sideg directly? *)
    let emits = ref [] in
    let ctx'' = outer_ctx "branch" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "branch" ~splits ~post_all ctx'' n d in
      n, repr @@ S.branch ctx' e tv
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  (* Explicitly polymorphic type required here for recursive GADT call in ask. *)
  and query': type a. querycache:Obj.t Queries.Hashtbl.t -> Queries.Set.t -> (D.t, G.t, C.t, V.t) ctx -> a Queries.t -> a Queries.result = fun ~querycache asked ctx q ->
    let anyq = Queries.Any q in
    if M.tracing then M.traceli "query" "query %a\n" Queries.Any.pretty anyq;
    let r = match Queries.Hashtbl.find_option querycache anyq with
      | Some r ->
        if M.tracing then M.trace "query" "cached\n";
        Obj.obj r
      | None ->
        let module Result = (val Queries.Result.lattice q) in
        if Queries.Set.mem anyq asked then (
          if M.tracing then M.trace "query" "cycle\n";
          Result.top () (* query cycle *)
        )
        else
          let asked' = Queries.Set.add anyq asked in
          let sides = ref [] in
          let ctx'' = outer_ctx "query" ~sides ctx in
          let f ~q a (n,(module S:MCPSpec),d) =
            let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "query" ctx'' n d in
            (* sideg is discouraged in query, because they would bypass sides grouping in other transfer functions.
               See https://github.com/goblint/analyzer/pull/214. *)
            let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
              { ctx' with
                ask    = (fun (type b) (q: b Queries.t) -> query' ~querycache asked' ctx q)
              }
            in
            (* meet results so that precision from all analyses is combined *)
            Result.meet a @@ S.query ctx' q
          in
          match q with
          | Queries.WarnGlobal g ->
            (* WarnGlobal is special: it only goes to corresponding analysis and the argument variant is unlifted for it *)
            let (n, g): V.t = Obj.obj g in
            f ~q:(WarnGlobal (Obj.repr g)) (Result.top ()) (n, spec n, assoc n ctx.local)
          | Queries.InvariantGlobal g ->
            (* InvariantGlobal is special: it only goes to corresponding analysis and the argument variant is unlifted for it *)
            let (n, g): V.t = Obj.obj g in
            f ~q:(InvariantGlobal (Obj.repr g)) (Result.top ()) (n, spec n, assoc n ctx.local)
          | Queries.PartAccess a ->
            Obj.repr (access ctx a)
          | Queries.IterSysVars (vq, fi) ->
            (* IterSysVars is special: argument function is lifted for each analysis *)
            iter (fun ((n,(module S:MCPSpec),d) as t) ->
                let fi' x = fi (Obj.repr (v_of n x)) in
                let q' = Queries.IterSysVars (vq, fi') in
                f ~q:q' () t
              ) @@ spec_list ctx.local
          (* | EvalInt e ->
             (* TODO: only query others that actually respond to EvalInt *)
             (* 2x speed difference on SV-COMP nla-digbench-scaling/ps6-ll_valuebound5.c *)
             f (Result.top ()) (!base_id, spec !base_id, assoc !base_id ctx.local) *)
          | Queries.DYojson ->
            `Lifted (D.to_yojson ctx.local)
          | _ ->
            let r = fold_left (f ~q) (Result.top ()) @@ spec_list ctx.local in
            do_sideg ctx !sides;
            Queries.Hashtbl.replace querycache anyq (Obj.repr r);
            r
    in
    if M.tracing then (
      let module Result = (val Queries.Result.lattice q) in
      M.traceu "query" "-> %a\n" Result.pretty r
    );
    r

  and query: type a. (D.t, G.t, C.t, V.t) ctx -> a Queries.t -> a Queries.result = fun ctx q ->
    let querycache = Queries.Hashtbl.create 13 in
    query' ~querycache Queries.Set.empty ctx q

  and access (ctx:(D.t, G.t, C.t, V.t) ctx) a: MCPAccess.A.t =
    let ctx'' = outer_ctx "access" ctx in
    let f (n, (module S: MCPSpec), d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "access" ctx'' n d in
      (n, repr (S.access ctx' a))
    in
    BatList.map f (spec_list ctx.local) (* map without deadcode *)

  and outer_ctx tfname ?spawns ?sides ?emits ctx =
    let spawn = match spawns with
      | Some spawns -> (fun l v a  -> spawns := (v,(l,a)) :: !spawns)
      | None -> (fun v d    -> failwith ("Cannot \"spawn\" in " ^ tfname ^ " context."))
    in
    let sideg = match sides with
      | Some sides -> (fun v g    -> sides  := (v, (!WideningTokens.side_tokens, g)) :: !sides)
      | None -> (fun v g       -> failwith ("Cannot \"sideg\" in " ^ tfname ^ " context."))
    in
    let emit = match emits with
      | Some emits -> (fun e -> emits := e :: !emits) (* [emits] is in reverse order. *)
      | None -> (fun _ -> failwith ("Cannot \"emit\" in " ^ tfname ^ " context."))
    in
    let querycache = Queries.Hashtbl.create 13 in
    (* TODO: make rec? *)
    { ctx with
      ask    = (fun (type a) (q: a Queries.t) -> query' ~querycache Queries.Set.empty ctx q)
    ; emit
    ; spawn
    ; sideg
    }

  (* Explicitly polymorphic type required here for recursive call in branch. *)
  and inner_ctx: type d g c v. string -> ?splits:(int * (Obj.t * Events.t list)) list ref -> ?post_all:(int * Obj.t) list -> (D.t, G.t, C.t, V.t) ctx -> int -> Obj.t -> (d, g, c, v) ctx = fun tfname ?splits ?(post_all=[]) ctx n d ->
    let split = match splits with
      | Some splits -> (fun d es   -> splits := (n,(repr d,es)) :: !splits)
      | None -> (fun _ _    -> failwith ("Cannot \"split\" in " ^ tfname ^ " context."))
    in
    { ctx with
      local  = obj d
    ; context = (fun () -> ctx.context () |> assoc n |> obj)
    ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
    ; split
    ; sideg  = (fun v g    -> ctx.sideg (v_of n v) (g_of n g))
    }

  and assign (ctx:(D.t, G.t, C.t, V.t) ctx) l e =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "assign" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "assign" ~splits ~post_all ctx'' n d in
      n, repr @@ S.assign ctx' l e
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d


  let vdecl (ctx:(D.t, G.t, C.t, V.t) ctx) v =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "vdecl" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "vdecl" ~splits ~post_all ctx'' n d in
      n, repr @@ S.vdecl ctx' v
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let body (ctx:(D.t, G.t, C.t, V.t) ctx) f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "body" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "body" ~splits ~post_all ctx'' n d in
      n, repr @@ S.body ctx' f
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let return (ctx:(D.t, G.t, C.t, V.t) ctx) e f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "return" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "return" ~splits ~post_all ctx'' n d in
      n, repr @@ S.return ctx' e f
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d


  let asm (ctx:(D.t, G.t, C.t, V.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "asm" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "asm" ~splits ~post_all ctx'' n d in
      n, repr @@ S.asm ctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let skip (ctx:(D.t, G.t, C.t, V.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "skip" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "skip" ~splits ~post_all ctx'' n d in
      n, repr @@ S.skip ctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let special (ctx:(D.t, G.t, C.t, V.t) ctx) r f a =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "special" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "special" ~splits ~post_all ctx'' n d in
      n, repr @@ S.special ctx' r f a
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let sync (ctx:(D.t, G.t, C.t, V.t) ctx) reason =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "sync" ~spawns ~sides ~emits ctx in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "sync" ~splits ~post_all ctx'' n d in
      n, repr @@ S.sync ctx' reason
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits !emits;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let enter (ctx:(D.t, G.t, C.t, V.t) ctx) r f a =
    let spawns = ref [] in
    let sides  = ref [] in
    let ctx'' = outer_ctx "enter" ~spawns ~sides ctx in
    let f (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "enter" ctx'' n d in
      map (fun (c,d) -> ((n, repr c), (n, repr d))) @@ S.enter ctx' r f a
    in
    let css = map f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    map (fun xs -> (topo_sort_an @@ map fst xs, topo_sort_an @@ map snd xs)) @@ n_cartesian_product css

  let combine_env (ctx:(D.t, G.t, C.t, V.t) ctx) r fe f a fc fd f_ask =
    let spawns = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "combine_env" ~spawns ~sides ~emits ctx in
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
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "combine_env" ~post_all ctx'' n d in
      n, repr @@ S.combine_env ctx' r fe f a (Option.map obj fc) (obj fd) f_ask
    in
    let d, q = map_deadcode f @@ List.rev @@ spec_list3_rev_acc [] ctx.local fc fd in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let combine_assign (ctx:(D.t, G.t, C.t, V.t) ctx) r fe f a fc fd f_ask =
    let spawns = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "combine_assign" ~spawns ~sides ~emits ctx in
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
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "combine_assign" ~post_all ctx'' n d in
      n, repr @@ S.combine_assign ctx' r fe f a (Option.map obj fc) (obj fd) f_ask
    in
    let d, q = map_deadcode f @@ List.rev @@ spec_list3_rev_acc [] ctx.local fc fd in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let threadenter (ctx:(D.t, G.t, C.t, V.t) ctx) lval f a =
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "threadenter" ~sides ~emits ctx in
    let f (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "threadenter" ctx'' n d in
      map (fun d -> (n, repr d)) @@ S.threadenter ctx' lval f a
    in
    let css = map f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    (* TODO: this do_emits is now different from everything else *)
    map (fun d -> do_emits ctx !emits d false) @@ map topo_sort_an @@ n_cartesian_product css

  let threadspawn (ctx:(D.t, G.t, C.t, V.t) ctx) lval f a fctx =
    let sides  = ref [] in
    let emits = ref [] in
    let ctx'' = outer_ctx "threadspawn" ~sides ~emits ctx in
    let fctx'' = outer_ctx "threadspawn" ~sides ~emits fctx in
    let f post_all (n,(module S:MCPSpec),(d,fd)) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "threadspawn" ~post_all ctx'' n d in
      let fctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx = inner_ctx "threadspawn" ~post_all fctx'' n fd in
      n, repr @@ S.threadspawn ctx' lval f a fctx'
    in
    let d, q = map_deadcode f @@ spec_list2 ctx.local fctx.local in
    do_sideg ctx !sides;
    let d = do_emits ctx !emits d q in
    if q then raise Deadcode else d

  let event (ctx:(D.t, G.t, C.t, V.t) ctx) e _ = do_emits ctx [e] ctx.local false

  (* Just to satisfy signature *)
  let paths_as_set ctx = [ctx.local]
end
