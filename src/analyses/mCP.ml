(** Master Control Program *)

open Prelude.Ana
open GobConfig
open Analyses

module QuerySet = Set.Make (Queries.Any)

include MCPRegistry

module MCP2 : Analyses.Spec
  with module D = DomListLattice (LocalDomainListSpec)
   and module G = DomVariantLattice (GlobalDomainListSpec)
   and module C = DomListPrintable (ContextListSpec)
   and module V = DomVariantPrintable (VarListSpec) =
struct
  module D = DomListLattice (LocalDomainListSpec)
  module G = DomVariantLattice (GlobalDomainListSpec)
  module C = DomListPrintable (ContextListSpec)
  module V = DomVariantPrintable (VarListSpec)

  open List open Obj
  let v_of n v = (n, repr v)
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
    let msg (x,_) = failwith ("Analyses have circular dependencies, conflict for "^assoc x !analyses_table^".") in
    let deps (y,_) = map (fun x -> x, assoc x xs) @@ assoc y !dep_list in
    topo_sort deps msg xs

  let check_deps xs =
    let check_dep x y =
      if not (exists (fun (y',_) -> y=y') xs) then begin
        let xn = assoc x !analyses_table in
        let yn = assoc y !analyses_table in
        Legacy.Printf.eprintf "Activated analysis '%s' depends on '%s' and '%s' is not activated.\n" xn yn yn;
        raise Exit
      end
    in
    let deps (x,_) = iter (check_dep x) @@ assoc x !dep_list in
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
    let xs = map' (flip assoc_inv !analyses_table) xs in
    base_id := assoc_inv "base" !analyses_table;
    analyses_list := map (fun s -> s, assoc s !analyses_list') xs;
    path_sens := map' (flip assoc_inv !analyses_table) @@ get_string_list "ana.path_sens";
    cont_inse := map' (flip assoc_inv !analyses_table) @@ get_string_list "ana.ctx_insens";
    dep_list  := map (fun (n,d) -> (n,map' (flip assoc_inv !analyses_table) d)) !dep_list';
    check_deps !analyses_list;
    analyses_list := topo_sort_an !analyses_list;
    (*iter (fun (x,y) -> Printf.printf "%s -> %a\n"  (flip assoc !analyses_table x) (List.print (fun f -> String.print f % flip assoc !analyses_table)) y) !dep_list_trans;
      Printf.printf "\n";
      iter (Printf.printf "%s\n" % flip assoc !analyses_table % fst) !analyses_list;
      Printf.printf "\n";*)
    match marshal with
    | Some marshal ->
      combine !analyses_list marshal
      |> iter (fun ((_,{spec=(module S:MCPSpec); _}), marshal) -> S.init (Some (Obj.obj marshal)))
    | None ->
      iter (fun (_,{spec=(module S:MCPSpec); _}) -> S.init None) !analyses_list

  let finalize () = map (fun (_,{spec=(module S:MCPSpec); _}) -> Obj.repr (S.finalize ())) !analyses_list

  let spec x = (assoc x !analyses_list).spec
  let spec_list xs =
    map (fun (n,x) -> (n,spec n,x)) xs
  let spec_name (n:int) : string = assoc n !analyses_table

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

  let exitstate  v = map (fun (n,{spec=(module S:MCPSpec); _}) -> n, repr @@ S.exitstate  v) !analyses_list
  let startstate v = map (fun (n,{spec=(module S:MCPSpec); _}) -> n, repr @@ S.startstate v) !analyses_list
  let morphstate v x = map (fun (n,(module S:MCPSpec),d) -> n, repr @@ S.morphstate v (obj d)) (spec_list x)

  let call_descr f xs =
    let xs = filter (fun (x,_) -> x = !base_id) xs in
    fold_left (fun a (n,(module S:MCPSpec),d) -> S.call_descr f (obj d)) f.svar.vname @@ spec_list xs


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

  let assoc_sub n xs name =
    let n' = List.assoc_inv name !analyses_table in
    assoc n' xs

  let do_spawns ctx (xs:(varinfo * (int * lval option * exp list)) list) =
    let spawn_one v d =
      List.iter (fun (n, lval, args) -> ctx.spawn lval v args) d
    in
    if not (get_bool "exp.single-threaded") then
      iter (uncurry spawn_one) @@ group_assoc_eq Basetype.Variables.equal xs

  let do_sideg ctx (xs:(V.t * (int * Obj.t)) list) =
    let side_one v d =
      ctx.sideg v @@ fold_left (fun x y -> G.join x (`Lifted y)) (G.bot ()) d
    in
    iter (uncurry side_one) @@ group_assoc_eq V.equal xs

  let do_assigns ctx assigns (xs:(int * Obj.t) list) =
    if List.is_empty assigns then xs (* nothing to do *)
    else
      let spec_assign n d : Obj.t =
        (* spec of current analysis *)
        let (module S:MCPSpec) = spec n in
        let assign_one d (lval, exp, name, ctx) =
          match name with
          | Some x when x <> spec_name n -> obj d (* do nothing if current spec name is filtered out *)
          | _ ->
            let ctx' = {(obj ctx) with local = obj d} in
            S.assign ctx' lval exp
        in
        let get_lval (lval, exp, name, ctx) = lval in
        (* group by assigns on the same lval -> only those must be joined *)
        List.group (compareBy get_lval) assigns
        |> List.fold_left (fun d xs -> List.map (assign_one d) xs |> List.reduce S.D.join |> repr) d
      in
      List.map (fun (n,d) -> n, spec_assign n d) xs

  let rec do_splits ctx pv (xs:(int * (Obj.t * Events.t list)) list) =
    let split_one n (d,emits) =
      let nv = assoc_replace (n,d) pv in
      ctx.split (do_emits ctx emits nv) []
    in
    iter (uncurry split_one) xs

  and do_emits ctx emits xs =
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
      | e ->
        let spawns = ref [] in
        let splits = ref [] in
        let sides  = ref [] in (* why do we need to collect these instead of calling ctx.sideg directly? *)
        let assigns = ref [] in
        let emits = ref [] in
        let f post_all (n,(module S:MCPSpec),d) =
          let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
            { local  = obj d
            ; node   = ctx.node
            ; prev_node = ctx.prev_node
            ; control_context = ctx.control_context
            ; context = (fun () -> ctx.context () |> assoc n |> obj)
            ; edge   = ctx.edge
            ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
            ; emit   = (fun e -> emits := e :: !emits)
            ; presub = assoc_sub n ctx.local
            ; postsub= assoc_sub n post_all
            ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
            ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
            ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
            ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
            ; assign = (fun ?name v e    -> assigns := (v,e,name, repr ctx')::!assigns)
            }
          in
          let rec octx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
            { local  = obj (assoc n octx.local)
            ; node   = octx.node
            ; prev_node = octx.prev_node
            ; control_context = octx.control_context
            ; context = (fun () -> octx.context () |> assoc n |> obj)
            ; edge   = octx.edge
            ; ask    = (fun (type a) (q: a Queries.t) -> query octx q)
            ; emit   = (fun e -> emits := e :: !emits)
            ; presub = assoc_sub n octx.local
            ; postsub= assoc_sub n post_all
            ; global = (fun v      -> octx.global (v_of n v) |> g_to n |> obj)
            ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
            ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
            ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
            ; assign = (fun ?name v e    -> assigns := (v,e,name, repr octx')::!assigns)
            }
          in
          n, repr @@ S.event ctx' e octx'
        in
        let d, q = map_deadcode f @@ spec_list ctx.local in
        if M.tracing then M.tracel "event" "%a\n  before: %a\n  after:%a\n" Events.pretty e D.pretty ctx.local D.pretty d;
        do_sideg ctx !sides;
        do_spawns ctx !spawns;
        do_splits ctx d !splits;
        let d = do_assigns ctx !assigns d in
        let d = do_emits ctx !emits d in
        if q then raise Deadcode else ctx_with_local ctx d
    in
    let ctx' = List.fold_left do_emit (ctx_with_local ctx xs) emits in
    ctx'.local

  and branch (ctx:(D.t, G.t, C.t, V.t) ctx) (e:exp) (tv:bool) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in (* why do we need to collect these instead of calling ctx.sideg directly? *)
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e    -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.branch ctx' e tv
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  (* Explicitly polymorphic type required here for recursive GADT call in ask. *)
  and query': type a. QuerySet.t -> (D.t, G.t, C.t, V.t) ctx -> a Queries.t -> a Queries.result = fun asked ctx q ->
    let module Result = (val Queries.Result.lattice q) in
    if QuerySet.mem (Any q) asked then
      Result.top () (* query cycle *)
    else
      let asked' = QuerySet.add (Any q) asked in
      let sides = ref [] in
      let f ~q a (n,(module S:MCPSpec),d) =
        let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
          { local  = obj d
          ; node   = ctx.node
          ; prev_node = ctx.prev_node
          ; control_context = ctx.control_context
          ; context = (fun () -> ctx.context () |> assoc n |> obj)
          ; edge   = ctx.edge
          ; ask    = (fun (type b) (q: b Queries.t) -> query' asked' ctx q)
          ; emit   = (fun _ -> failwith "Cannot \"emit\" in query context.")
          ; presub = assoc_sub n ctx.local
          ; postsub= assoc_sub n []
          ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
          ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in query context.")
          ; split  = (fun d es   -> failwith "Cannot \"split\" in query context.")
          ; sideg  = (fun v g    -> sides := (v_of n v, (n, repr g)) :: !sides)
          (* sideg is discouraged in query, because they would bypass sides grouping in other transfer functions.
             See https://github.com/goblint/analyzer/pull/214. *)
          ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in query context.")
          }
        in
        (* meet results so that precision from all analyses is combined *)
        Result.meet a @@ S.query ctx' q
      in
      match q with
      | Queries.PrintFullState ->
        ignore (Pretty.printf "Current State:\n%a\n\n" D.pretty ctx.local);
        ()
      | Queries.WarnGlobal g ->
        (* WarnGlobal is special: it only goes to corresponding analysis and the argument variant is unlifted for it *)
        let (n, g): V.t = Obj.obj g in
        f ~q:(WarnGlobal (Obj.repr g)) (Result.top ()) (n, spec n, assoc n ctx.local)
      | Queries.PartAccess {exp; var_opt; write} ->
        Obj.repr (access ctx exp var_opt write)
      (* | EvalInt e ->
        (* TODO: only query others that actually respond to EvalInt *)
        (* 2x speed difference on SV-COMP nla-digbench-scaling/ps6-ll_valuebound5.c *)
        f (Result.top ()) (!base_id, spec !base_id, assoc !base_id ctx.local) *)
      | _ ->
        let r = fold_left (f ~q) (Result.top ()) @@ spec_list ctx.local in
        do_sideg ctx !sides;
        r

  and query: type a. (D.t, G.t, C.t, V.t) ctx -> a Queries.t -> a Queries.result = fun ctx q ->
    query' QuerySet.empty ctx q

  and access (ctx:(D.t, G.t, C.t, V.t) ctx) e vo w: MCPAccess.A.t =
    let f (acc: MCPAccess.A.t) (n, (module S: MCPSpec), d) : MCPAccess.A.t =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in access context.")
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n []
        ; global = (fun v         -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun v d       -> failwith "part_access::spawn")
        ; split  = (fun d es      -> failwith "part_access::split")
        ; sideg  = (fun v g       -> failwith "Cannot \"sideg\" in access context.")
        ; assign = (fun ?name v e -> failwith "part_access::assign")
        }
      in
      (n, repr (S.access ctx' e vo w)) :: acc
    in
    List.fold_left f [] (spec_list ctx.local) (* map without deadcode *)

  let assign (ctx:(D.t, G.t, C.t, V.t) ctx) l e =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in assign context (cycles?).")
        }
      in
      n, repr @@ S.assign ctx' l e
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d


  let vdecl (ctx:(D.t, G.t, C.t, V.t) ctx) v =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in assign context (cycles?).")
        }
      in
      n, repr @@ S.vdecl ctx' v
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let body (ctx:(D.t, G.t, C.t, V.t) ctx) f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.body ctx' f
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let return (ctx:(D.t, G.t, C.t, V.t) ctx) e f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.return ctx' e f
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let intrpt (ctx:(D.t, G.t, C.t, V.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.intrpt ctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let asm (ctx:(D.t, G.t, C.t, V.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.asm ctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let skip (ctx:(D.t, G.t, C.t, V.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.skip ctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let special (ctx:(D.t, G.t, C.t, V.t) ctx) r f a =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.special ctx' r f a
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let sync (ctx:(D.t, G.t, C.t, V.t) ctx) reason =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> splits := (n,(repr d,es)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in sync context.")
        }
      in
      n, repr @@ S.sync ctx' reason
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let enter (ctx:(D.t, G.t, C.t, V.t) ctx) r f a =
    let spawns = ref [] in
    let sides  = ref [] in
    let f (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter context.")
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n []
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun _ _    -> failwith "Cannot \"split\" in enter context." )
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in enter context.")
        }
      in
      map (fun (c,d) -> ((n, repr c), (n, repr d))) @@ S.enter ctx' r f a
    in
    let css = map f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    map (fun xs -> (topo_sort_an @@ map fst xs, topo_sort_an @@ map snd xs)) @@ n_cartesian_product css

  let combine (ctx:(D.t, G.t, C.t, V.t) ctx) r fe f a fc fd =
    let spawns = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun l v a  -> spawns := (v,(n,l,a)) :: !spawns)
        ; split  = (fun d es   -> failwith "Cannot \"split\" in combine context.")
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.combine ctx' r fe f a (Option.map obj (Option.bind fc (assoc_opt n))) (obj (assoc n fd))
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    let d = do_assigns ctx !assigns d in
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d

  let threadenter (ctx:(D.t, G.t, C.t, V.t) ctx) lval f a =
    let sides  = ref [] in
    let emits = ref [] in
    let f (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n []
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in threadenter context.")
        ; split  = (fun d es   -> failwith "Cannot \"split\" in threadenter context.")
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> failwith "Cannot \"assign\" in threadenter context.")
        }
      in
      map (fun d -> (n, repr d)) @@ S.threadenter ctx' lval f a
    in
    let css = map f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    (* TODO: this do_emits is now different from everything else *)
    map (do_emits ctx !emits) @@ map topo_sort_an @@ n_cartesian_product css

  let threadspawn (ctx:(D.t, G.t, C.t, V.t) ctx) lval f a fctx =
    let sides  = ref [] in
    let emits = ref [] in
    let f post_all (n,(module S:MCPSpec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query ctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n ctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> ctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in threadspawn context.")
        ; split  = (fun d es   -> failwith "Cannot \"split\" in threadspawn context.")
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> failwith "Cannot \"assign\" in threadspawn context.")
        }
      in
      let fctx' : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
        { local  = obj (assoc n fctx.local)
        ; node   = fctx.node
        ; prev_node = fctx.prev_node
        ; control_context = fctx.control_context
        ; context = (fun () -> fctx.context () |> assoc n |> obj)
        ; edge   = fctx.edge
        ; ask    = (fun (type a) (q: a Queries.t) -> query fctx q)
        ; emit   = (fun e -> emits := e :: !emits)
        ; presub = assoc_sub n fctx.local
        ; postsub= assoc_sub n post_all
        ; global = (fun v      -> fctx.global (v_of n v) |> g_to n |> obj)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in threadspawn context.")
        ; split  = (fun d es   -> failwith "Cannot \"split\" in threadspawn context.")
        ; sideg  = (fun v g    -> sides  := (v_of n v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> failwith "Cannot \"assign\" in threadspawn context.")
        }
      in
      n, repr @@ S.threadspawn ctx' lval f a fctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    let d = do_emits ctx !emits d in
    if q then raise Deadcode else d
end
