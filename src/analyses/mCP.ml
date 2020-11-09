(** Master Control Program *)

open Prelude.Ana
open GobConfig
open Analyses

type spec_modules = { spec : (module Spec)
                    ; dom  : (module Lattice.S)
                    ; glob : (module Lattice.S)
                    ; cont : (module Printable.S) }

let analyses_list  : (int * spec_modules) list ref = ref []
let analyses_list' : (int * spec_modules) list ref = ref []
let dep_list       : (int * (int list)) list ref   = ref []
let dep_list'      : (int * (string list)) list ref= ref []

let analyses_table = ref []

let register_analysis =
  let count = ref 0 in
  fun ?(dep=[]) (module S:Spec) ->
    let s = { spec = (module S : Spec)
            ; dom  = (module S.D : Lattice.S)
            ; glob = (module S.G : Lattice.S)
            ; cont = (module S.C : Printable.S)
            }
    in
    let n = S.name () in
    analyses_table := (!count,n) :: !analyses_table;
    analyses_list' := (!count,s) :: !analyses_list';
    dep_list'      := (!count,dep) :: !dep_list';
    count := !count + 1


type unknown = Obj.t

module type DomainListPrintableSpec =
sig
  val assoc_dom : int -> (module Printable.S)
  val domain_list : unit -> (int * (module Printable.S)) list
end

module type DomainListLatticeSpec =
sig
  val assoc_dom : int -> (module Lattice.S)
  val domain_list : unit -> (int * (module Lattice.S)) list
end

module PrintableOfLatticeSpec (D:DomainListLatticeSpec) : DomainListPrintableSpec =
struct
  let assoc_dom n =
    let f (module L:Lattice.S) = (module L : Printable.S)
    in
    f (D.assoc_dom n)

  let domain_list () =
    let f (module L:Lattice.S) = (module L : Printable.S) in
    List.map (fun (x,y) -> (x,f y)) (D.domain_list ())
end

exception DomListBroken of string

module DomListPrintable (DLSpec : DomainListPrintableSpec)
  (*  : Printable.S with type t = (string * unknown) list *)
=
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open DLSpec
  open List
  open Obj

  type t = (int * unknown) list

  let unop_fold f a (x:t) =
    let f a n d =
      f a n (assoc_dom n) d
    in
    fold_left (fun a (n,d) -> f a n d) a x

  let pretty_f _ () x =
    let f a n (module S : Printable.S) x = Pretty.dprintf "%s:%a" (S.name ()) S.pretty (obj x) :: a in
    let xs = unop_fold f [] x in
    match xs with
    | [] -> text "[]"
    | x :: [] -> x
    | x :: y ->
      let rest  = List.fold_left (fun p n->p ++ text "," ++ break ++ n) nil y in
      text "[" ++ align ++ x ++ rest ++ unalign ++ text "]"

  let short w x =
    let w2 = let n = List.length x in if n=0 then w else w / n in
    (* width violated anyway? *)
    let xs = unop_fold (fun a n (module S : Printable.S) x ->
        let analysis_name = assoc n !analyses_table in
        (analysis_name ^ ":(" ^ S.short w2 (obj x) ^ ")") :: a) [] x
    in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (rev xs)

  let to_yojson x =
    let xs = unop_fold (fun a n (module S : Printable.S) x -> S.to_yojson (obj x) :: a) [] x in
    [%to_yojson: Printable.json list] xs

  let pretty = pretty_f short

  let binop_fold f a (x:t) (y:t) =
    let f a n d1 d2 =
      f a n (assoc_dom n) d1 d2
    in
    try if length x <> length y
      then raise (DomListBroken "binop_fold : differing lengths")
      else fold_left (fun a (n,d) -> f a n d @@ assoc n y) a x
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure")

  let binop_map_rev (f: (module Printable.S) -> Obj.t -> Obj.t -> Obj.t) =
    binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) []

  let equal   x y = binop_fold (fun a n (module S : Printable.S) x y -> a && S.equal (obj x) (obj y)) true x y
  let compare x y = binop_fold (fun a n (module S : Printable.S) x y -> if a <> 0 then a else S.compare (obj x) (obj y)) 0 x y

  let hashmul x y = if x=0 then y else if y=0 then x else x*y

  let hash     = unop_fold (fun a n (module S : Printable.S) x -> hashmul a @@ S.hash (obj x)) 0
  let isSimple = unop_fold (fun a n (module S : Printable.S) x -> a && S.isSimple (obj x)) true

  let name () =
    let domain_name (n, (module D: Printable.S)) =
      let analysis_name = assoc n !analyses_table in
      analysis_name ^ ":(" ^ D.name () ^ ")"
    in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (map domain_name @@ domain_list ())

  let pretty_diff () (x,y) = text "Please override me!"

  let printXml f xs =
    let print_one a n (module S : Printable.S) x : unit =
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (List.assoc n !analyses_table);
      S.printXml f (obj x);
      BatPrintf.fprintf f "</analysis>\n"
    in
    unop_fold print_one () xs

  let invariant c = unop_fold (fun a n (module S : Printable.S) x ->
      Invariant.(a && S.invariant c (obj x))
    ) Invariant.none

  let arbitrary () =
    let arbs = map (fun (n, (module D: Printable.S)) -> QCheck.map ~rev:(fun (_, o) -> obj o) (fun x -> (n, repr x)) @@ D.arbitrary ()) @@ domain_list () in
    MyCheck.Arbitrary.sequence arbs
end

let _ =
  let module Test : functor (DLSpec : DomainListPrintableSpec) -> Printable.S with type t = (int * unknown) list = DomListPrintable  in
  ()

module DomListLattice (DLSpec : DomainListLatticeSpec)
  : Lattice.S with type t = (int * unknown) list
=
struct
  open DLSpec
  open List
  open Obj

  include DomListPrintable (PrintableOfLatticeSpec (DLSpec))

  let binop_fold f a (x:t) (y:t) =
    let f a n d1 d2 =
      f a n (assoc_dom n) d1 d2
    in
    try if length x <> length y
      then raise (DomListBroken "binop_fold : differing lengths")
      else fold_left (fun a (n,d) -> f a n d @@ assoc n y) a x
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure")

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) x y =
    List.rev @@ binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) [] x y

  let unop_fold f a (x:t) =
    let f a n d =
      f a n (assoc_dom n) d
    in
    fold_left (fun a (n,d) -> f a n d) a x

  let narrow = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.narrow (obj x) (obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.widen  (obj x) (obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.meet   (obj x) (obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.join   (obj x) (obj y))

  let leq    = binop_fold (fun a n (module S : Lattice.S) x y -> a && S.leq (obj x) (obj y)) true

  let is_top = unop_fold (fun a n (module S : Lattice.S) x -> a && S.is_top (obj x)) true
  let is_bot = unop_fold (fun a n (module S : Lattice.S) x -> a && S.is_bot (obj x)) true

  let top () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.top ())) @@ domain_list ()
  let bot () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.bot ())) @@ domain_list ()

  let pretty_diff () (x,y) =
    let f a n (module S : Lattice.S) x y =
      if S.leq (obj x) (obj y) then a
      else a ++ S.pretty_diff () (obj x, obj y) ++ text ". "
    in
    binop_fold f nil x y
end

module LocalDomainListSpec : DomainListLatticeSpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).dom
  let domain_list () = List.map (fun (n,p) -> n, p.dom) !analyses_list
end

module GlobalDomainListSpec : DomainListLatticeSpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).glob
  let domain_list () = List.map (fun (n,p) -> n, p.glob) !analyses_list
end

module ContextListSpec : DomainListPrintableSpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).cont
  let domain_list () = List.map (fun (n,p) -> n, p.cont) !analyses_list
end

module MCP2 : Analyses.Spec
  with module D = DomListLattice (LocalDomainListSpec)
   and module G = DomListLattice (GlobalDomainListSpec)
   and module C = DomListPrintable (ContextListSpec) =
struct
  module D = DomListLattice (LocalDomainListSpec)
  module G = DomListLattice (GlobalDomainListSpec)
  module C = DomListPrintable (ContextListSpec)

  open List open Obj

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
        Legacy.Printf.fprintf !Messages.warn_out "Activated analysis '%s' depends on '%s' and '%s' is not activated.\n" xn yn yn;
        raise Exit
      end
    in
    let deps (x,_) = iter (check_dep x) @@ assoc x !dep_list in
    iter deps xs

  let init () =
    let map' f =
      let f x =
        try Some (f x)
        with Not_found -> raise @@ ConfigError ("Analysis '"^x^"' not found. Abort!")
      in
      List.filter_map f
    in
    let xs = map Json.string @@ get_list "ana.activated" in
    let xs = map' (flip assoc_inv !analyses_table) xs in
    base_id := assoc_inv "base" !analyses_table;
    analyses_list := map (fun s -> s, assoc s !analyses_list') xs;
    path_sens := map' (flip assoc_inv !analyses_table) @@ map Json.string @@ get_list "ana.path_sens";
    cont_inse := map' (flip assoc_inv !analyses_table) @@ map Json.string @@ get_list "ana.ctx_insens";
    dep_list  := map (fun (n,d) -> (n,map' (flip assoc_inv !analyses_table) d)) !dep_list';
    check_deps !analyses_list;
    analyses_list := topo_sort_an !analyses_list;
    (*iter (fun (x,y) -> Printf.printf "%s -> %a\n"  (flip assoc !analyses_table x) (List.print (fun f -> String.print f % flip assoc !analyses_table)) y) !dep_list_trans;
      Printf.printf "\n";
      iter (Printf.printf "%s\n" % flip assoc !analyses_table % fst) !analyses_list;
      Printf.printf "\n";*)
    iter (fun (_,{spec=(module S:Spec); _}) -> S.init ()) !analyses_list

  let finalize () = iter (fun (_,{spec=(module S:Spec); _}) -> S.finalize ()) !analyses_list

  let spec x = (assoc x !analyses_list).spec
  let spec_list xs =
    map (fun (n,x) -> (n,spec n,x)) xs
  let spec_name (n:int) : string = assoc n !analyses_table

  let map_deadcode f xs =
    let dead = ref false in
    let one_el xs (n,(module S:Spec),d) = try f xs (n,(module S:Spec),d) :: xs with Deadcode -> dead:=true; (n,repr @@ S.D.bot ()) :: xs in
    let ys = fold_left one_el [] xs in
    List.rev ys, !dead

  let val_of = identity
  let context x =
    let x = spec_list x in
    map (fun (n,(module S:Spec),d) ->
        let d' = if mem n !cont_inse then S.D.top () else obj d in
        n, repr @@ S.context d'
      ) x

  let should_join x y =
    let rec zip3 lst1 lst2 lst3 = match lst1,lst2,lst3 with
      | [],_, _ -> []
      | _,[], _ -> []
      | _,_ , []-> []
      | (x::xs),(y::ys), (z::zs) -> (x,y,z)::(zip3 xs ys zs)
    in
    let should_join ((_,(module S:Analyses.Spec),_),(_,x),(_,y)) = S.should_join (obj x) (obj y) in
    (* obtain all analyses specs that are path sensitive and their values both in x and y *)
    let specs = filter (fun (x,_,_) -> mem x !path_sens) (spec_list x) in
    let xs = filter (fun (x,_) -> mem x !path_sens) x in
    let ys = filter (fun (x,_) -> mem x !path_sens) y in
    let zipped = zip3 specs xs ys in
    List.for_all should_join zipped

  let exitstate  v = map (fun (n,{spec=(module S:Spec); _}) -> n, repr @@ S.exitstate  v) !analyses_list
  let startstate v = map (fun (n,{spec=(module S:Spec); _}) -> n, repr @@ S.startstate v) !analyses_list
  let morphstate v x = map (fun (n,(module S:Spec),d) -> n, repr @@ S.morphstate v (obj d)) (spec_list x)

  let call_descr f xs =
    let xs = filter (fun (x,_) -> x = !base_id) xs in
    fold_left (fun a (n,(module S:Spec),d) -> S.call_descr f (obj d)) f.svar.vname @@ spec_list xs


  let rec assoc_replace (n,c) = function
    | [] -> failwith "assoc_replace"
    | (n',c')::xs -> if n=n' then (n,c)::xs else (n',c') :: assoc_replace (n,c) xs

  (** [assoc_split_eq (=) 1 [(1,a);(1,b);(2,x)] = ([a,b],[(2,x)])] *)
  let assoc_split_eq (=) (k:'a) (xs:('a * 'b) list) : ('b list) * (('a * 'b) list) =
    let rec f a b = function
      | [] -> a, b
      | (k',v)::xs when k=k' -> f (v::a) b xs
      | x::xs -> f a (x::b) xs
    in
    f [] [] xs

  let assoc_split k xs = assoc_split_eq (=) k xs


  (** [group_assoc_eq (=) [(1,a);(1,b);(2,x);(2,y)] = [(1,[a,b]),(2,[x,y])]] *)
  let group_assoc_eq eq (xs: ('a * 'b) list) : ('a * ('b list)) list  =
    let rec f a = function
      | [] -> a
      | (k,v)::xs ->
        let a', b = assoc_split_eq eq k xs in
        f ((k,v::a')::a) b
    in f [] xs

  (** [group_assoc [(1,a);(1,b);(2,x);(2,y)] = [(1,[a,b]),(2,[x,y])]] *)
  let group_assoc xs = group_assoc_eq (=) xs

  let filter_presubs n xs =
    let f n =
      let x = try assoc n !analyses_table with Not_found -> Printf.eprintf "filter_presubs: Analysis '%d' not registered.\n" n; failwith "filter_presubs" in
      let y = try assoc n xs with Not_found ->
        (*iter (Printf.printf "%s\n" % flip assoc !analyses_table % fst) xs;*)
        Printf.eprintf "filter_presubs: Analysis '%s' (%d) not found.\n" x n; failwith "filter_presubs" in
      x, y
    in
    map f (assoc n !dep_list)

  let do_spawns ctx (xs:(varinfo * (int * exp list)) list) =
    let spawn_one v d =
      List.iter (fun (n, args) -> ctx.spawn v args) d
    in
    if not (get_bool "exp.single-threaded") then
      iter (uncurry spawn_one) @@ group_assoc_eq Basetype.Variables.equal xs

  let do_sideg ctx (xs:(varinfo * (int * Obj.t)) list) =
    let side_one v d =
      let join_vals (n,(module S:Spec),d) =
        n, repr @@ fold_left (fun x y -> S.G.join x (obj y)) (S.G.bot ()) d
      in
      ctx.sideg v @@ topo_sort_an @@ map join_vals @@ spec_list @@ group_assoc (d @ G.bot ())
    in
    iter (uncurry side_one) @@ group_assoc_eq Basetype.Variables.equal xs

  let do_assigns ctx assigns (xs:(int * Obj.t) list) =
    if List.is_empty assigns then xs (* nothing to do *)
    else
      let spec_assign n d : Obj.t =
        (* spec of current analysis *)
        let (module S:Spec) = spec n in
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

  let finalize () =
    finalize ();
    Access.print_result ()

  let rec do_splits ctx pv (xs:(int * (Obj.t * exp * bool)) list) =
    let split_one n (d,e,tv) =
      let nv = assoc_replace (n,d) pv in
      ctx.split (branch {ctx with local = nv} e tv) one true
    in
    iter (uncurry split_one) xs

  and branch (ctx:(D.t, G.t, C.t) ctx) (e:exp) (tv:bool) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in (* why do we need to collect these instead of calling ctx.sideg directly? *)
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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
    if q then raise Deadcode else d

  and query (ctx:(D.t, G.t, C.t) ctx) q =
    let sides  = ref [] in
    let f a (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= []
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in query context.")
        ; split  = (fun d e tv -> failwith "Cannot \"split\" in query context.")
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in query context.")
        }
      in
      Queries.Result.meet a @@ S.query ctx' q
    in
    match q with
    | Queries.PrintFullState ->
      ignore (Pretty.printf "Current State:\n%a\n\n" D.pretty ctx.local);
      `Bot
    | Queries.Access(e,b,reach,conf) ->
      if reach || b then do_access ctx b reach conf e;
      Access.distribute_access_exp (do_access ctx) false false conf e;
      `Bot
    | _ ->
      let x = fold_left f `Top @@ spec_list ctx.local in
      do_sideg ctx !sides;
      x

  and part_access ctx (e:exp) (vo:varinfo option) (w: bool) =
    let open Access in
    let start = (LSSSet.singleton (LSSet.empty ()), LSSet.empty ()) in
    let sides  = ref [] in
    let f (po,lo) (n, (module S: Spec), d) : part =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= []
        ; global = (fun v         -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d       -> failwith "part_access::spawn")
        ; split  = (fun d e tv    -> failwith "part_access::split")
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> failwith "part_access::assign")
        }
      in
      let (pd, ld) = S.part_access ctx' e vo w in
      let ln = LSSet.union lo ld in
      let mult_po s = LSSSet.union (LSSSet.map (LSSet.union s) po) in
      let pn = LSSSet.fold mult_po pd (LSSSet.empty ())  in
      do_sideg ctx !sides;
      (* printf "e=%a po=%a pd=%a pn=%a\n" d_exp e LSSSet.pretty po LSSSet.pretty pd LSSSet.pretty pn; *)
      (pn, ln)
      (* (LSSSet.map (LSSet.add pn) po, LSSet.union lo ln) *)
    in
    List.fold_left f start (spec_list ctx.local)

  and do_access (ctx: (D.t, G.t, C.t) ctx) (w:bool) (reach:bool) (conf:int) (e:exp) =
    let open Queries in
    let add_access conf vo oo =
      let (po,pd) = part_access ctx e vo w in
      Access.add e w conf vo oo (po,pd)
    in
    let add_access_struct conf ci =
      let (po,pd) = part_access ctx e None w in
      Access.add_struct e w conf (`Struct (ci,`NoOffset)) None (po,pd)
    in
    let has_escaped g =
      match ctx.ask (Queries.MayEscape g) with
      | `Bool false -> false
      | _ -> true
    in
    (* The following function adds accesses to the lval-set ls
       -- this is the common case if we have a sound points-to set. *)
    let on_lvals ls includes_uk =
      let ls = LS.filter (fun (g,_) -> g.vglob || has_escaped g) ls in
      let conf = if reach then conf - 20 else conf in
      let conf = if includes_uk then conf - 10 else conf in
      let f (var, offs) =
        let coffs = Lval.CilLval.to_ciloffs offs in
        if var.vid = dummyFunDec.svar.vid then
          add_access conf None (Some coffs)
        else
          add_access conf (Some var) (Some coffs)
      in
      LS.iter f ls
    in
    let reach_or_mpt = if reach then ReachableFrom e else MayPointTo e in
    match ctx.ask reach_or_mpt with
    | `Bot -> ()
    | `LvalSet ls when not (LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
      (* the case where the points-to set is non top and does not contain unknown values *)
      on_lvals ls false
    | `LvalSet ls when not (LS.is_top ls) ->
      (* the case where the points-to set is non top and contains unknown values *)
      let includes_uk = ref false in
      (* now we need to access all fields that might be pointed to: is this correct? *)
      begin match ctx.ask (ReachableUkTypes e) with
        | `Bot -> ()
        | `TypeSet ts when Queries.TS.is_top ts ->
          includes_uk := true
        | `TypeSet ts ->
          if Queries.TS.is_empty ts = false then
            includes_uk := true;
          let f = function
            | TComp (ci, _) ->
              add_access_struct (conf - 50) ci
            | _ -> ()
          in
          Queries.TS.iter f ts
        | _ ->
          includes_uk := true
      end;
      on_lvals ls !includes_uk
    | _ ->
      add_access (conf - 60) None None

  let assign (ctx:(D.t, G.t, C.t) ctx) l e =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in assign context (cycles?).")
        }
      in
      n, repr @@ S.assign ctx' l e
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    if q then raise Deadcode else d


  let vdecl (ctx:(D.t, G.t, C.t) ctx) v =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in assign context (cycles?).")
        }
      in
      n, repr @@ S.vdecl ctx' v
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    if q then raise Deadcode else d

  let body (ctx:(D.t, G.t, C.t) ctx) f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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
    if q then raise Deadcode else d

  let return (ctx:(D.t, G.t, C.t) ctx) e f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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
    if q then raise Deadcode else d

  let intrpt (ctx:(D.t, G.t, C.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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
    if q then raise Deadcode else d

  let asm (ctx:(D.t, G.t, C.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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
    if q then raise Deadcode else d

  let skip (ctx:(D.t, G.t, C.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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
    if q then raise Deadcode else d

  let special (ctx:(D.t, G.t, C.t) ctx) r f a =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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
    if q then raise Deadcode else d

  let sync (ctx:(D.t, G.t, C.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f (n,(module S:Spec),d) (dl,cs) =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= []
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in sync context.")
        }
      in
      let d, ds = S.sync ctx' in
      (n, repr d)::dl, (map (fun (v,d) -> v, (n,repr d)::(remove_assoc n @@ G.bot ())) ds) @ cs
    in
    let d,cs = fold_right f (spec_list ctx.local) ([],[]) in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    do_splits ctx d !splits;
    d, cs

  let enter (ctx:(D.t, G.t, C.t) ctx) r f a =
    let spawns = ref [] in
    let sides  = ref [] in
    let f (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= []
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun _ _    -> failwith "Cannot \"split\" in enter context." )
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in enter context.")
        }
      in
      map (fun (c,d) -> ((n, repr c), (n, repr d))) @@ S.enter ctx' r f a
    in
    let css = map f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    map (fun xs -> (topo_sort_an @@ map fst xs, topo_sort_an @@ map snd xs)) @@ n_cartesian_product css

  let combine (ctx:(D.t, G.t, C.t) ctx) r fe f a fc fd =
    let spawns = ref [] in
    let sides  = ref [] in
    let assigns = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let rec ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v a    -> spawns := (v,(n,a)) :: !spawns)
        ; split  = (fun d e tv -> failwith "Cannot \"split\" in combine context.")
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> assigns := (v,e,name, repr ctx')::!assigns)
        }
      in
      n, repr @@ S.combine ctx' r fe f a (obj (assoc n fc)) (obj (assoc n fd))
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    do_spawns ctx !spawns;
    let d = do_assigns ctx !assigns d in
    if q then raise Deadcode else d

  let threadenter (ctx:(D.t, G.t, C.t) ctx) f a =
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in threadenter context.")
        ; split  = (fun d e tv -> failwith "Cannot \"split\" in threadenter context.")
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> failwith "Cannot \"assign\" in threadenter context.")
        }
      in
      n, repr @@ S.threadenter ctx' f a
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    if q then raise Deadcode else d

  let threadcombine (ctx:(D.t, G.t, C.t) ctx) f a fd =
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t, S.C.t) ctx =
        { local  = obj d
        ; node   = ctx.node
        ; prev_node = ctx.prev_node
        ; control_context = ctx.control_context
        ; context = (fun () -> ctx.context () |> assoc n |> obj)
        ; edge   = ctx.edge
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in threadcombine context.")
        ; split  = (fun d e tv -> failwith "Cannot \"split\" in threadcombine context.")
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        ; assign = (fun ?name v e -> failwith "Cannot \"assign\" in threadcombine context.")
        }
      in
      n, repr @@ S.threadcombine ctx' f a (obj (assoc n fd))
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
    do_sideg ctx !sides;
    if q then raise Deadcode else d
end
