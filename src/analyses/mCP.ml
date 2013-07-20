(** Master Control Program *)

open Batteries
open GobConfig
open Analyses
open Pretty
open Cil

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
    let n = S.name in
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
    let xs = unop_fold (fun a n (module S : Printable.S) x -> S.short w2 (obj x) :: a) [] x in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) xs

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
  
  let name () = IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (map (flip assoc !analyses_table) @@ map fst @@ domain_list ())

  let toXML_f sf x =
    let xs = unop_fold (fun a n (module S : Printable.S) x -> S.toXML (obj x) :: a) [] x in
    let esc = Goblintutil.escape in
    let node_leaf = if xs = [] then "Leaf" else "Node" in
      Xml.Element (node_leaf, [("text", esc (sf Goblintutil.summary_length x))], xs)

  let toXML = toXML_f short
  
  let pretty_diff () (x,y) = text "Please override me!"
    
  let printXml f xs =
    let print_one a n (module S : Printable.S) x : unit = 
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (S.name ());
      S.printXml f (obj x);
      BatPrintf.fprintf f "</analysis>\n"
    in
    unop_fold print_one () xs
  
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
  open Tuple4
  let assoc_dom n = (List.assoc n !analyses_list).dom
  let domain_list () = List.map (fun (n,p) -> n, p.dom) !analyses_list
end

module GlobalDomainListSpec : DomainListLatticeSpec =
struct
  open Tuple4
  let assoc_dom n = (List.assoc n !analyses_list).glob
  let domain_list () = List.map (fun (n,p) -> n, p.glob) !analyses_list
end

module ContextListSpec : DomainListPrintableSpec =
struct
  open Tuple4
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
  
  let name = "MCP2"
    
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
        raise Goblintutil.BailFromMain
      end
    in
    let deps (x,_) = iter (check_dep x) @@ assoc x !dep_list in
    iter deps xs
    
  let init     () = 
    let map' f = 
      let f x = 
        try Some (f x) 
        with Not_found -> Legacy.Printf.fprintf !Messages.warn_out "Analysis '%s' not found. Ignoring.\n" x;None
      in
      List.filter_map f
    in
    let xs = map Json.string @@ get_list "ana.activated[0]" in
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
      iter (fun (_,{spec=(module S:Spec)}) -> S.init ()) !analyses_list
          
  let finalize () = iter (fun (_,{spec=(module S:Spec)}) -> S.finalize ()) !analyses_list

  let spec x = (assoc x !analyses_list).spec
  let spec_list xs = 
    map (fun (n,x) -> (n,spec n,x)) xs
  
  let map_deadcode f xs =
    let dead = ref false in
    let one_el xs (n,(module S:Spec),d) = try f xs (n,(module S:Spec),d) :: xs with Deadcode -> dead:=true; (n,repr @@ S.D.bot ()) :: xs in
    let ys = fold_left one_el [] xs in
    List.rev ys, !dead

  let val_of = identity
  let context x = 
    let x = filter (fun (x,_) -> not (mem x !cont_inse)) x in
    let x = spec_list x in
      map (fun (n,(module S:Spec),d) -> n, repr @@ S.context (obj d)) x

  let should_join x y = 
    let xs = filter (fun (x,_) -> mem x !path_sens) x in
    let ys = filter (fun (x,_) -> mem x !path_sens) y in
    D.equal xs ys
    
  let otherstate v = map (fun (n,{spec=(module S:Spec)}) -> n, repr @@ S.otherstate v) !analyses_list
  let exitstate  v = map (fun (n,{spec=(module S:Spec)}) -> n, repr @@ S.exitstate  v) !analyses_list
  let startstate v = map (fun (n,{spec=(module S:Spec)}) -> n, repr @@ S.startstate v) !analyses_list
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
  
  let do_spawns ctx (xs:(varinfo * (int * Obj.t)) list) =
    let spawn_one v d = 
      let join_vals (n,(module S:Spec),d) =
        n, repr @@ fold_left (fun x y -> S.D.join x (obj y)) (S.D.bot ()) d
      in
      let otherstates = List.filter (fun (n,_) -> not (mem_assoc n d)) (otherstate v) in
      ctx.spawn v @@ topo_sort_an @@ map join_vals @@ spec_list @@ group_assoc (d @ otherstates)
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

  
  let rec do_splits ctx pv (xs:(int * (Obj.t * exp * bool)) list) =
    let split_one n (d,e,tv) = 
      let nv = assoc_replace (n,d) pv in
      ctx.split (branch {ctx with local = nv} e tv) one true
    in
    iter (uncurry split_one) xs

  
  and branch (ctx:(D.t, G.t) ctx) (e:exp) (tv:bool) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.branch ctx' e tv
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
      
  and query (ctx:(D.t, G.t) ctx) q =
    let f a (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= []
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in query context.")
        ; split  = (fun d e tv -> failwith "Cannot \"split\" in query context.")
        ; sideg  = (fun v g    -> failwith "Cannot \"sideg\" in query context.")
        } 
      in
      Queries.Result.meet a @@ S.query ctx' q
    in
      fold_left f `Top @@ spec_list ctx.local 

  let assign (ctx:(D.t, G.t) ctx) l e =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.assign ctx' l e 
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
  
  let body (ctx:(D.t, G.t) ctx) f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.body ctx' f 
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
  
  let return (ctx:(D.t, G.t) ctx) e f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.return ctx' e f
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
      
  let intrpt (ctx:(D.t, G.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.intrpt ctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
      
  let special (ctx:(D.t, G.t) ctx) r f a =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.special ctx' r f a
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      do_splits ctx d !splits;
      if q then raise Deadcode else d

  let sync (ctx:(D.t, G.t) ctx) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f (n,(module S:Spec),d) (dl,cs) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= []
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
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

  let enter (ctx:(D.t, G.t) ctx) r f a =
    let spawns = ref [] in
    let sides  = ref [] in
    let f (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= []
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun _ _    -> failwith "Cannot \"split\" in enter context." )
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      map (fun (c,d) -> ((n, repr c), (n, repr d))) @@ S.enter ctx' r f a
    in
    let css = map f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      map (fun xs -> (topo_sort_an @@ map fst xs, topo_sort_an @@ map snd xs)) @@ n_cartesian_product css
  
  let combine (ctx:(D.t, G.t) ctx) r fe f a fd =
    let spawns = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec),d) =
      let ctx' : (S.D.t, S.G.t) ctx = 
        { local  = obj d
        ; ask    = query ctx
        ; presub = filter_presubs n ctx.local
        ; postsub= filter_presubs n post_all
        ; global = (fun v      -> ctx.global v |> assoc n |> obj)
        ; spawn  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split  = (fun d e tv -> failwith "Cannot \"split\" in combine context.")
        ; sideg  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.combine ctx' r fe f a @@ obj @@ assoc n fd
    in
    let d, q = map_deadcode f @@ spec_list ctx.local in
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      if q then raise Deadcode else d
      
end
