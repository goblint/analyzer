open Prelude.Ana
open Analyses

type spec_modules = { name : string
                    ; dep  : string list
                    ; spec : (module MCPSpec)
                    ; dom  : (module Lattice.S)
                    ; glob : (module Lattice.S)
                    ; cont : (module Printable.S)
                    ; var  : (module SpecSysVar)
                    ; acc  : (module MCPA) }

let activated  : (int * spec_modules) list ref = ref []
let activated_ctx_sens: (int * spec_modules) list ref = ref []
let registered: (int, spec_modules) Hashtbl.t = Hashtbl.create 100
let registered_name: (string, int) Hashtbl.t = Hashtbl.create 100

let register_analysis =
  let count = ref 0 in
  fun ?(dep=[]) (module S:MCPSpec) ->
    let n = S.name () in
    let s = { name = n
            ; dep
            ; spec = (module S : MCPSpec)
            ; dom  = (module S.D : Lattice.S)
            ; glob = (module S.G : Lattice.S)
            ; cont = (module S.C : Printable.S)
            ; var  = (module S.V : SpecSysVar)
            ; acc  = (module S.A : MCPA)
            }
    in
    Hashtbl.replace registered !count s;
    Hashtbl.replace registered_name n !count;
    incr count

let find_spec = Hashtbl.find registered
let find_spec_name n = (find_spec n).name
let find_id = Hashtbl.find registered_name

type unknown = Obj.t

module type DomainListPrintableSpec =
sig
  val assoc_dom : int -> (module Printable.S)
  val domain_list : unit -> (int * (module Printable.S)) list
end

module type DomainListSysVarSpec =
sig
  val assoc_dom : int -> (module SpecSysVar)
  val domain_list : unit -> (int * (module SpecSysVar)) list
end

module type DomainListMCPASpec =
sig
  val assoc_dom : int -> (module MCPA)
  val domain_list : unit -> (int * (module MCPA)) list
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

module PrintableOfMCPASpec (D:DomainListMCPASpec) : DomainListPrintableSpec =
struct
  let assoc_dom n =
    let f (module L:MCPA) = (module L : Printable.S)
    in
    f (D.assoc_dom n)

  let domain_list () =
    let f (module L:MCPA) = (module L : Printable.S) in
    List.map (fun (x,y) -> (x,f y)) (D.domain_list ())
end

module PrintableOfSysVarSpec (D:DomainListSysVarSpec) : DomainListPrintableSpec =
struct
  let assoc_dom n =
    let f (module L:SpecSysVar) = (module L : Printable.S)
    in
    f (D.assoc_dom n)

  let domain_list () =
    let f (module L:SpecSysVar) = (module L : Printable.S) in
    List.map (fun (x,y) -> (x,f y)) (D.domain_list ())
end

module DomListPrintable (DLSpec : DomainListPrintableSpec)
  : Printable.S with type t = (int * unknown) list
=
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open DLSpec
  open List
  open Obj

  type t = (int * unknown) list

  let unop_fold f a (x:t) =
    fold_left2 (fun a (n,d) (n',s) -> assert (n = n'); f a n s d) a x (domain_list ())

  let pretty () x =
    let f a n (module S : Printable.S) x = Pretty.dprintf "%s:%a" (S.name ()) S.pretty (obj x) :: a in
    let xs = unop_fold f [] x in
    match xs with
    | [] -> text "[]"
    | x :: [] -> x
    | x :: y ->
      let rest  = List.fold_left (fun p n->p ++ text "," ++ break ++ n) nil y in
      text "[" ++ align ++ x ++ rest ++ unalign ++ text "]"

  let show x =
    let xs = unop_fold (fun a n (module S : Printable.S) x ->
        let analysis_name = find_spec_name n in
        (analysis_name ^ ":(" ^ S.show (obj x) ^ ")") :: a) [] x
    in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (rev xs)

  let to_yojson xs =
    let f a n (module S : Printable.S) x =
      let name = find_spec_name n in
      (name, S.to_yojson (obj x)) :: a
    in `Assoc (unop_fold f [] xs)

  let binop_for_all f (x:t) (y:t) =
    GobList.for_all3 (fun (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f n s d d') x y (domain_list ())

  (* too specific for GobList *)
  let rec compare3 f l1 l2 l3 = match l1, l2, l3 with
    | [], [], [] -> 0
    | x1 :: l1, x2 :: l2, x3 :: l3 ->
      let c = f x1 x2 x3 in
      if c <> 0 then
        c
      else
        (compare3 [@tailcall]) f l1 l2 l3
    | _, _, _ -> invalid_arg "DomListPrintable.compare3"

  let binop_compare f (x:t) (y:t) =
    compare3 (fun (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f n s d d') x y (domain_list ())

  let equal   x y = binop_for_all (fun n (module S : Printable.S) x y -> S.equal (obj x) (obj y)) x y
  let compare x y = binop_compare (fun n (module S : Printable.S) x y -> S.compare (obj x) (obj y)) x y

  let hashmul x y = if x=0 then y else if y=0 then x else x*y

  let hash     = unop_fold (fun a n (module S : Printable.S) x -> hashmul a @@ S.hash (obj x)) 0

  (* let name () =
       let domain_name (n, (module D: Printable.S)) =
         let analysis_name = find_spec_name n in
         analysis_name ^ ":(" ^ D.name () ^ ")"
       in
       IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (map domain_name @@ domain_list ()) *)
  let name () = "MCP.C"

  let printXml f xs =
    let print_one a n (module S : Printable.S) x : unit =
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (find_spec_name n);
      S.printXml f (obj x);
      BatPrintf.fprintf f "</analysis>\n"
    in
    unop_fold print_one () xs

  let arbitrary () =
    let arbs = map (fun (n, (module D: Printable.S)) -> QCheck.map ~rev:(fun (_, o) -> obj o) (fun x -> (n, repr x)) @@ D.arbitrary ()) @@ domain_list () in
    MyCheck.Arbitrary.sequence arbs
end

module DomVariantPrintable (DLSpec : DomainListPrintableSpec)
  : Printable.S with type t = int * unknown
=
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open DLSpec
  open List
  open Obj

  type t = int * unknown

  let unop_map f ((n, d):t) =
    f n (assoc_dom n) d

  let pretty () = unop_map (fun n (module S: Printable.S) x ->
      let analysis_name = find_spec_name n in
      Pretty.dprintf "%s:%a" analysis_name S.pretty (obj x)
    )

  let show = unop_map (fun n (module S: Printable.S) x ->
      let analysis_name = find_spec_name n in
      analysis_name ^ ":" ^ S.show (obj x)
    )

  let to_yojson x =
    `Assoc [
      unop_map (fun n (module S: Printable.S) x ->
          let name = find_spec_name n in
          (name, S.to_yojson (obj x))
        ) x
    ]

  let equal (n1, x1) (n2, x2) =
    n1 = n2 && (
      let module S = (val assoc_dom n1) in
      S.equal (obj x1) (obj x2)
    )

  let compare (n1, x1) (n2, x2) =
    let r = Stdlib.compare n1 n2 in
    if r <> 0 then
      r
    else
      let module S = (val assoc_dom n1) in
      S.compare (obj x1) (obj x2)

  let hash = unop_map (fun n (module S: Printable.S) x ->
      Hashtbl.hash (n, S.hash (obj x))
    )

  let name () =
    let domain_name (n, (module S: Printable.S)) =
      let analysis_name = find_spec_name n in
      analysis_name ^ ":" ^ S.name ()
    in
    IO.to_string (List.print ~first:"" ~last:"" ~sep:" | " String.print) (map domain_name @@ domain_list ())

  let printXml f = unop_map (fun n (module S: Printable.S) x ->
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (find_spec_name n);
      S.printXml f (obj x);
      BatPrintf.fprintf f "</analysis>\n"
    )

  let arbitrary () =
    let arbs = map (fun (n, (module S: Printable.S)) -> QCheck.map ~rev:(fun (_, o) -> obj o) (fun x -> (n, repr x)) @@ S.arbitrary ()) @@ domain_list () in
    QCheck.oneof arbs
end

module DomVariantSysVar (DLSpec : DomainListSysVarSpec)
  : SpecSysVar with type t = int * unknown
=
struct
  open DLSpec
  open Obj

  include DomVariantPrintable (PrintableOfSysVarSpec (DLSpec))
  let name () = "MCP.V"

  let unop_map f ((n, d):t) =
    f n (assoc_dom n) d

  let is_write_only = unop_map (fun n (module S: SpecSysVar) x ->
      S.is_write_only (obj x)
    )
end

module DomListLattice (DLSpec : DomainListLatticeSpec)
  : Lattice.S with type t = (int * unknown) list
=
struct
  open DLSpec
  open List
  open Obj

  include DomListPrintable (PrintableOfLatticeSpec (DLSpec))

  let binop_fold f a (x:t) (y:t) =
    GobList.fold_left3 (fun a (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f a n s d d') a x y (domain_list ())

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) x y =
    List.rev @@ binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) [] x y

  let binop_for_all f (x:t) (y:t) =
    GobList.for_all3 (fun (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f n s d d') x y (domain_list ())

  let unop_for_all f (x:t) =
    List.for_all2 (fun (n,d) (n',s) -> assert (n = n'); f n s d) x (domain_list ())

  let narrow = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.narrow (obj x) (obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.widen  (obj x) (obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.meet   (obj x) (obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.join   (obj x) (obj y))

  let leq    = binop_for_all (fun n (module S : Lattice.S) x y -> S.leq (obj x) (obj y))

  let is_top = unop_for_all (fun n (module S : Lattice.S) x -> S.is_top (obj x))
  let is_bot = unop_for_all (fun n (module S : Lattice.S) x -> S.is_bot (obj x))

  let top () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.top ())) @@ domain_list ()
  let bot () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.bot ())) @@ domain_list ()

  let pretty_diff () (x,y) =
    let f a n (module S : Lattice.S) x y =
      if S.leq (obj x) (obj y) then a
      else a ++ S.pretty_diff () (obj x, obj y) ++ text ". "
    in
    binop_fold f nil x y
end

module DomVariantLattice0 (DLSpec : DomainListLatticeSpec)
  : Lattice.S with type t = int * unknown
=
struct
  open DLSpec
  open Obj

  include DomVariantPrintable (PrintableOfLatticeSpec (DLSpec))
  let name () = "MCP.G"

  let binop_map' (f: int -> (module Lattice.S) -> Obj.t -> Obj.t -> 'a) (n1, d1) (n2, d2) =
    assert (n1 = n2);
    f n1 (assoc_dom n1) d1 d2

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) =
    binop_map' (fun n s d1 d2 -> (n, f s d1 d2))

  let narrow = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.narrow (obj x) (obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.widen  (obj x) (obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.meet   (obj x) (obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.join   (obj x) (obj y))

  let leq    = binop_map' (fun _ (module S : Lattice.S) x y -> S.leq (obj x) (obj y))

  let is_top x = false
  let is_bot x = false
  let top () = failwith "DomVariantLattice0.top"
  let bot () = failwith "DomVariantLattice0.bot"

  let pretty_diff () (x, y) =
    let f _ (module S : Lattice.S) x y =
      if S.leq (obj x) (obj y) then nil
      else S.pretty_diff () (obj x, obj y)
    in
    binop_map' f x y
end

module DomVariantLattice (DLSpec : DomainListLatticeSpec) =
struct
  include Lattice.Lift (DomVariantLattice0 (DLSpec)) (Printable.DefaultNames)
  let name () = "MCP.G"
end

module LocalDomainListSpec : DomainListLatticeSpec =
struct
  let assoc_dom n = (find_spec n).dom
  let domain_list () = List.map (fun (n,p) -> n, p.dom) !activated
end

module GlobalDomainListSpec : DomainListLatticeSpec =
struct
  let assoc_dom n = (find_spec n).glob
  let domain_list () = List.map (fun (n,p) -> n, p.glob) !activated
end

module ContextListSpec : DomainListPrintableSpec =
struct
  let assoc_dom n = (find_spec n).cont
  let domain_list () = List.map (fun (n,p) -> n, p.cont) !activated_ctx_sens
end

module VarListSpec : DomainListSysVarSpec =
struct
  let assoc_dom n = (find_spec n).var
  let domain_list () = List.map (fun (n,p) -> n, p.var) !activated
end

module AccListSpec : DomainListMCPASpec =
struct
  let assoc_dom n = (find_spec n).acc
  let domain_list () = List.map (fun (n,p) -> n, p.acc) !activated
end
