(** Registry of dynamically activatable analyses.
    Analysis specification modules for the dynamic product. *)

open Batteries
open GoblintCil
open Pretty
open Analyses

type spec_modules = { name : string
                    ; dep  : string list
                    ; spec : (module MCPSpec)
                    ; dom  : (module Lattice.S)
                    ; glob : (module Lattice.S)
                    ; cont : (module Printable.S)
                    ; var  : (module SpecSysVar)
                    ; acc  : (module MCPA)
                    ; path : (module DisjointDomain.Representative) }

let activated  : (int * spec_modules) list ref = ref []
let activated_context_sens: (int * spec_modules) list ref = ref []
let activated_path_sens: (int * spec_modules) list ref = ref []
let registered: (int, spec_modules) Hashtbl.t = Hashtbl.create 100
let registered_name: (string, int) Hashtbl.t = Hashtbl.create 100

let register_analysis =
  let count = ref 0 in
  fun ?(dep=[]) (module S:MCPSpec) ->
    let n = S.name () in
    let module P =
    struct
      include S.P
      type elt = S.D.t
    end
    in
    let s = { name = n
            ; dep
            ; spec = (module S : MCPSpec)
            ; dom  = (module S.D : Lattice.S)
            ; glob = (module S.G : Lattice.S)
            ; cont = (module S.C : Printable.S)
            ; var  = (module S.V : SpecSysVar)
            ; acc  = (module S.A : MCPA)
            ; path = (module P : DisjointDomain.Representative)
            }
    in
    Hashtbl.replace registered !count s;
    Hashtbl.replace registered_name n !count;
    incr count

let find_spec = Hashtbl.find registered
let find_spec_name n = (find_spec n).name
let find_id = Hashtbl.find registered_name

module type DomainListPrintableSpec =
sig
  val assoc_dom : int -> (module Printable.S)
  val domain_list : unit -> (int * (module Printable.S)) list
end

module type DomainListRepresentativeSpec =
sig
  val assoc_dom : int -> (module DisjointDomain.Representative)
  val domain_list : unit -> (int * (module DisjointDomain.Representative)) list
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

module PrintableOfRepresentativeSpec (D:DomainListRepresentativeSpec) : DomainListPrintableSpec =
struct
  let assoc_dom n =
    let f (module L:DisjointDomain.Representative) = (module L : Printable.S)
    in
    f (D.assoc_dom n)

  let domain_list () =
    let f (module L:DisjointDomain.Representative) = (module L : Printable.S) in
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
  : Printable.S with type t = (int * Obj.t) list
=
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open DLSpec
  open List

  type t = (int * Obj.t) list

  let unop_fold f a (x:t) =
    fold_left2 (fun a (n,d) (n',s) -> assert (n = n'); f a n s d) a x (domain_list ())

  let unop_map f x =
    List.rev @@ unop_fold (fun a n s d -> (n, f s d) :: a) [] x

  let pretty () xs =
    let pretty_one a n (module S: Printable.S) x =
      let doc = Pretty.dprintf "%s:%a" (find_spec_name n) S.pretty (Obj.obj x) in
      match a with
      | None -> Some doc
      | Some a -> Some (a ++ text "," ++ line ++ doc)
    in
    let doc = Option.default Pretty.nil (unop_fold pretty_one None xs) in
    Pretty.dprintf "[@[%a@]]" Pretty.insert doc

  let show x =
    let xs = unop_fold (fun a n (module S : Printable.S) x ->
        let analysis_name = find_spec_name n in
        (analysis_name ^ ":(" ^ S.show (Obj.obj x) ^ ")") :: a
      ) [] x
    in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (rev xs)

  let to_yojson xs =
    let f a n (module S : Printable.S) x =
      let name = find_spec_name n in
      (name, S.to_yojson (Obj.obj x)) :: a
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

  let equal   x y = binop_for_all (fun n (module S : Printable.S) x y -> S.equal (Obj.obj x) (Obj.obj y)) x y
  let compare x y = binop_compare (fun n (module S : Printable.S) x y -> S.compare (Obj.obj x) (Obj.obj y)) x y

  let hashmul x y = if x=0 then y else if y=0 then x else x*y

  let hash     = unop_fold (fun a n (module S : Printable.S) x -> hashmul a @@ S.hash (Obj.obj x)) 0

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
      S.printXml f (Obj.obj x);
      BatPrintf.fprintf f "</analysis>\n"
    in
    unop_fold print_one () xs

  let arbitrary () =
    let arbs = map (fun (n, (module D: Printable.S)) -> QCheck.map ~rev:(fun (_, o) -> Obj.obj o) (fun x -> (n, Obj.repr x)) @@ D.arbitrary ()) @@ domain_list () in
    GobQCheck.Arbitrary.sequence arbs

  let relift = unop_map (fun (module S: Printable.S) x -> Obj.repr (S.relift (Obj.obj x)))
end

module DomVariantPrintable (DLSpec : DomainListPrintableSpec)
  : Printable.S with type t = int * Obj.t
=
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open DLSpec
  open List

  type t = int * Obj.t

  let unop_map f ((n, d):t) =
    f n (assoc_dom n) d

  let pretty () = unop_map (fun n (module S: Printable.S) x ->
      let analysis_name = find_spec_name n in
      Pretty.dprintf "%s:%a" analysis_name S.pretty (Obj.obj x)
    )

  let show = unop_map (fun n (module S: Printable.S) x ->
      let analysis_name = find_spec_name n in
      analysis_name ^ ":" ^ S.show (Obj.obj x)
    )

  let to_yojson x =
    `Assoc [
      unop_map (fun n (module S: Printable.S) x ->
          let name = find_spec_name n in
          (name, S.to_yojson (Obj.obj x))
        ) x
    ]

  let equal (n1, x1) (n2, x2) =
    n1 = n2 && (
      let module S = (val assoc_dom n1) in
      S.equal (Obj.obj x1) (Obj.obj x2)
    )

  let compare (n1, x1) (n2, x2) =
    let r = Stdlib.compare n1 n2 in
    if r <> 0 then
      r
    else
      let module S = (val assoc_dom n1) in
      S.compare (Obj.obj x1) (Obj.obj x2)

  let hash = unop_map (fun n (module S: Printable.S) x ->
      Hashtbl.hash (n, S.hash (Obj.obj x))
    )

  let name () =
    let domain_name (n, (module S: Printable.S)) =
      let analysis_name = find_spec_name n in
      analysis_name ^ ":" ^ S.name ()
    in
    IO.to_string (List.print ~first:"" ~last:"" ~sep:" | " String.print) (map domain_name @@ domain_list ())

  let printXml f = unop_map (fun n (module S: Printable.S) x ->
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (find_spec_name n);
      S.printXml f (Obj.obj x);
      BatPrintf.fprintf f "</analysis>\n"
    )

  let arbitrary () =
    let arbs = map (fun (n, (module S: Printable.S)) -> QCheck.map ~rev:(fun (_, o) -> Obj.obj o) (fun x -> (n, Obj.repr x)) @@ S.arbitrary ()) @@ domain_list () in
    QCheck.oneof arbs

  let relift = unop_map (fun n (module S: Printable.S) x -> (n, Obj.repr (S.relift (Obj.obj x))))
end

module DomVariantSysVar (DLSpec : DomainListSysVarSpec)
  : SpecSysVar with type t = int * Obj.t
=
struct
  open DLSpec

  include DomVariantPrintable (PrintableOfSysVarSpec (DLSpec))
  let name () = "MCP.V"

  let unop_map f ((n, d):t) =
    f n (assoc_dom n) d

  let is_write_only = unop_map (fun n (module S: SpecSysVar) x ->
      S.is_write_only (Obj.obj x)
    )
end

module DomListRepresentative (DLSpec : DomainListRepresentativeSpec)
  : DisjointDomain.Representative with type t = (int * Obj.t) list and type elt = (int * Obj.t) list
=
struct
  open DLSpec
  open List

  include DomListPrintable (PrintableOfRepresentativeSpec (DLSpec))
  let name () = "MCP.P"

  type elt = (int * Obj.t) list

  let of_elt (xs: elt): t =
    let rec aux xs ss acc =
      match xs, ss with
      | [], [] -> acc
      | _ :: _, [] -> acc
      | (n, d) :: xs', (n', (module P: DisjointDomain.Representative)) :: ss' when n = n' ->
        aux xs' ss' ((n, Obj.repr (P.of_elt (Obj.obj d))) :: acc)
      | _ :: xs', _ :: _ ->
        aux xs' ss acc
      | [], _ :: _ -> invalid_arg "DomListRepresentative.of_elt"
    in
    List.rev (aux xs (domain_list ()) [])
end

module DomListLattice (DLSpec : DomainListLatticeSpec)
  : Lattice.S with type t = (int * Obj.t) list
=
struct
  open DLSpec
  open List

  include DomListPrintable (PrintableOfLatticeSpec (DLSpec))
  let name () = "MCP.D"

  let binop_fold f a (x:t) (y:t) =
    GobList.fold_left3 (fun a (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f a n s d d') a x y (domain_list ())

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) x y =
    List.rev @@ binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) [] x y

  let binop_for_all f (x:t) (y:t) =
    GobList.for_all3 (fun (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f n s d d') x y (domain_list ())

  let unop_for_all f (x:t) =
    List.for_all2 (fun (n,d) (n',s) -> assert (n = n'); f n s d) x (domain_list ())

  let narrow = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.narrow (Obj.obj x) (Obj.obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.widen  (Obj.obj x) (Obj.obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.meet   (Obj.obj x) (Obj.obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.join   (Obj.obj x) (Obj.obj y))

  let leq    = binop_for_all (fun n (module S : Lattice.S) x y -> S.leq (Obj.obj x) (Obj.obj y))

  let is_top = unop_for_all (fun n (module S : Lattice.S) x -> S.is_top (Obj.obj x))
  let is_bot = unop_for_all (fun n (module S : Lattice.S) x -> S.is_bot (Obj.obj x))

  let top () = map (fun (n,(module S : Lattice.S)) -> (n, Obj.repr @@ S.top ())) @@ domain_list ()
  let bot () = map (fun (n,(module S : Lattice.S)) -> (n, Obj.repr @@ S.bot ())) @@ domain_list ()

  let pretty_diff () (xs, ys) =
    let pretty_one a n (module S: Lattice.S) x y =
      if S.leq (Obj.obj x) (Obj.obj y) then
        a
      else (
        let doc = Pretty.dprintf "%s:%a" (find_spec_name n) S.pretty_diff (Obj.obj x, Obj.obj y) in
        match a with
        | None -> Some doc
        | Some a -> Some (a ++ text "," ++ line ++ doc)
      )
    in
    let doc = Option.default Pretty.nil (binop_fold pretty_one None xs ys) in
    Pretty.dprintf "[@[%a@]]" Pretty.insert doc
end

module DomVariantLattice0 (DLSpec : DomainListLatticeSpec)
  : Lattice.S with type t = int * Obj.t
=
struct
  open DLSpec

  include DomVariantPrintable (PrintableOfLatticeSpec (DLSpec))
  let name () = "MCP.G"

  let binop_map' (f: int -> (module Lattice.S) -> Obj.t -> Obj.t -> 'a) (n1, d1) (n2, d2) =
    assert (n1 = n2);
    f n1 (assoc_dom n1) d1 d2

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) =
    binop_map' (fun n s d1 d2 -> (n, f s d1 d2))

  let narrow = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.narrow (Obj.obj x) (Obj.obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.widen  (Obj.obj x) (Obj.obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.meet   (Obj.obj x) (Obj.obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> Obj.repr @@ S.join   (Obj.obj x) (Obj.obj y))

  let leq    = binop_map' (fun _ (module S : Lattice.S) x y -> S.leq (Obj.obj x) (Obj.obj y))

  let is_top x = false
  let is_bot x = false
  let top () = failwith "DomVariantLattice0.top"
  let bot () = failwith "DomVariantLattice0.bot"

  let pretty_diff () (x, y) =
    let f _ (module S : Lattice.S) x y =
      if S.leq (Obj.obj x) (Obj.obj y) then nil
      else S.pretty_diff () (Obj.obj x, Obj.obj y)
    in
    binop_map' f x y
end

module DomVariantLattice (DLSpec : DomainListLatticeSpec) =
struct
  include Lattice.LiftConf (struct include Printable.DefaultConf let expand1 = false end) (DomVariantLattice0 (DLSpec))
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
  let domain_list () = List.map (fun (n,p) -> n, p.cont) !activated_context_sens
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

module PathListSpec : DomainListRepresentativeSpec =
struct
  let assoc_dom n = (find_spec n).path
  let domain_list () = List.map (fun (n,p) -> n, p.path) !activated_path_sens
end
