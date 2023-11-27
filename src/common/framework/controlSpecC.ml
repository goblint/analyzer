module Failwith = Printable.Failwith (
  struct
    let message = "uninitialized control_spec_c"
  end
  )

let control_spec_c: (module Printable.S) ref = ref (module Failwith: Printable.S)


type t = Obj.t (** represents [(val !control_spec_c).t] *)

(* The extra level of indirection allows calls to this static module to go to a dynamic first-class module. *)

let name () =
  let module C = (val !control_spec_c) in
  C.name ()

let equal x y =
  let module C = (val !control_spec_c) in
  C.equal (Obj.obj x) (Obj.obj y)
let compare x y =
  let module C = (val !control_spec_c) in
  C.compare (Obj.obj x) (Obj.obj y)
let hash x =
  let module C = (val !control_spec_c) in
  C.hash (Obj.obj x)
let tag x =
  let module C = (val !control_spec_c) in
  C.tag (Obj.obj x)

let show x =
  let module C = (val !control_spec_c) in
  C.show (Obj.obj x)
let pretty () x =
  let module C = (val !control_spec_c) in
  C.pretty () (Obj.obj x)
let printXml f x =
  let module C = (val !control_spec_c) in
  C.printXml f (Obj.obj x)
let to_yojson x =
  let module C = (val !control_spec_c) in
  C.to_yojson (Obj.obj x)

let arbitrary () =
  let module C = (val !control_spec_c) in
  QCheck.map ~rev:Obj.obj Obj.repr (C.arbitrary ())
let relift x =
  let module C = (val !control_spec_c) in
  Obj.repr (C.relift (Obj.obj x))
