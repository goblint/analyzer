module Stack (VD: Lattice.S) =
struct
  module Map = MapDomain.MapTop (Basetype.Variables) (VD)
  include Lattice.LiftBot (Map)

  type key = Map.key
  type value = Map.value

  let add k v x : t=
    match x with
    | `Lifted x -> `Lifted (Map.add k v x)
    | _ -> x
  let find k x =
    match x with
    | `Lifted x -> Map.find k x
    | _ -> VD.bot ()
  let remove key x =
    match x with
    | `Lifted x -> `Lifted (Map.remove key x)
    | _ -> x
  let iter f x =
    match x with
    | `Lifted x -> Map.iter f x
    | _ -> ()
  let map f x =
    match x with
    | `Lifted x -> `Lifted (Map.map f x)
    | `Bot -> `Bot
  let fold f x y =
    match x with
    | `Lifted x -> Map.fold f x y
    | _ -> y
  let add_list_fun ks f x =
    match x with
    | `Lifted x -> `Lifted (Map.add_list_fun ks f x)
    | _ -> x
  let add_list_set ks v x =
    match x with
    | `Lifted x -> `Lifted (Map.add_list_set ks v x)
    | _ -> x
  let add_list ds x =
    match x with
    | `Lifted x -> `Lifted (Map.add_list ds x)
    | _ -> x
  let long_map2 f x y =
    match x,y with
    | `Lifted x,`Lifted y -> `Lifted (Map.long_map2 f x y)
    |  _ -> `Bot
  let map2 f x y =
    match x,y with
    | `Lifted x,`Lifted y -> `Lifted (Map.map2 f x y)
    | _ -> `Bot
  let for_all f x =
    match x with
    | `Lifted x -> Map.for_all f x
    | _ -> false
end
