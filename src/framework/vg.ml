open Analyses

module type S0 =
sig

end

module type S =
sig
  module V0: SpecSysVar
  module G0: Lattice.S

  module V: SpecSysVar
  module G: Lattice.S

  val global: (_, G.t, _, V.t) man -> V0.t -> G0.t
  val sideg: (_, G.t, _, V.t) man -> V0.t -> G0.t -> unit
end

module Make (V: SpecSysVar) (G: Lattice.S): S with module V0 = V and module G0 = G =
struct
  module V0 = V
  module G0 = G
  module V = V
  module G = G

  let global man = man.global
  let sideg man = man.sideg
end


module Either (VG1: S) (VG2: S):
sig
  module V: SpecSysVar
  module G: Lattice.S
  module Left: S with module V0 = VG1.V0 and module G0 = VG1.G0 and module V = V and module G = G
  module Right: S with module V0 = VG2.V0 and module G0 = VG2.G0 and module V = V and module G = G
end =
struct
  module V =
  struct
    include Printable.Either (VG1.V) (VG2.V)
    include StdV
  end
  module G = Lattice.Lift2 (VG1.G) (VG2.G)

  module Left: S with module V0 = VG1.V0 and module G0 = VG1.G0 and module V = V and module G = G =
  struct
    module V0 = VG1.V0
    module G0 = VG1.G0
    module V = V
    module G = G

    let man' man: (_, VG1.G.t, _, VG1.V.t) man =
        {
          man with
          global = (fun x -> match man.global (`Left x) with
            | `Bot -> VG1.G.bot ()
            | `Lifted1 x -> x
            | _ -> assert false);
          sideg = (fun g x -> man.sideg (`Left g) (`Lifted1 x));
        }

    let global man g =
      VG1.global (man' man) g
    let sideg man g x =
      VG1.sideg (man' man) g x
  end

  module Right: S with module V0 = VG2.V0 and module G0 = VG2.G0 and module V = V and module G = G =
  struct
    module V0 = VG2.V0
    module G0 = VG2.G0
    module V = V
    module G = G

    let man' man: (_, VG2.G.t, _, VG2.V.t) man =
        {
          man with
          global = (fun x -> match man.global (`Right x) with
            | `Bot -> VG2.G.bot ()
            | `Lifted2 x -> x
            | _ -> assert false);
          sideg = (fun g x -> man.sideg (`Right g) (`Lifted2 x));
        }

    let global man g =
      VG2.global (man' man) g
    let sideg man g x =
      VG2.sideg (man' man) g x
  end
end


module type S2 =
sig
  include S

  val all_globals: (_, G.t, _, V.t) man -> V0.t Seq.t
end

module Make2 (V: SpecSysVar) (G: Lattice.S): S2 with module V0 = V and module G0 = G =
struct
  module V0 = V
  module G0 = G

  module V =
  struct
    include Printable.Either (V0) (UnitV)
    include StdV
  end

  module V0Set = SetDomain.Make (V0)

  module G =
  struct
    include Lattice.Lift2 (G0) (V0Set)
  end

  let global man g =
    match man.global (`Left g) with
    | `Bot -> G0.bot ()
    | `Lifted1 x -> x
    | _ -> assert false

  let sideg man g x =
    man.sideg (`Left g) (`Lifted1 x);
    man.sideg (`Right ()) (`Lifted2 (V0Set.singleton g))

  let all_globals man =
    match man.global (`Right ()) with
    | `Bot -> Seq.empty
    | `Lifted2 x -> V0Set.to_seq x
    | _ -> assert false
end

module Make32 (V: SpecSysVar) (G: Lattice.S): S2 with module V0 = V and module G0 = G =
struct
  module V0 = V
  module G0 = G
  module V0Set = SetDomain.Make (V)
  include Either (Make (V) (G)) (Make (UnitV) (V0Set))

  let global = Left.global

  let sideg man g x =
    Left.sideg man g x;
    Right.sideg man () (V0Set.singleton g)

  let all_globals man =
    Right.global man ()
    |> V0Set.to_seq
end

module Make3 (V: SpecSysVar) (G: Lattice.S): S2 with module V0 = V and module G0 = G =
struct
  module V0 = V
  module G0 = G

  module V = UnitV
  module G = MapDomain.MapBot (V0) (G0)

  let global man g = G.find g (man.global ())
  let sideg man g x = man.sideg () (G.singleton g x)

  let all_globals man =
    man.global ()
    |> G.bindings
    |> List.to_seq
    |> Seq.map fst
end
