open Batteries
include Apron

module Scalar =
struct
  include Scalar

  let pp = print
  include Printable.SimpleFormat (
    struct
      type nonrec t = t
      let pp = pp
    end
    )

  let of_z z = of_mpqf (Mpqf.of_mpz (Z_mlgmpidl.mpz_of_z z))
end

module Coeff =
struct
  include Coeff

  let s_of_z z = Coeff.s_of_mpqf (Mpqf.of_mpz (Z_mlgmpidl.mpz_of_z z))
end

module Var =
struct
  include Var

  let pp = print
  include Printable.SimpleFormat (
    struct
      type nonrec t = t
      let pp = pp
    end
    )

  let equal x y = Var.compare x y = 0
end

module Lincons1 =
struct
  include Lincons1

  let pp = print
  include Printable.SimpleFormat (
    struct
      type nonrec t = t
      let pp = pp
    end
    )

  let compare x y =
    (* TODO: implement proper total Lincons1 order *)
    String.compare (show x) (show y) (* HACK *)

  let num_vars x =
    (* Apron.Linexpr0.get_size returns some internal nonsense, so we count ourselves. *)
    let size = ref 0 in
    Lincons1.iter (fun coeff var ->
        if not (Apron.Coeff.is_zero coeff) then
          incr size
      ) x;
    !size
end

module Lincons1Set =
struct
  include Set.Make (Lincons1)

  let of_earray ({lincons0_array; array_env}: Lincons1.earray): t =
    Array.enum lincons0_array
    |> Enum.map (fun (lincons0: Lincons0.t) ->
        Lincons1.{lincons0; env = array_env}
      )
    |> of_enum
end

module Texpr1 =
struct
  include Texpr1

  let pp = print
  include Printable.SimpleFormat (
    struct
      type nonrec t = t
      let pp = pp
    end
    )

  module Expr =
  struct
    type t = expr

    let pp = print_expr
    include Printable.SimpleFormat (
      struct
        type nonrec t = t
        let pp = pp
      end
      )
  end
end

module Tcons1 =
struct
  include Tcons1

  let pp = print
  include Printable.SimpleFormat (
    struct
      type nonrec t = t
      let pp = pp
    end
    )
end

(** A few code elements for environment changes from functions as remove_vars etc. have been moved to sharedFunctions as they are needed in a similar way inside affineEqualityDomain.
    A module that includes various methods used by variable handling operations such as add_vars, remove_vars etc. in apronDomain and affineEqualityDomain. *)
module Environment =
struct
  include Environment

  let pp: Format.formatter -> Environment.t -> unit = Environment.print
  include Printable.SimpleFormat (
    struct
      type nonrec t = t
      let pp = pp
    end
    )

  let compare (x: t) (y: t): int =
    (* TODO: implement total Environment order in OCaml *)
    failwith "Apron.Environment doesn't have total order" (* https://github.com/antoinemine/apron/issues/99 *)

  let ivars_only env =
    let ivs, fvs = Environment.vars env in
    assert (Array.length fvs = 0); (* shouldn't ever contain floats *)
    List.of_enum (Array.enum ivs)

  let add_vars env vs =
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> not (Environment.mem_var env v))
      |> Array.of_enum
    in
    Environment.add env vs' [||]

  let remove_vars env vs =
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    Environment.remove env vs'

  let remove_filter env f =
    let vs' =
      ivars_only env
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    Environment.remove env vs'

  let keep_vars env vs =
    (* Instead of iterating over all vars in env and doing a linear lookup in vs just to remove them,
        make a new env with just the desired vs. *)
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    Environment.make vs' [||]

  let keep_filter env f =
    (* Instead of removing undesired vars,
       make a new env with just the desired vars. *)
    let vs' =
      ivars_only env
      |> List.enum
      |> Enum.filter f
      |> Array.of_enum
    in
    Environment.make vs' [||]
end
