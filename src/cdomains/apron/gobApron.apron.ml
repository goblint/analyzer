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

  let pp = print
  include Printable.SimpleFormat (
    struct
      type nonrec t = t
      let pp = pp
    end
    )

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

module Linexpr0 =
struct
  include Linexpr0

  (** Negate linear expression. *)
  let neg (linexpr0: t): t =
    let r = copy linexpr0 in
    Linexpr0.iter (fun c i ->
        Linexpr0.set_coeff r i (Coeff.neg c)
      ) linexpr0;
    Linexpr0.set_cst r (Coeff.neg (Linexpr0.get_cst linexpr0));
    r
end

module Linexpr1 =
struct
  include Linexpr1

  (** Negate linear expression. *)
  let neg (linexpr1: t): t =
    {linexpr0 = Linexpr0.neg linexpr1.linexpr0; env = linexpr1.env}
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

  (** Flip comparison operator in linear constraint, i.e., swap sides. *)
  let flip (lincons1: t): t =
    (* Apron constraints have rhs 0 and inequality only in one direction, so do the following: *)
    (* e >= 0  ->  e <= 0  ->  -e >= 0 *)
    make (Linexpr1.neg (get_linexpr1 lincons1)) (get_typ lincons1)
end

module Lincons1Set =
struct
  include Set.Make (Lincons1)

  let of_earray ({lincons0_array; array_env}: Lincons1.earray): t =
    Array.to_seq lincons0_array
    |> Seq.map (fun (lincons0: Lincons0.t) -> Lincons1.{lincons0; env = array_env})
    |> of_seq

  (** Simplify (octagon) constraint set to replace two {!SUPEQ}-s with single {!EQ}. *)
  let simplify (lincons1s: t): t =
    fold (fun lincons1 acc ->
        match Lincons1.get_typ lincons1 with
        | SUPEQ ->
          let flipped = Lincons1.flip lincons1 in
          if mem flipped lincons1s then (
            if Lincons1.compare lincons1 flipped < 0 then (
              Lincons1.set_typ flipped EQ; (* reuse flipped copy for equality *)
              add flipped acc
            )
            else
              acc
          )
          else
            add lincons1 acc
        | _ ->
          add lincons1 acc
      ) lincons1s empty
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
    Array.to_list ivs

  let add_vars env vs =
    let vs' =
      vs
      |> List.to_seq
      |> Seq.filter (fun v -> not (Environment.mem_var env v))
      |> Array.of_seq
    in
    Environment.add env vs' [||]

  let remove_vars env vs =
    let vs' =
      vs
      |> List.to_seq
      |> Seq.filter (fun v -> Environment.mem_var env v)
      |> Array.of_seq
    in
    Environment.remove env vs'

  let remove_filter env f =
    let vs' =
      ivars_only env
      |> List.to_seq
      |> Seq.filter f
      |> Array.of_seq
    in
    Environment.remove env vs'

  let keep_vars env vs =
    (* Instead of iterating over all vars in env and doing a linear lookup in vs just to remove them,
        make a new env with just the desired vs. *)
    let vs' =
      vs
      |> List.to_seq
      |> Seq.filter (fun v -> Environment.mem_var env v)
      |> Array.of_seq
    in
    Environment.make vs' [||]

  let keep_filter env f =
    (* Instead of removing undesired vars,
       make a new env with just the desired vars. *)
    let vs' =
      ivars_only env
      |> List.to_seq
      |> Seq.filter f
      |> Array.of_seq
    in
    Environment.make vs' [||]
end
