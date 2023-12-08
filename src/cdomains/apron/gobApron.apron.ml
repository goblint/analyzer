open Batteries
include Apron

module Var =
struct
  include Var
  let equal x y = Var.compare x y = 0
end

module Lincons1 =
struct
  include Lincons1

  let show = Format.asprintf "%a" print
  let compare x y = String.compare (show x) (show y) (* HACK *)

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

(** A few code elements for environment changes from functions as remove_vars etc. have been moved to sharedFunctions as they are needed in a similar way inside affineEqualityDomain.
    A module that includes various methods used by variable handling operations such as add_vars, remove_vars etc. in apronDomain and affineEqualityDomain. *)
module Environment =
struct
  include Environment

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
