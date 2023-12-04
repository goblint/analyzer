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
