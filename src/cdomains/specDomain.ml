open Cil
open Batteries

include LvalMapDomain

module Val =
struct
  type s = string
  let name = "Spec value"
  let var_state = ""
  let string_of_state s = s

  (* transforms May-Sets of length 1 to Must. NOTE: this should only be done if the original set had more than one element! *)
  (* let maybe_must = function May xs when Set.cardinal xs = 1 -> Must (Set.choose xs) | x -> x *)
  (* let may = function Must x -> May (Set.singleton x) | xs -> xs *)
  (* let records = function Must x -> (Set.singleton x) | May xs -> xs *)
  (* let list_of_records = function Must x -> [x] | May xs -> List.of_enum (Set.enum xs) *)
  (* let vnames x = String.concat ", " (List.map (fun r -> string_of_key r.var) (list_of_records x)) *)
end


(* module Dom = Domain (Value (Val)) *)

module Dom =
struct
  include Domain (Value (Val))

  (* extend here *)
end
