open Apron

module ApronImplementation = struct
  (* Oct *)
  type t = Oct.t
  let to_oct = Oct.Abstract1.to_oct
  let of_oct = Oct.Abstract1.of_oct
  let widening_thresholds = Oct.widening_thresholds
  let substitute_texpr_with = Abstract1.substitute_texpr_with
  let manager_is_oct = Oct.manager_is_oct
  let manager_to_oct = Oct.manager_to_oct
  let narrowing = Oct.narrowing
  let manager_alloc = Oct.manager_alloc

  (* Poly *)
  type pt = Polka.loose Polka.t
  let manager_alloc_loose = Polka.manager_alloc_loose

  (* Other *)
  let hash mgr x = Abstract1.hash mgr x
  let impl () = "Apron"
  let bound_texpr mgr _ d texpr1 = Abstract1.bound_texpr mgr d texpr1
end