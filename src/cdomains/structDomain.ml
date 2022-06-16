open Cil
open GobConfig
open Pretty
open FlagHelper

(* Exception raised when the set domain can not support the requested operation.
 * This will be raised, when trying to iterate a set that has been set to Top *)
exception Unsupported of string

module type Arg =
sig
  include Lattice.S
  val is_bot_value: t -> bool
  val is_top_value: t -> typ -> bool
end

module type S =
sig
  include Lattice.S
  type value
  type field
  val create: (field -> value) -> compinfo -> t
  val get: t -> field -> value
  val replace: t -> field -> value -> t
  val fold: (field -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val map: (value -> value) -> t -> t
  val keys: t -> field list
  val widen_with_fct: (value -> value -> value) -> t -> t -> t
  val join_with_fct: (value -> value -> value) -> t -> t -> t
  val leq_with_fct: (value -> value -> bool) -> t -> t -> bool
  val invariant: value_invariant:(offset:Cil.offset -> Invariant.context -> value -> Invariant.t) -> offset:Cil.offset -> Invariant.context -> t -> Invariant.t
end

module Simple (Val: Arg) =
struct
  include Printable.Std
  module M = MapDomain.MapTop_LiftBot (Basetype.CilField) (Val)
  let name () = "simple structs"
  type t = M.t [@@deriving to_yojson]
  type field = fieldinfo
  type value = M.value

  (** Short summary for structs *)
  let show mapping =
    let assoclist = M.fold (fun x y rest -> (x,y)::rest) mapping [] in
    let f (key, st) = Val.show st in
    let whole_str_list = List.rev_map f assoclist in
    Printable.get_short_list "<" ">" whole_str_list

  let pretty () = M.pretty ()
  let replace s field value = M.add field value s
  let get s field = M.find field s
  let fold = M.fold
  let map = M.map
  let keys x = M.fold (fun k _ a -> k::a) x []

  (* Add these or the byte code will segfault ... *)
  let equal = M.equal
  let compare = M.compare
  let is_top = M.is_top
  let top () = M.top ()
  let create fn compinfo = List.fold_left (fun s fd -> replace s fd (fn fd)) (M.top ()) compinfo.cfields
  let is_bot = M.is_bot
  let bot () = M.bot ()
  let meet = M.meet
  let join = M.join
  let leq = M.leq
  let hash  = M.hash
  let widen = M.widen
  let narrow = M.narrow
  let pretty_diff () (x,y) =
    Pretty.dprintf "{@[%a@] ...}" M.pretty_diff (x,y)
  let printXml = M.printXml
  let widen_with_fct = M.widen_with_fct
  let leq_with_fct = M.leq_with_fct
  let join_with_fct = M.join_with_fct

  let for_all_fields f = M.for_all f

  let invariant ~value_invariant ~offset c x =
    match offset with
    (* invariants for all fields *)
    | NoOffset ->
      let c_lval = BatOption.get c.Invariant.lval in
      fold (fun f v acc ->
          let f_lval = Cil.addOffsetLval (Field (f, NoOffset)) c_lval in
          let f_c = {c with lval=Some f_lval} in
          let i = value_invariant ~offset f_c v in
          Invariant.(acc && i)
        ) x (Invariant.top ())
    (* invariant for one field *)
    | Field (f, offset) ->
      let v = get x f in
      value_invariant ~offset c v
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none
end

module SetsCommon (Val:Arg) =
struct
  include Printable.Std
  module SS = Simple (Val)
  module HS = HoareDomain.Set (SS)

  type field = fieldinfo
  type value = SS.value
  type variant = SS.t
  type set = HS.t

  let join_ss s =
    match HS.elements s with
    | [] -> raise (Unsupported "join_ss on an empty set")
    | h::t -> List.fold_left SS.join h t

  let on_joint_ss f default s =
    if HS.is_bot s then default else f (join_ss s)

  let hs_top () = HS.singleton (SS.top ())
  let hs_is_top = HS.exists SS.is_top
  let hs_bot () = HS.bot ()
  let hs_is_bot x = HS.is_bot x || HS.for_all (SS.for_all_fields (fun _ value -> Val.is_bot_value value)) x
  let hs_get s field = HS.fold (fun ss acc -> Val.join acc (SS.get ss field)) s (Val.bot ())
  let hs_create fn compinfo = HS.singleton (SS.create fn compinfo)
  let hs_fold f s a = on_joint_ss (fun ss -> SS.fold f ss a) a s
  let hs_keys = on_joint_ss (SS.keys) []
  let hs_map f s = if HS.is_bot s
    then s
    else HS.map (SS.map f) s
end

module Sets (Val: Arg) =
struct
  include SetsCommon(Val)

  let name () = "set structs"
  type t = HS.t [@@deriving to_yojson]

  let show mapping = HS.show mapping
  let pretty = HS.pretty
  let top = hs_top
  let is_top = hs_is_top
  let bot = hs_bot
  let is_bot = hs_is_bot
  let create = hs_create

  let replace s field value =
    if Messages.tracing then Messages.tracel "simplesets" "Normalize top Replace - s:\n%a\nfield:%a\nvalue: %a\n---------\n" HS.pretty s Basetype.CilField.pretty field Val.pretty value;
    HS.map (fun s -> SS.replace s field value) s

  let get = hs_get
  let fold = hs_fold
  let map = hs_map
  let keys = hs_keys
  let equal = HS.equal
  let compare = HS.compare

  let meet_narrow_common x y f =
    (*
    Normally, this would be just: HS.meet x y |> filter_variants_with_bot
    However, arrays set everything at start to bottom, and we would quickly have
    some things initialized to bottom, and discard them.
    That's why we take a more involved approach and only discard those variants
    that were not overlapped before (non-bottom meet && none of them was bottom before)
     *)
    let values_overlap (a: value) (b: value) =
      Val.is_bot_value a || Val.is_bot_value b || not (Val.is_bot_value (Val.meet a b))
    in
    let maps_overlap ssx ssy =
      let fields = SS.keys ssx in
      List.for_all (fun field -> values_overlap (SS.get ssx field) (SS.get ssy field)) fields
    in
    let x_list = HS.elements x in
    let y_list = HS.elements y in
    List.concat_map (fun xss -> List.map (fun yss -> (xss, yss)) y_list) x_list
    |> List.filter (fun (ssx, ssy) -> maps_overlap ssx ssy)
    |> List.map (fun (ssx, ssy) -> f ssx ssy)
    |> HS.of_list

  let meet x y = meet_narrow_common x y SS.meet
  let hash = HS.hash

  let narrow x y =
    meet_narrow_common x y (fun x y -> if SS.leq y x then SS.narrow x y else x)

  let pretty_diff () (x,y) =
    Pretty.dprintf "{@[%a@] ...}" HS.pretty_diff (x,y)
  let printXml f xs = HS.printXml f xs

  let widen_with_fct f =
    let product_widen op a b = (* assumes b to be bigger than a *) (* from HS.product_widen *)
      let xs,ys = HS.elements a, HS.elements b in
      List.concat_map (fun x -> List.map (fun y -> op x y) ys) xs |> fun x -> HS.of_list (List.append x ys)
    in
    product_widen (fun x y -> if SS.leq x y then (SS.widen_with_fct f) x y else SS.bot ())

  let widen = widen_with_fct Val.widen

  let leq_with_fct f a b =
    let mem x s f = HS.exists ((SS.leq_with_fct f) x) s in (* from HS.mem *)
    HS.for_all (fun x -> mem x b f) a

  let leq = leq_with_fct Val.leq

  let join_with_fct f x y =
    let appended = List.append (HS.elements x) (HS.elements y) in
    if Messages.tracing then Messages.tracel "simplesets-fct" "Join-fct start!\nx: %a\ny: %a\n" pretty x pretty y;
    let reduce_list_with_fct join_f xs s =
      let rec aux unique remaining =
        match remaining with
        | [] -> HS.of_list unique
        | h::t ->
          let (overlapping, rem_uniq) = List.partition (fun ss -> SS.leq h ss || SS.leq ss h ) unique in
          let joined = List.fold_left (fun el acc ->
              let res = join_f acc el in
              if Messages.tracing then Messages.tracel "simplesets-fct" "Join-fct joining others!\nacc: %a\nel: %a\nres: %a\n" SS.pretty acc SS.pretty el SS.pretty res;
              res
            ) h overlapping in
          aux (joined::rem_uniq) t
      in aux [] xs
    in
    let res = reduce_list_with_fct (SS.join_with_fct f) appended x in
    if Messages.tracing then Messages.tracel "simplesets-fct" "Join-fct result!\nx: %a\ny: %a\nconverted: %a\nres: %a\n" pretty x pretty y pretty (HS.of_list appended) pretty res;
    res

  let join = join_with_fct Val.join

  (* let invariant = HS.invariant *)
  let invariant ~value_invariant ~offset _ _ = Invariant.none (* TODO *)
end

module KeyedSets (Val: Arg) =
struct
  include SetsCommon(Val)
  module F = Basetype.CilField

  let name () = "keyed set structs"
  type t = HS.t * Basetype.CilField.t option [@@deriving to_yojson]

  let show (s, k) = match k with
    | Some k -> HS.show s ^ " with key " ^ F.show k
    | None -> HS.show s ^ " without key"

  let pretty () (s, k) = match k with
    | Some k -> (HS.pretty () s) ++ (text " with key ") ++ (F.pretty () k)
    | None -> (HS.pretty () s) ++ (text " without key")

  let top () = (hs_top (), None)
  let is_top (s, _) = hs_is_top s
  let bot () = (hs_bot (), None)
  let is_bot (s, _) = hs_is_bot s

  let keys (s,_) = hs_keys s

  let get_key_from_compinfo (info:compinfo): field option =
    let fields = info.cfields in
    if fields = []
    then None
    else
      let fields = if get_bool "ana.base.structs.key.forward" then fields else List.rev fields in
      let rec first_appropriate_key (rem_fields: field list) (second_choice: field): field =
        match rem_fields with
        | [] -> second_choice
        | h::t -> begin
            match (h.ftype, get_bool "ana.base.structs.key.prefer-ptrs", get_bool "ana.base.structs.key.avoid-ints") with
            | (TPtr (_, _), _, _) -> h
            | (TInt (_, _), true, _)
            | (TInt (_, _), _, true) -> first_appropriate_key t second_choice
            | (TInt (_, _), _, _) -> h
            | (_, false, _) -> h
            | (_, _, false) -> first_appropriate_key t second_choice
            | (_, _, _) ->
              let second = match second_choice.ftype with
                | TInt (_,_) -> h
                | _ -> second_choice
              in first_appropriate_key t second
          end
      in Some (first_appropriate_key fields (List.hd fields))

  let find_key_field (s,k): field option =
    match k with
    | Some k -> Some k
    | _ ->
      let existing_fields = keys (s,k) in
      if existing_fields = []
      then None
      else get_key_from_compinfo ((List.hd existing_fields).fcomp)


  let reduce_key_with_fct f ((s,k) as x): t =
    if HS.cardinal s <= 1 then x else
      match find_key_field x with
      | None -> x
      | Some key ->
        let rec aux unique remaining =
          match remaining with
          | [] -> HS.of_list unique
          | h::t ->
            let h_key = SS.get h key in
            let variant_overlaps ss =
              let v = SS.get ss key in
              Val.leq h_key v || Val.leq v h_key || not (Val.is_bot_value (Val.meet h_key v))
            in
            let (overlapping, rem_uniq) = List.partition variant_overlaps unique in
            let joined = List.fold_left (fun el acc -> f el acc) h overlapping in
            aux (joined::rem_uniq) t
        in
        let res = aux [] (HS.elements s) in
        if Messages.tracing then Messages.tracel "reduce-key" "Reduced - s:\n%a\nto:\n%a\n---------\n" HS.pretty s HS.pretty res;
        (res, Some key)

  let reduce_key (x: t): t = reduce_key_with_fct (SS.join) x

  let replace (s,k) field value : t =
    let join_set s =if HS.is_bot s then s else HS.singleton (join_ss s) in
    if Messages.tracing then Messages.tracel "keyedsets" "Replace - s:\n%a\nfield:%a\nvalue: %a\n---------\n" HS.pretty s Basetype.CilField.pretty field Val.pretty value ;
    let replaced = HS.map (fun s -> SS.replace s field value) s in
    let result_key =
      match find_key_field (s,k) with
      | None -> (replaced, None)
      | Some key ->
        if Basetype.CilField.equal key field
        then (join_set replaced, Some key) (* Key is now the same in all variants *)
        else (replaced, Some key)
    in
    result_key

  let create fn compinfo = (hs_create fn compinfo, get_key_from_compinfo compinfo)
  let get (s, _: t) = hs_get s

  let fold f (s, _: t) = hs_fold f s

  let map f (s,k) =
    let s' = hs_map f s in
    reduce_key (s', k)

  let equal (x,_) (y,_) = HS.equal x y
  let compare (x,_) (y,_) = HS.compare x y

  let take_some_key (k1: field option) (k2: field option) (s: set) =
    match (k1, k2) with
    | (Some(x), _) -> Some x
    | (_, Some(y)) -> Some y
    | _ -> find_key_field (s, None)

  let meet_narrow_common x y f =
    (*
    Normally, this would be just: HS.meet x y |> filter_variants_with_bot
    However, arrays set everything at start to bottom, and we would quickly have
    some things initialized to bottom, and discard them.
    That's why we take a more involved approach and only discard those variants
    that were not overlapped before (non-bottom meet && none of them was bottom before)
     *)
    begin
      let fields_overlap a b =
        Val.is_bot_value a || Val.is_bot_value b || not (Val.is_bot_value (Val.meet a b))
      in
      let maps_overlap ssx ssy =
        let fields = SS.keys ssx in
        List.for_all (fun field -> fields_overlap (SS.get ssx field) (SS.get ssy field)) fields
      in
      let ((sx, kx), (sy, ky)) = (x, y) in
      let x_list = HS.elements sx in
      let y_list = HS.elements sy in
      let s = List.concat_map (fun xss -> List.map (fun yss -> (xss, yss)) y_list) x_list
              |> List.filter (fun (ssx, ssy) -> maps_overlap ssx ssy)
              |> List.map (fun (ssx, ssy) -> f ssx ssy)
              |> HS.of_list
      in reduce_key (s, take_some_key kx ky s)
    end

  let meet x y = meet_narrow_common x y SS.meet

  let hash (s, _) = HS.hash s

  let narrow x y = meet_narrow_common x y (fun x y -> if SS.leq y x then SS.narrow x y else x)

  let pretty_diff () ((x, _), (y, _)) = Pretty.dprintf "{@[%a@] ...}" HS.pretty_diff (x, y)
  let printXml f x = match x with
    | (s, Some k) ->
      BatPrintf.fprintf f "<value>\n<map>\n
              <key>Keyed by</key>\n%a\n
              <key>Variants</key>\n%a\n\n
            </map></value>\n" F.printXml k HS.printXml s
    | (s,_) -> BatPrintf.fprintf f "<value>\n<map>\n
        <key>Keyed by</key>\n<value>No key</value>\n
        <key>Variants</key>\n%a\n\n
      </map></value>\n" HS.printXml s

  let widen_with_fct f (x, kx) (y, ky) =
    let product_widen op a b = (* assumes b to be bigger than a *) (* from HS.product_widen *)
      let xs,ys = HS.elements a, HS.elements b in
      List.concat_map (fun x -> List.map (op x) ys) xs |> fun x -> HS.of_list (List.append x ys)
    in
    let s = product_widen (fun x y -> if SS.leq x y then (SS.widen_with_fct f) x y else SS.bot ()) x y
    in reduce_key (s, take_some_key kx ky s)

  let widen = widen_with_fct Val.widen

  let leq_with_fct f (a, _) (b, _) =
    let mem x s f = HS.exists ((SS.leq_with_fct f) x) s in (* from HS.mem *)
    HS.for_all (fun x -> mem x b f) a

  let leq = leq_with_fct Val.leq

  let join_with_fct f (x, k) (y, _) =
    let appended = List.append (HS.elements x) (HS.elements y) in
    if Messages.tracing then Messages.tracel "bettersets" "Join-fct start!\nx: %a\ny: %a\n" HS.pretty x HS.pretty y;
    let reduce_list_key_with_fct join_f (xs: variant list) (x: t) =
      match find_key_field x with
      | None -> x
      | Some key ->
        let rec aux unique remaining =
          match remaining with
          | [] -> HS.of_list unique
          | h::t ->
            let h_key = SS.get h key in
            let (overlapping, rem_uniq) = List.partition (fun ss ->
                let ss_key = SS.get ss key in
                Val.leq h_key ss_key || Val.leq ss_key h_key || not (Val.is_bot_value (Val.meet h_key ss_key))
              ) unique in
            let joined = List.fold_left (fun el acc ->
                let res = join_f acc el in
                if Messages.tracing then Messages.tracel "bettersets" "Join-fct joining others!\nacc: %a\nel: %a\nres: %a\n" SS.pretty acc SS.pretty el SS.pretty res;
                res
              ) h overlapping in
            aux (joined::rem_uniq) t
        in (aux [] xs, Some key)
    in
    let res = reduce_list_key_with_fct (SS.join_with_fct f) appended (x,k) in
    if Messages.tracing then Messages.tracel "bettersets" "Join-fct result!\nx: %a\ny: %a\nconverted: %a\nres: %a\n" HS.pretty x HS.pretty y HS.pretty (HS.of_list appended) pretty res;
    res

  let join = join_with_fct Val.join

  (* let invariant c (x,_) = HS.invariant c x *)
  let invariant ~value_invariant ~offset _ _ = Invariant.none (* TODO *)
end


module FlagConfiguredStructDomain (Val: Arg) =
struct
  include Printable.Std
  module S = Simple(Val)
  module HS = Sets(Val)
  module KS = KeyedSets(Val)

  type field = fieldinfo
  type value = S.value

  module P = struct
    let msg = "FlagConfiguredStructDomain received a value where not exactly one component is set"
    let name = "FlagConfiguredStructDomain"
  end

  let of_t = function
    | (Some s, None) -> (Some s, None, None)
    | (None, Some (hs,ks)) -> (None, hs, ks)
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"

  let to_t = function
    | (Some s, None, None) -> (Some s, None)
    | (None, Some hs, None) -> (None, Some (Some hs, None))
    | (None, None, Some ks) -> (None, Some (None, Some ks))
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"


  module I = struct include LatticeFlagHelper (HS) (KS) (P) let name () = "" end
  include LatticeFlagHelper (S) (I) (P)

  let invariant ~value_invariant ~offset c = unop (S.invariant ~value_invariant ~offset c) (I.unop (HS.invariant ~value_invariant ~offset c) (KS.invariant ~value_invariant ~offset c))

  let twoaccop_to_t ops ophs opks (s,r) a1 a2 = to_t @@ match of_t (s,r) with
    | (Some s, None, None) -> (Some (ops s a1 a2), None, None)
    | (None, Some hs, None) -> (None, Some(ophs hs a1 a2), None)
    | (None, None, Some ks) -> (None, None, Some(opks ks a1 a2))
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"

  let binop' ops ophs opks = binop ops (I.binop ophs opks)
  let unop' ops ophs opks = unop ops (I.unop ophs opks)
  let binop_to_t' ops ophs opks = binop_to_t ops (I.binop_to_t ophs opks)
  let unop_to_t' ops ophs opks = unop_to_t ops (I.unop_to_t ophs opks)

  let leq_with_fct f = binop' (S.leq_with_fct f) (HS.leq_with_fct f) (KS.leq_with_fct f)
  let join_with_fct f = binop_to_t' (S.join_with_fct f) (HS.join_with_fct f) (KS.join_with_fct f)
  let widen_with_fct f = binop_to_t' (S.widen_with_fct f) (HS.widen_with_fct f) (KS.widen_with_fct f)
  let get = unop' S.get HS.get KS.get
  let replace = twoaccop_to_t S.replace HS.replace KS.replace
  let keys = unop' S.keys HS.keys KS.keys
  let map f = unop_to_t' (S.map f) (HS.map f) (KS.map f)
  let fold f = unop' (S.fold f) (HS.fold f) (KS.fold f)

  (* Functions that make us of the configuration flag *)
  let chosen_domain () = get_string "ana.base.structs.domain"

  let pick_combined setting (comp: compinfo) =
    let all_bool () = List.for_all (fun f -> match f.ftype with TInt(IBool, _) -> true | _ -> false) comp.cfields in
    let has_ptr () = List.exists (fun f -> match f.ftype with TPtr(_, _) -> true | _ -> false) comp.cfields in
    match setting with
    | "combined-sk" -> if has_ptr () then "keyed" else "simple"
    | "combined-all" ->
      if all_bool () then "sets"
      else if has_ptr () then "keyed"
      else "simple"
    | _ -> setting

  let name () = "FlagConfiguredStructDomain: " ^ match chosen_domain () with
    | "simple" -> S.name ()
    | "sets" -> HS.name ()
    | "keyed" -> KS.name ()
    | "combined-all" -> "simple/sets/keyed structs"
    | "combined-sk" -> "simple/keyed structs"
    | _ -> failwith "FlagConfiguredStructDomain cannot name a struct from set option"

  let bot () =
    to_t @@ match chosen_domain () with
    | "simple" -> (Some (S.bot ()), None, None)
    | "sets" -> (None, Some (HS.bot ()), None)
    | "keyed" -> (None, None, Some (KS.bot ()))
    | _ -> failwith "FlagConfiguredStructDomain cannot construct a bot struct from set option"

  let top () =
    to_t @@ match chosen_domain () with
    | "simple" -> (Some (S.top ()), None, None)
    | "sets" -> (None, Some (HS.top ()), None)
    | "keyed" -> (None, None, Some (KS.top ()))
    | _ -> failwith "FlagConfiguredStructDomain cannot construct a top struct from set option"

  let create fn (comp: compinfo): t =
    to_t @@ match pick_combined (chosen_domain ()) comp with
    | "simple" -> (Some (S.create fn comp), None, None)
    | "sets" -> (None, Some (HS.create fn comp), None)
    | "keyed" -> (None, None, Some (KS.create fn comp))
    | _ -> failwith "FlagConfiguredStructDomain cannot construct a struct from set option"

end
