open Cil
open GobConfig
open Pretty

(* Exception raised when the set domain can not support the requested operation.
 * This will be raised, when trying to iterate a set that has been set to Top *)
exception Unsupported of string

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
end

module type LatticeWithIsTopBotValue =
sig
  include Lattice.S
  val is_bot_value: t -> bool
  val is_top_value: t -> typ -> bool
end

module Simple (Val: Lattice.S) =
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

  let invariant c x =
    match c.Invariant.offset with
    (* invariants for all fields *)
    | NoOffset ->
      let c_lval = BatOption.get c.Invariant.lval in
      fold (fun f v acc ->
          let f_lval = Cil.addOffsetLval (Field (f, NoOffset)) c_lval in
          let f_c = {c with lval=Some f_lval} in
          let i = Val.invariant f_c v in
          Invariant.(acc && i)
        ) x Invariant.none
    (* invariant for one field *)
    | Field (f, offset) ->
      let f_c = {c with offset} in
      let v = get x f in
      Val.invariant f_c v
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none
end

module Sets (Val: LatticeWithIsTopBotValue) =
struct
  include Printable.Std
  module SS = Simple (Val)
  module HS = HoareDomain.Set (SS)

  let name () = "set structs"
  type t = HS.t [@@deriving to_yojson]
  type field = fieldinfo
  type value = SS.value

  let show mapping = HS.show mapping

  let pretty = HS.pretty

  let for_all_fields f ss = SS.fold (fun field value acc -> acc && (f field value)) ss true
  let all_fields_bot = for_all_fields (fun _ value -> Val.is_bot_value value)

  let top () = HS.singleton (SS.top ())
  let is_top = HS.exists SS.is_top
  let bot = HS.bot
  let is_bot x = HS.is_bot x || HS.for_all all_fields_bot x
  let create fn compinfo = HS.singleton (SS.create fn compinfo)

  let replace s field value =
    if Messages.tracing then Messages.tracel "simplesets" "Normalize top Replace - s:\n%a\nfield:%a\nvalue: %a\n---------\n" HS.pretty s Basetype.CilField.pretty field Val.pretty value;
    HS.map (fun s -> SS.replace s field value) s

  let get s field =
    HS.fold (fun ss acc -> Val.join acc (SS.get ss field)) s (Val.bot ())

  let join_ss s =
    match HS.elements s with
    | [] -> raise (Unsupported "join_ss on an empty set")
    | h::t -> List.fold_left SS.join h t

  let on_joint_ss f default s =
    if HS.is_bot s then default else f (join_ss s)

  let fold f s a = on_joint_ss (fun ss -> SS.fold f ss a) a s

  let map f s =
    if HS.is_bot s
    then s
    else HS.singleton (on_joint_ss (SS.map f) (SS.bot ()) s)

  let keys = on_joint_ss (SS.keys) []

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
    List.map (fun xss -> List.map (fun yss -> (xss, yss)) y_list) x_list
    |> List.flatten
    |> List.filter (fun (ssx, ssy) -> maps_overlap ssx ssy)
    |> List.map (fun (ssx, ssy) -> f ssx ssy)
    |> HS.of_list

  let meet x y = meet_narrow_common x y SS.meet

  let join = HS.join
  let leq = HS.leq
  let hash = HS.hash
  let widen = HS.widen

  let narrow x y =
    meet_narrow_common x y (fun x y -> if SS.leq y x then SS.narrow x y else x)

  let pretty_diff () (x,y) =
    Pretty.dprintf "{@[%a@] ...}" HS.pretty_diff (x,y)
  let printXml f xs = HS.printXml f xs

  let widen_with_fct f =
    let product_widen op a b = (* assumes b to be bigger than a *) (* from HS.product_widen *)
      let xs,ys = HS.elements a, HS.elements b in
      List.map (fun x -> List.map (fun y -> op x y) ys) xs |> List.flatten |> fun x -> HS.of_list (List.append x ys)
    in
    product_widen (fun x y -> if SS.leq x y then (SS.widen_with_fct f) x y else SS.bot ())

  let leq_with_fct f a b =
    let mem x s f = HS.exists ((SS.leq_with_fct f) x) s in (* from HS.mem *)
    HS.for_all (fun x -> mem x b f) a

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

  let invariant = HS.invariant
end

module KeyedSets (Val: LatticeWithIsTopBotValue) =
struct
  include Printable.Std
  module SS = Simple (Val)
  module HS = HoareDomain.Set (SS)
  module F = Basetype.CilField

  let name () = "keyed set structs"
  type field = fieldinfo
  type value = SS.value
  type variant = SS.t
  type set = HS.t
  type t = HS.t * Basetype.CilField.t option [@@deriving to_yojson]

  let show (s, k) = match k with
    | Some k -> HS.show s ^ " with key " ^ F.show k
    | None -> HS.show s ^ " without key"

  let pretty () (s, k) = match k with
    | Some k -> (HS.pretty () s) ++ (text " with key ") ++ (F.pretty () k)
    | None -> (HS.pretty () s) ++ (text " without key")

  let for_all_fields f ss = SS.fold (fun field value acc -> acc && (f field value)) ss true
  let all_fields_bot = for_all_fields (fun _ value -> Val.is_bot_value value)

  let top () = (HS.singleton (SS.top ()), None)
  let is_top (s, _) = HS.exists SS.is_top s
  let bot () = (HS.bot (), None)
  let is_bot (s, _) = HS.is_bot s || HS.for_all all_fields_bot s
  let create fn compinfo = (HS.singleton (SS.create fn compinfo), None)

  let join_ss (s: set): variant =
    match HS.elements s with
    | [] -> raise (Unsupported "join_ss on an empty set")
    | h::t -> List.fold_left (fun el acc -> SS.join el acc) h t

  let on_joint_ss f default (s: set) =
    if HS.is_bot s then default else f (join_ss s)

  let join_set (s: set): set = if HS.is_bot s then s else HS.singleton (join_ss s)

  let keys (s,_): field list = on_joint_ss (SS.keys) [] s

  let get_key_from_compinfo (info:compinfo): field option =
    let fields = info.cfields in
    if fields = []
    then None
    else
      let fields = if get_bool "exp.structs.key.forward" then fields else List.rev fields in
      let rec first_appropriate_key (rem_fields: field list) (second_choice: field): field =
        match rem_fields with
        | [] -> second_choice
        | h::t -> begin
            match (h.ftype, get_bool "exp.structs.key.prefer-ptrs", get_bool "exp.structs.key.avoid-ints") with
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

  let find_key_field (x: t): field option =
    match x with
    | (_, Some k) -> Some k
    | (s, _) ->
      let existing_fields = keys x in
      if existing_fields = []
      then None
      else get_key_from_compinfo ((List.hd existing_fields).fcomp)


  let reduce_key_with_fct f (x: t): t =
    let (s, _) = x in
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

  let replace (x: t) field value : t =
    let (s, _) = x in
    if Messages.tracing then Messages.tracel "keyedsets" "Replace - s:\n%a\nfield:%a\nvalue: %a\n---------\n" HS.pretty s Basetype.CilField.pretty field Val.pretty value ;
    let replaced = HS.map (fun s -> SS.replace s field value) s in
    let result_key =
      match find_key_field x with
      | None -> (replaced, None)
      | Some key ->
        if Basetype.CilField.equal key field
        then (join_set replaced, Some key) (* Key is now the same in all variants *)
        else (replaced, Some key)
    in
    result_key

  let get (s, _: t) field =
    HS.fold (fun ss acc -> Val.join acc (SS.get ss field)) s (Val.bot ())

  let fold f (s, _: t) a = on_joint_ss (fun ss -> SS.fold f ss a) a s

  let map f (x: t) =
    let (s, k) = x in
    if HS.is_bot s
    then x
    else
      let s' = HS.singleton (on_joint_ss (SS.map f) (SS.top ()) s) in
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
      let s = List.map (fun xss -> List.map (fun yss -> (xss, yss)) y_list) x_list
              |> List.flatten
              |> List.filter (fun (ssx, ssy) -> maps_overlap ssx ssy)
              |> List.map (fun (ssx, ssy) -> f ssx ssy)
              |> HS.of_list
      in reduce_key (s, take_some_key kx ky s)
    end

  let meet x y = meet_narrow_common x y SS.meet

  let join (x, kx) (y, ky) = let s = HS.join x y in reduce_key (s, take_some_key kx ky s)
  let leq (x, _) (y, _) = HS.leq x y

  let hash (s, _) = HS.hash s
  let widen (x, kx) (y, ky) =
    let s = HS.widen x y in
    reduce_key (s, take_some_key kx ky s)

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
      List.map (fun x -> List.map (op x) ys) xs |> List.flatten |> fun x -> HS.of_list (List.append x ys)
    in
    let s = product_widen (fun x y -> if SS.leq x y then (SS.widen_with_fct f) x y else SS.bot ()) x y
    in reduce_key (s, take_some_key kx ky s)

  let leq_with_fct f (a, _) (b, _) =
    let mem x s f = HS.exists ((SS.leq_with_fct f) x) s in (* from HS.mem *)
    HS.for_all (fun x -> mem x b f) a

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

  let invariant c (x,_) = HS.invariant c x
end


module FlagConfiguredStructDomain (Val: LatticeWithIsTopBotValue) =
struct
  include Printable.Std
  module S = Simple(Val)
  module HS = Sets(Val)
  module KS = KeyedSets(Val)

  type field = fieldinfo
  type value = S.value
  type t = S.t option * HS.t option * KS.t option [@@deriving to_yojson]

  let tag _ = failwith "FlagConfiguredStructDomain: no tag"
  let arbitrary () = failwith "FlagConfiguredStructDomain: no arbitrary"

  let unop ops ophs opks (s,hs,ks) = match (s, hs, ks) with
    | (Some s, None, None) -> ops s
    | (None, Some hs, None) -> ophs hs
    | (None, None, Some ks) -> opks ks
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"

  let invariant c = unop (S.invariant c) (HS.invariant c) (KS.invariant c)

  let pretty () = unop (S.pretty ()) (HS.pretty ()) (KS.pretty ())

  (* Helpers *)
  let binop ops ophs opks (s1,hs1,ks1) (s2,hs2,ks2) = match (s1, hs1, ks1), (s2, hs2, ks2) with
    | (Some s1, None, None), (Some s2, None, None) -> ops s1 s2
    | (None, Some hs1, None), (None, Some hs2, None) -> ophs hs1 hs2
    | (None, None, Some ks1), (None, None, Some ks2) -> opks ks1 ks2
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"

  let binop_to_t ops ophs opks (s1,hs1,ks1) (s2,hs2,ks2)= match (s1, hs1, ks1),(s2, hs2, ks2) with
    | (Some s1, None, None), (Some s2, None, None) -> (Some (ops s1 s2), None, None)
    | (None, Some hs1, None), (None, Some hs2, None) -> (None, Some(ophs hs1 hs2), None)
    | (None, None, Some ks1), (None, None, Some ks2) -> (None, None, Some(opks ks1 ks2))
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"


  let unop_to_t ops ophs opks (s,hs,ks) = match (s, hs, ks) with
    | (Some s, None, None) -> (Some (ops s), None, None)
    | (None, Some hs, None) -> (None, Some(ophs hs), None)
    | (None, None, Some ks) -> (None, None, Some(opks ks))
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"

  let twoaccop_to_t ops ophs opks (s,hs,ks) a1 a2 = match (s, hs, ks) with
    | (Some s, None, None) -> (Some (ops s a1 a2), None, None)
    | (None, Some hs, None) -> (None, Some(ophs hs a1 a2), None)
    | (None, None, Some ks) -> (None, None, Some(opks ks a1 a2))
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"

  (* Simply call appropriate function for component that is not None *)
  let equal = binop S.equal HS.equal KS.equal
  let hash = unop S.hash HS.hash KS.hash
  let compare = binop S.compare HS.compare KS.compare
  let show = unop S.show HS.show KS.show
  let leq = binop S.leq HS.leq KS.leq
  let leq_with_fct f = binop (S.leq_with_fct f) (HS.leq_with_fct f) (KS.leq_with_fct f)
  let join = binop_to_t S.join HS.join KS.join
  let join_with_fct f = binop_to_t (S.join_with_fct f) (HS.join_with_fct f) (KS.join_with_fct f)
  let meet = binop_to_t S.meet HS.meet KS.meet
  let widen = binop_to_t S.widen HS.widen KS.widen
  let widen_with_fct f = binop_to_t (S.widen_with_fct f) (HS.widen_with_fct f) (KS.widen_with_fct f)
  let narrow = binop_to_t S.narrow HS.narrow KS.narrow
  let is_top = unop S.is_top HS.is_top KS.is_top
  let is_bot = unop S.is_bot HS.is_bot KS.is_bot
  let get = unop S.get HS.get KS.get
  let replace = twoaccop_to_t S.replace HS.replace KS.replace
  let keys = unop S.keys HS.keys KS.keys
  let map f = unop_to_t (S.map f) (HS.map f) (KS.map f)
  let fold f = unop (S.fold f) (HS.fold f) (KS.fold f)
  let printXml f = unop (S.printXml f) (HS.printXml f) (KS.printXml f)
  let to_yojson = unop (S.to_yojson) (HS.to_yojson) (KS.to_yojson)
  let pretty_diff () ((s1,hs1,ks1),(s2,hs2,ks2)) = match (s1, hs1, ks1),(s2, hs2, ks2) with
    | (Some s1, None, None), (Some s2, None, None) -> S.pretty_diff () (s1, s2)
    | (None, Some hs1, None), (None, Some hs2, None) -> HS.pretty_diff () (hs1, hs2)
    | (None, None, Some ks1), (None, None, Some ks2) -> KS.pretty_diff () (ks1, ks2)
    | _ -> failwith "FlagConfiguredStructDomain received a value where not exactly one component is set"

  (* Functions that make us of the configuration flag *)
  let chosen_domain () = get_string "exp.structs.domain"

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
    match chosen_domain () with
    | "simple" -> (Some (S.bot ()), None, None)
    | "sets" -> (None, Some (HS.bot ()), None)
    | "keyed" -> (None, None, Some (KS.bot ()))
    | _ -> failwith "FlagConfiguredStructDomain cannot construct a bot struct from set option"

  let top () =
    match chosen_domain () with
    | "simple" -> (Some (S.top ()), None, None)
    | "sets" -> (None, Some (HS.top ()), None)
    | "keyed" -> (None, None, Some (KS.top ()))
    | _ -> failwith "FlagConfiguredStructDomain cannot construct a top struct from set option"

  let create fn (comp: compinfo): t =
    match pick_combined (chosen_domain ()) comp with
    | "simple" -> (Some (S.create fn comp), None, None)
    | "sets" -> (None, Some (HS.create fn comp), None)
    | "keyed" -> (None, None, Some (KS.create fn comp))
    | _ -> failwith "FlagConfiguredStructDomain cannot construct a struct from set option"

end
