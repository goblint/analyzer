module B = Basetype
module BV = B.Variables
module OPT = BatOption
module BI = IntOps.BigIntOps
open OctagonDomain

let min_int = INV.top () |> INV.minimal |> BatOption.get
let max_int = INV.top () |> INV.maximal |> BatOption.get

let cast varinfo inv =
  let get_ikind varinfo =
    match varinfo.Cil.vtype with
    | Cil.TInt (ikind, _) -> Some ikind
    | _ -> None
  in
  match get_ikind varinfo with
  | Some ikind -> inv
  | None -> inv

module MatrixCache = struct
  module L = (Lattice.Prod (Lattice.Fake(BV)) (IntDomain.Booleans))
  include Hashtbl.Make (Lattice.Prod (L) (L))
end


let ikind = OctagonDomain.IKind.ikind
module Liszt (B: Lattice.S) =
struct
  include Lattice.Liszt (B)

  let rec map2 keep f x y =
    let concat elt ls = if keep then elt::ls else ls in
    match x, y with
    | [], [] -> []
    | hd::tl, [] | [], hd::tl ->
      concat hd (map2 keep f [] tl)
    | xh :: xs, yh :: ys when (B.compare xh yh) = 0 ->
      let res = f xh yh in
      if B.is_top res then
        (map2 keep f xs ys)
      else
        res :: (map2 keep f xs ys)
    | xh :: xs, yh :: ys ->
      if B.compare xh yh < 0 then
        concat xh (map2 keep f xs y)
      else
        concat yh (map2 keep f x ys)

  (* on meets we want to preserve the value of one octagon if *)
  (* the other octagon does not have a value at that position *)
  (* on joins we don't *)
  let meet a b = map2 true B.meet a b
  let join a b = map2 false B.join a b
  let narrow a b = map2 true B.narrow a b
  let widen a b = map2 false B.widen a b

  let rec filter f x =
    match x with
    | [] -> []
    | hd :: tl ->
      if f x then
        hd :: (filter f tl)
      else
        filter f tl

  let rec leq x y =
    match x, y with
    | _, [] -> true (* x has additional constraints (x leq y for all other constraints) => x leq y  *)
    | [], _ -> false (* y has additional constraints (x leq y for all other constraints) => not (x leq y) *)
    | x :: xs, y :: ys
      when (B.compare x y) = 0 -> (* Compare zero here if constraints are about the same var and sign *)
      B.leq x y && leq xs ys
    | _ :: xs, y ->
      leq xs y
end

module ConstraintType = struct
  type side = Upper | Lower | UpperAndLower

  let opposite s =
    if s = Upper then
      Lower
    else if s = Lower then
      Upper
    else
      s

  let plus = true
  let minus = false
end

module CT = ConstraintType

module type S =
sig
  include Lattice.S
  type key
  val set_constraint  : key * (bool * key) option * CT.side * BI.t -> t -> t
  val adjust          : key -> BI.t -> t -> t
  val erase           : key -> t -> t
  val projection      : key -> (bool * key) option -> t -> INV.t
  val strong_closure  : t -> t
  val map_to_matrix   : t -> elt array array * (BV.t, int) Hashtbl.t
  val matrix_to_map   : elt array array -> (BV.t, int) Hashtbl.t -> t
  val get_relation    : Deriving.Cil.varinfo -> Deriving.Cil.varinfo -> t -> OctagonDomain.INV.t option * OctagonDomain.INV.t option * bool
  val keep_only       : Deriving.Cil.varinfo list -> t -> t
  (* TODO: Currently last bool indicates if it was necessary to switch the order of vars and therefore multiplying diff by -1 in consumers may be necessary. *)
  (* This is ugly and needs to be fixed *)
end


module E = struct
  module F = Lattice.Fake(BV)
  include Lattice.Prod3 (IntDomain.Booleans) (F) (INV)

  let compare (lsign, lvar, _) (rsign, rvar, _) =
    let cmp = BV.compare lvar rvar in
    if cmp <> 0 then
      cmp
    else
      -(IntDomain.Booleans.compare lsign rsign)

  let leq (lsign, lvar, linv) (rsign, rvar, rinv) =
    lsign = rsign && BV.equal lvar rvar && INV.leq linv rinv

  let is_top (_, _, inv) = INV.is_top_of (ikind ()) inv
end


module VD = Lattice.Prod (INV) (Liszt(E))
module MapOctagon : S
  with type key = BV.t
= struct
  include MapDomain.MapTop (BV) (VD)

  let ikind = OctagonDomain.IKind.ikind

  (* TODO: choose ikind dynamically *)
  let print_oct oct =
    Prelude.Ana.sprint pretty oct

  let rec set_constraint_list (sign, v, side, (value: INV.int_t) ) ls =   (* Idea: Not set constraints that are [MinInt,MaxInt] those don't mean shit *)
    let inv =
      if side = CT.Upper then
        INV.ending oct_ik value
      else if side = CT.Lower then
        INV.starting oct_ik value
      else
        INV.of_interval oct_ik (value, value)
    in
    (* let inv = cast v inv in *) (* this does nothing, why do we have it? *)
    let construct_inv old_inv =
      if side = CT.UpperAndLower then
        INV.of_interval oct_ik (value, value)
      else
        let old_inv = if INV.is_bot old_inv then INV.top () else old_inv in
        let old_lower = INV.minimal old_inv |> OPT.get in
        let old_upper = INV.maximal old_inv |> OPT.get in
        if side = CT.Upper then
          let res = INV.of_interval oct_ik (old_lower, min old_upper value) in    (* When both setting lower and upper after each other                                                *)
          if INV.is_bot res then                                           (* from e.g. [1,3] to [5,6] the result would be [5,3] i.e. bot after setting the lower boundary only *)
            INV.ending oct_ik value
          else
            res
        else
          let res = INV.of_interval oct_ik (max old_lower value, old_upper) in
          if INV.is_bot res then
            INV.starting oct_ik value
          else
            res
    in
    match ls with
    | ((sign2, v2, inv2) as x) :: xs ->
      let cmp = E.compare (sign, v, value) x in
      if cmp = 0 then
        begin
          let inv = construct_inv inv2 in
          if INV.is_top_of (ikind ()) inv then
            xs
          else
            (sign, v, inv) :: xs
        end
      else if cmp < 0 then
        if  INV.is_top_of (ikind ()) inv then
          ls (* no prexisting constraint on these two vars -> adding top is pointless *)
        else
          (sign, v, inv) :: ls
      else
        x :: (set_constraint_list (sign, v, side, value) xs)
    | [] ->
      if INV.is_top_of (ikind ()) inv then
        [] (* no prexisting constraint on these two vars -> adding top is pointless *)
      else
        [(sign, v, inv)]

  let add_var var oct =
    if mem var oct then
      oct
    else
      add var (INV.top(), []) oct

  let rec delete_constraint (sign, v) ls =
    match ls with
    | ((sign2, v2, _) as x) :: xs ->
      let cmp = E.compare (sign,v, INV.top ()) x in
      if cmp = 0 then
        xs
      else if cmp < 0 then
        ls
      else
        x :: (delete_constraint (sign, v) xs)
    | [] -> []

  (* return sum, diff constraint for this variable *)
  let find_constraints var ls =
    let rec find_constraints first ls =
      match ls with
      | (sign, v, inv) :: xs ->
        let cmp = BV.compare var v in
        if cmp = 0 then
          if sign = ConstraintType.plus then
            find_constraints (Some inv) xs
          else
            first, (Some inv)
        else if cmp > 0 then
          find_constraints first xs
        else
          first, None
      | [] -> first, None
    in
    find_constraints None ls

  let find x oct =
    try
      find x oct
    with Lattice.Unsupported _ ->
      raise Not_found

  let rec get_relation i j oct =
    if BV.compare i j = 1 then
      begin
        let sum,diff,_ = get_relation j i oct in
        sum,diff,true
      end
    else try
        let _, l = find i oct in
        let summ, diff = find_constraints j l in
        summ,diff, false
      with Not_found ->
        None, None, false

  let get_interval i oct =
    try
      let (inv, _) = find i oct in
      Some inv
    with Not_found ->
      None

  let print_inv = function
    | None -> print_endline "None"
    | Some i -> print_endline (INV.short 0 i)

  let rec set_constraint const oct =
    match const with
    | var, None, side, value ->
      let oct = add_var var oct in
      let old_inv, consts = find var oct in
      let old_inv = if INV.is_bot old_inv then INV.top () else old_inv in (* TODO: why would it be \bot? *)
      let new_inv =
        if side = CT.Upper then
          let res = INV.of_interval oct_ik (OPT.get (INV.minimal old_inv), value) in
          if INV.is_bot res then
            INV.ending oct_ik value
          else
            res
        else if side = CT.Lower then
          let res = INV.of_interval oct_ik (value, OPT.get (INV.maximal old_inv)) in
          if INV.is_bot res then
            INV.starting oct_ik value
          else
            res
        else
          INV.of_interval oct_ik (value, value)
      in
      add var (cast var new_inv, consts) oct
    | var1, Some (sign, var2), side, value ->
      let cmp = (BV.compare var1 var2) in
      if cmp = 0 then
        Lattice.unsupported "Can't set constraint between variable and itself"
      else if cmp > 0 then
        let side, value =
          if sign = CT.plus then
            side, value
          else
            CT.opposite side, BI.neg value
        in
        set_constraint (var2, Some (sign, var1), side, value) oct
      else begin
        let oct = add_var var1 (add_var var2 oct) in
        let (const, consts) = find var1 oct in
        let consts = set_constraint_list (sign, var2, side, value) consts in
        add var1 (const, consts) oct
      end

  let myadd inv c =                                 (* this looks like it is the same as INV.add inv (INV.of_int c)  ? *)
    let lower = INV.minimal inv |> OPT.get in
    let upper = INV.maximal inv |> OPT.get in
    let lower = max (BI.add lower c) min_int in
    let upper = min (BI.add upper c) max_int in
    INV.of_interval oct_ik (lower, upper)

  (* deals with incrementing var by constant `value` *)
  let adjust var value oct =
    try
      let const, consts = find var oct in
      let const = myadd const value in
      let consts = List.map
          (fun (sign, var2, old_val) ->
             sign, var2, (myadd old_val value)) consts in
      let oct = add var (const, consts) oct in

      map (fun (a, consts) ->
          (a, List.map (fun (sign, var2, old_val) ->
              if (BV.compare var var2) <> 0 then
                sign, var2, old_val
              else if sign = ConstraintType.plus then
                sign, var2, (myadd old_val value)
              else
                sign, var2, (myadd old_val (BI.neg value))
             )
              consts)
        ) oct
    with Not_found ->
      oct

  let erase var oct =
    let oct = remove var oct in
    map (fun (a, consts) ->
        (a, List.fold_right (fun a b ->
             let (_, var2, _) = a in
             if (BV.compare var var2) = 0
             then b
             else a :: b
           ) consts [])
      ) oct

  let rec projection var1 var2 oct =
    (* TODO: swap var1 var2 if var1 > var2 *)
    match var2 with
    | None ->
      (try
         let (inv, _) = find var1 oct in
         inv
       with Not_found ->
         INV.top ())
    | Some (sign, var2) ->
      let cmp = (BV.compare var1 var2) in
      if cmp < 0 then
        try
          let (_, consts) = find var1 oct in
          let first, second = find_constraints var2 consts in
          let candidate = if sign = CT.plus then first else second in
          match candidate with
          | Some inv -> inv
          | None -> INV.top ()
        with Not_found ->
          INV.top ()
      else if cmp > 0 then
        if sign = CT.plus then
          projection var2 (Some (true, var1)) oct
        else
          INV.neg (projection var2 (Some (false, var1)) oct)
      else
        Lattice.unsupported "wrong arguments (projection should not be called with var1=var2(?) )"

  let upper = function                          (* this looks like it is the same as BatOption.map INV.maximal *)
    | None -> None
    | Some inv -> INV.maximal inv

  let lower = function                          (* this looks like it is the same as BatOption.map INV.maximal *)
    | None -> None
    | Some inv -> INV.minimal inv

  let neg = function
    | None -> None
    | Some i -> Some (Int64.neg i)

  let cache = ref (MatrixCache.create 0)

  let matrix_get (i, i_inv) (j, j_inv) oct =
    let key = ((i, i_inv), (j, j_inv)) in
    if MatrixCache.mem !cache key
    then MatrixCache.find !cache key
    else
      let rec matrix_get (i, i_inv) (j, j_inv) oct =
        let cmp = BV.compare i j in
        if cmp <> 0
        then
          if cmp > 0 then
            let sumConst, difConst, _ = get_relation j i oct in
            match i_inv, j_inv with
            | true, false -> upper sumConst
            | false, true -> OPT.map BI.neg (lower sumConst)
            | false, false -> upper difConst
            | true, true -> OPT.map BI.neg (lower difConst)
          else if i_inv <> j_inv then
            matrix_get (j, i_inv) (i, j_inv) oct
          else
            matrix_get (j, not i_inv) (i, not j_inv) oct
        else
          let const = get_interval i oct in
          match i_inv, j_inv with
          | false, true -> OPT.map (BI.mul (BI.neg (BI.of_int 2))) (lower const)
          | true, false -> OPT.map (BI.mul (BI.of_int 2)) (upper const)
          | _ -> Some (BI.zero)
      in
      let res = OPT.bind
          (matrix_get (i, i_inv) (j, j_inv) oct)
          (fun a ->
             let a = min max_int a |> max min_int in
             if a = max_int || a = min_int then None
             else Some a
          )
      in
      MatrixCache.add !cache key res;
      res


  let rec matrix_set (i, i_inv) (j, j_inv) value oct =
    let key = ((i, i_inv), (j, j_inv)) in
    MatrixCache.add !cache key (Some value);
    if BV.compare i j <> 0 then
      (let i, j = j, i in
       let i_inv, j_inv =
         if i_inv = j_inv
         then (not i_inv, not j_inv)
         else i_inv, j_inv
       in
       MatrixCache.add !cache ((i, i_inv), (j, j_inv)) (Some value));
    let cmp = BV.compare i j in
    if cmp <> 0
    then
      if cmp > 0
      then
        match i_inv, j_inv with
        | true, false ->
          set_constraint (j, Some (CT.plus, i), CT.Upper, value) oct
        | false, true ->
          set_constraint (j, Some (CT.plus, i), CT.Lower, BI.neg value) oct
        | false, false ->
          set_constraint (j, Some (CT.minus, i), CT.Upper, value) oct
        | true, true ->
          set_constraint (j, Some (CT.minus, i), CT.Lower, BI.neg value) oct
      else if i_inv <> j_inv
      then matrix_set (j, i_inv) (i, j_inv) value oct
      else matrix_set (j, not i_inv) (i, not j_inv) value oct
    else
    if not i_inv && j_inv
    then
      set_constraint (i, None, CT.Lower, BI.neg(BI.div value (BI.of_int 2))) oct
    else if i_inv && not j_inv
    then
      set_constraint (i, None, CT.Upper, BI.div value (BI.of_int 2)) oct
    else Lattice.unsupported "error"


  let strong_closure_map oct =
    let vars = fold (fun key _ keys -> key::keys) oct []
               |> List.rev
    in

    let var_amount = List.length vars in
    cache := MatrixCache.create (var_amount * var_amount * 4);

    let add a b =
      match a, b with
      | Some a, Some b -> Some (BI.add a b)
      | _ -> None
    in

    let min a b =
      match a, b with
      | Some a, Some b -> Some (min a b)
      | Some a, None | None, Some a -> Some a
      | _ -> None
    in

    let signs = [(false, false);
                 (true, false);
                 (false, true);
                 (true, true)]
    in

    let strong_closure_s oct =
      List.fold_left (fun oct i ->
          List.fold_left (fun oct j ->
              if BV.compare i j > 0 then
                List.fold_left (fun oct (i_sign, j_sign) ->
                    let old_val = matrix_get (i, i_sign) (j, j_sign) oct in
                    let first = (matrix_get (i, i_sign) (i, i_sign <> true) oct) in
                    let second = (matrix_get (j, j_sign <> true) (j, j_sign) oct) in
                    let new_val = add first second in
                    let new_val = OPT.map
                        (fun x -> BI.div x (BI.of_int 2))
                        new_val
                    in
                    let new_val = min old_val new_val in
                    if new_val <> old_val &&
                       not (old_val = None &&
                            not ((OPT.get new_val)
                                 < max_int))
                    then
                      match new_val with
                      | Some new_val ->
                        matrix_set (i, i_sign) (j, j_sign) new_val oct
                      | None -> oct
                    else oct
                  ) oct signs
              else oct
            ) oct vars
        ) oct vars
    in

    List.fold_left (fun oct k ->
        let oct = List.fold_left (fun oct i ->
            List.fold_left (fun oct j ->
                let cmp = BV.compare i j in
                if cmp < 0
                then oct
                else
                  List.fold_left (fun oct (i_sign, j_sign) ->
                      if cmp = 0 && i_sign = j_sign then oct else
                        let old_val = matrix_get (i, i_sign) (j, j_sign) oct in
                        let a = add (matrix_get (i, i_sign) (k, false) oct)
                            (matrix_get (k, false) (j, j_sign) oct) in
                        let b = add (matrix_get (i, i_sign) (k, true) oct)
                            (matrix_get (k, true) (j, j_sign) oct) in
                        let c = add (matrix_get (i, i_sign) (k, false) oct)
                            (add (matrix_get (k, false) (k, true) oct)
                               (matrix_get (k, true) (j, j_sign) oct)) in
                        let d = add (matrix_get (i, i_sign) (k, true) oct)
                            (add (matrix_get (k, true) (k, false) oct)
                               (matrix_get (k, false) (j, j_sign) oct)) in
                        let new_val = List.fold_left min old_val [a; b; c; d] in
                        if new_val <> old_val &&
                           not (old_val = None &&
                                not ((OPT.get new_val)
                                     < max_int))
                        then begin
                          let oct =
                            match new_val with
                            | Some new_val ->
                              matrix_set (i, i_sign) (j, j_sign) new_val oct
                            | None -> oct
                          in oct
                        end
                        else oct
                    ) oct signs
              ) oct vars
          ) oct vars
        in
        let oct = strong_closure_s oct in
        oct
      ) oct vars

  let inv_index i = i lxor 1

  let map_to_matrix oct =
    let vars : (BV.t, int) Hashtbl.t = Hashtbl.create 0 in
    let () = iter (fun var _ -> Hashtbl.add vars var (Hashtbl.length vars)) oct in

    let matrix =
      let size = (Hashtbl.length vars) * 2 in
      Array.make_matrix size size Infinity
    in
    let add_constraints var (const, consts) =
      let set i j v = Array.set (Array.get matrix i) j (Val (Big_int_Z.float_of_big_int v)) in
      let index1 = (Hashtbl.find vars var) * 2 in
      set index1 index1 BI.zero;
      set (inv_index index1) (inv_index index1) BI.zero;
      let upper = OPT.default max_int (INV.maximal const) in
      let lower = OPT.default min_int (INV.minimal const) in
      let two = BI.of_int 2 in
      set (inv_index index1) index1 (BI.mul upper two);
      set index1 (inv_index index1) (BI.neg (BI.mul lower two));

      let add_constraints (sign, var2, const) =
        let index2 = try (Hashtbl.find vars var2) * 2
          with Not_found ->
          raise (Invalid_argument ("Not found var:" ^ var2.vname ^ "@" ^ var2.vdecl.file ^ ":" ^ (string_of_int var2.vdecl.line)))
        in
        let upper = OPT.default max_int (INV.maximal const) in
        let lower = OPT.default min_int (INV.minimal const) in
        if not (BI.compare lower min_int = 0)
        then if sign = CT.plus
          then (set index1 (inv_index index2) (BI.neg lower);
                set index2 (inv_index index1) (BI.neg lower))
          else (set (inv_index index2) (inv_index index1) (BI.neg lower);
                set index1 index2 (BI.neg lower));
        if not (BI.compare upper max_int = 0)
        then if sign = CT.plus
          then (set (inv_index index1) index2 upper;
                set (inv_index index2) index1 upper)
          else (set index2 index1 upper;
                set (inv_index index1) (inv_index index2) upper)
      in
      List.iter add_constraints consts
    in

    iter add_constraints oct;
    matrix, vars

  let big_int_of_float x =
    let float_str = Float.to_string x in
    let dot_index = String.index float_str '.' in
    let integer_str = String.sub float_str 0 dot_index in
    Big_int_Z.big_int_of_string integer_str

  let matrix_to_map matrix vars =
    let inv_vars = Hashtbl.create (Hashtbl.length vars) in
    Hashtbl.iter (fun var index -> Hashtbl.add inv_vars (index * 2) var) vars;
    let get i j = Array.get (Array.get matrix i) j in
    let unpack_interval lower upper =
      match lower, upper with
      | Val l, Val u ->
        let l' = Big_int_Z.max_big_int (BI.neg (BI.div (big_int_of_float l) (BI.of_int 2))) min_int in (* ok to do this here, only called from closure and closure does not cause over/underflows for vars -> is that true (?!) *)
        let u' =  Big_int_Z.min_big_int (BI.div (big_int_of_float u) (BI.of_int 2)) max_int in
        INV.of_interval oct_ik (l', u')
      | _ -> INV.top ()
    in
    let unpack_constraints lower upper =
      match lower, upper with
      | Val l, Val u -> (* only if both upper and lower boundaries have definitive values, if one of them is Infinity there is no guarantee that no wraparound occurred *)
        let l' = BI.neg (big_int_of_float l) in
        let u' = big_int_of_float u in
        INV.of_interval oct_ik (l', u')   (* By creating an interval here, wraparounds (min < min_int or max > max_int) are handled properly *)
      | _ -> INV.top ()
    in
    let rec matrix_iter i j oct =
      if i >= Array.length matrix then
        oct
      else if j >= Array.length matrix then
        matrix_iter (i + 2) 0 oct
      else
        let var1 = Hashtbl.find inv_vars i in
        let var2 = Hashtbl.find inv_vars j in
        if i = j then
          let inv = unpack_interval (get i (inv_index i)) (get (inv_index i) i) in
          let upper = BatOption.default max_int (INV.maximal inv) in
          let lower = BatOption.default min_int (INV.minimal inv) in
          let oct = set_constraint (var1, None, CT.Upper, upper) oct in
          let oct = set_constraint (var1, None, CT.Lower, lower) oct in
          matrix_iter i (j + 2) oct
        else if i < j then
          let oct =
            let inv = unpack_constraints (get i (inv_index j)) (get (inv_index i) j) in
            let lower = BatOption.default min_int (INV.minimal inv) in
            let upper = BatOption.default max_int (INV.maximal inv) in
            set_constraint (var1, Some(CT.plus, var2), CT.Upper, upper)
              (set_constraint (var1, Some(CT.plus, var2), CT.Lower, lower) oct)
          in
          let oct =
            let inv = unpack_constraints (get i j) (get (inv_index i) (inv_index j)) in
            let lower = BatOption.default min_int (INV.minimal inv) in
            let upper = BatOption.default max_int (INV.maximal inv) in
            set_constraint (var1, Some(CT.minus, var2), CT.Upper, upper)
              (set_constraint (var1, Some(CT.minus, var2), CT.Lower, lower) oct)
          in
          matrix_iter i (j + 2) oct
        else
          matrix_iter i (j + 2) oct
    in
    matrix_iter 0 0 (top ())

  let use_matrix_closure = true

  let remove_empty = filter (fun var (const, consts) ->
      not (INV.is_top_of (ikind ()) const) || not ((List.length consts) = 0))

  let strong_closure oct =
    if use_matrix_closure then
      let matrix, vars = map_to_matrix oct in
      let closed = ArrayOctagon.strong_closure matrix in
      matrix_to_map closed vars
    else
      strong_closure_map oct

  (* Remove all information except those concerning variables in vars *)
  let keep_only vars oct =
    let oct_keys_filtered = filter (fun k _ -> List.mem k vars) oct in
    let filter_constraints (inv, consts) = (inv, List.filter (fun (_,v,_) -> List.mem v vars) consts) in
    map filter_constraints oct_keys_filtered

  let widen a b = widen a (strong_closure b) (* strong closure must not(!) be called for the result (https://arxiv.org/pdf/cs/0703084.pdf)  *)

  let narrow a b =
    (* Some constraints may involve a variable that is not there in the result. These need to be removed *)
    let remove_invalid_constraints a = map (fun (k, (l:E.t list)) -> (k, List.filter (fun (_, v, _) -> mem v a) l)) in
    map2 VD.narrow a b |> remove_invalid_constraints a |> strong_closure

  let meet a b = meet a b |> strong_closure
  let join a b = join a b                       (* a strong closure is useless here if the arguments are strongly closed *)

  let leq a b =
    if !Goblintutil.in_verifying_stage then
      leq a b || leq (strong_closure a) b
    else
      leq a b
end

module MapOctagonBot : S
  with type key = BV.t = struct
  include Lattice.LiftBot (MapOctagon)

  type key = MapOctagon.key

  let ignore_bot f = function
    | `Bot -> `Bot
    | `Lifted x -> lift @@ f x

  let set_constraint const =
    ignore_bot (MapOctagon.set_constraint const)

  let adjust key value =
    ignore_bot (MapOctagon.adjust key value)

  let erase key =
    ignore_bot (MapOctagon.erase key)

  let strong_closure =
    ignore_bot MapOctagon.strong_closure

  let map_to_matrix = function
    | `Bot -> Array.make_matrix 0 0 Infinity, Hashtbl.create 0
    | `Lifted x -> MapOctagon.map_to_matrix x

  let matrix_to_map m v = `Lifted (MapOctagon.matrix_to_map m v)

  let projection key key2 = function
    | `Bot -> INV.top ()
    | `Lifted x -> MapOctagon.projection key key2 x

  let get_relation i j oct =
    match oct with
    | `Bot -> None, None, false
    | `Lifted x -> MapOctagon.get_relation i j x

  let keep_only vars =
    ignore_bot (MapOctagon.keep_only vars)
end
