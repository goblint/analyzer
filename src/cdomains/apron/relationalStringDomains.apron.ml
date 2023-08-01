open GoblintCil
open Pretty
open Apron
open Analyses

module ID = IntDomain.IntDomTuple

module Var = SharedFunctions.PrintableVar
module V = RelationDomain.V (Var)

module SetS = Set.Make(Var)

module type StringRelationDomain =
sig
  include RelationDomain.RD

  val varinfo_to_var: varinfo -> var

  val string_copy: ('a, 'b, 'c, 'd) ctx -> t -> varinfo -> varinfo -> int option -> t
  val string_concat: ('a, 'b, 'c, 'd) ctx -> t -> varinfo -> varinfo -> int option -> t
  val to_string_length: ('a, 'b, 'c, 'd) ctx -> t  -> varinfo -> ID.t
  val substring_extraction: ('a, 'b, 'c, 'd) ctx -> t -> varinfo -> varinfo -> varinfo -> t * bool * bool
  val string_comparison: ('a, 'b, 'c, 'd) ctx -> t -> varinfo -> varinfo -> int option -> ID.t
end

module RelationalSubstring : StringRelationDomain =
struct
  module Var = Var
  module V = V

  type var = Var.t

  module Tracked = SharedFunctions.Tracked
  module EnvOps = SharedFunctions.EnvOps

  (* only track must substring relations; we don't track trivial x <= x relations explicitly *)
  module S = SetDomain.Reverse (SetDomain.ToppedSet (Printable.Prod (Var) (Var)) (struct let topname = "All substrings of each other" end))

  type t = {
    r_set : S.t;
    env : Environment.t
  }
  [@@deriving eq, ord, hash]

  let name () = "RelationalSubstringDomain"

  let show t = "Substrings:" ^ S.fold (fun (v1, v2) acc -> acc ^ " (" ^ Var.show v1 ^ " <= " ^ Var.show v2 ^ ")") t.r_set String.empty
  let pretty () t = text (show t)
  let pretty_diff () (t1, t2) = dprintf "%s: %a not leq %a" (name ()) pretty t1 pretty t2
  let printXml f t = BatPrintf.fprintf f "<value>\n<map>\n<key>\nsubstrings\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key><value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show t))) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (t.env)))
  let to_yojson t = `String (show t) (* TODO: does this work? *)
  let tag _ = failwith "no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift t = t

  (* helpers *)
  let transitive_closure s =
    let transitive_relations (x, y) s =
      let new_x_relations =
        S.filter (fun (y', z) -> Var.equal y' y && not (Var.equal z x)) s
        |> S.map (fun (y, z) -> (x, z)) in
      S.union s new_x_relations in
    let rec add_all_relations s =
      let s' = S.fold transitive_relations s s in
      if S.equal s s' then
        s
      else
        add_all_relations s' in
    add_all_relations s

  let vid_to_id ik = function (* TODO: okay? *)
    | `Lifted i -> i
    | _ -> ID.top_of ik

  let bot () = {r_set = S.bot (); env = Environment.make [||] [||]}
  let is_bot t = S.is_bot t.r_set

  let top () = {r_set = S.top (); env = Environment.make [||] [||]}
  let is_top t = S.is_top t.r_set

  let leq t1 t2 = S.leq t1.r_set t2.r_set

  let join t1 t2 = {r_set = S.join t1.r_set t2.r_set; env = Environment.lce t1.env t2.env}
  let meet t1 t2 = {r_set = transitive_closure (S.meet t1.r_set t2.r_set); env = Environment.lce t1.env t2.env}
  let widen = join
  let narrow = meet

  let is_bot_env t = S.is_empty t.r_set

  let vars t = EnvOps.vars t.env

  let add_vars t vars = {r_set = t.r_set; env = EnvOps.add_vars t.env vars}

  let remove_vars t vars = {r_set = S.filter (fun (x, y) -> not (List.mem x vars) && not (List.mem y vars)) t.r_set; env = EnvOps.remove_vars t.env vars}
  let remove_vars_with _ _ = ()

  let remove_filter t f = {r_set = S.filter (fun (x, y) -> not (f x) && not (f y)) t.r_set; env = EnvOps.remove_filter t.env f} 
  let remove_filter_with _ _ = ()

  let copy t = t

  let keep_vars t vars = {r_set = S.filter (fun (x, y) -> List.mem x vars && List.mem y vars) t.r_set; env = EnvOps.keep_vars t.env vars}
  let keep_filter t f = {r_set = S.filter (fun (x, y) -> f x && f y) t.r_set; env = EnvOps.keep_filter t.env f} 

  let forget_vars t vars = {r_set = S.filter (fun (x, y) -> not (List.mem x vars) && not (List.mem y vars)) t.r_set; env = t.env}

  let assign_exp t var _ _ = forget_vars (add_vars t [var]) [var]
  let assign_var t var var' = forget_vars (add_vars t [var; var']) [var]
  let assign_var_parallel_with _ _ = ()
  let assign_var_parallel' t vars1 vars2 = add_vars (forget_vars t vars1) (vars1 @ vars2)
  let substitute_exp = assign_exp

  let unify t1 t2 = meet t1 t2

  type marshal = unit
  let marshal _ = ()
  let unmarshal () = top ()

  let mem_var t var = Environment.mem_var t.env var

  (* nop *)
  let assert_inv t _ _ _ = t
  let eval_int t exp no_ov = Queries.ID.top () (* TODO: does this work? *)
  let cil_exp_of_lincons1 a = None

  let invariant t = []

  let varinfo_to_var (v:varinfo) =
    if v.vglob then
      V.global v
    else
      V.local v

  (* string functions *)
  let string_copy ctx t dest src n = 
    let dest' = varinfo_to_var dest in
    let src' = varinfo_to_var src in

    (* remove all relations involving dest; add (dest <= src), (src <= dest) and transitively all relations involving src also for dest *)
    let t_without_dest = forget_vars t [dest'] in
    let t_with_new_dest = {r_set = S.add (dest', src') (S.add (src', dest') t_without_dest.r_set); env = t_without_dest.env} in
    let t_with_new_relations = {r_set = transitive_closure t_with_new_dest.r_set; env = t_with_new_dest.env} in

    (* ask for size(dest) and strlen(src); 
     * no check for string literals as query returns top for size;
     * no check for null_ptr as query return top for them for both size and len *)
    let size_dest = vid_to_id ILong (ctx.ask (VarArraySize dest)) in
    let len_src = vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength src)) in

    (* add new relations if size(dest) > strlen(src) or (size(dest) >= n and n > strlen(src)) *)
    match ID.minimal size_dest, ID.maximal len_src, n with
    (* strcpy *)
    | Some min_size_dest, Some max_len_src, None 
      when Z.gt min_size_dest max_len_src -> 
      if Var.equal dest' src' then (* check only performed here to exclude strcpy(x, x) for a string literal since it's undefined behavior *)
        t
      else
        t_with_new_relations
    (* strncpy *)
    | Some min_size_dest, Some max_len_src, Some n 
      when Z.geq min_size_dest (Z.of_int n) && Z.gt (Z.of_int n) max_len_src -> 
      if Var.equal dest' src' || n <= 0 then
        t
      else
        t_with_new_relations
    | _ -> t_without_dest

  let string_concat ctx t dest src n =
    let dest' = varinfo_to_var dest in
    let src' = varinfo_to_var src in

    (* remove relation (dest <= x); add (src <= dest) and transitively for all relations (x <= src) add (x <= dest) *)
    let t_without_dest = {r_set = S.filter (fun (x, _) -> not (Var.equal x dest')) t.r_set; env = t.env} in
    let t_with_src_substr_dest = {r_set = S.add (src', dest') t_without_dest.r_set; env = t_without_dest.env} in
    let t_with_new_relations = {r_set = transitive_closure t_with_src_substr_dest.r_set; env = t_with_src_substr_dest.env} in

    (* ask for size(dest), strlen(dest) and strlen(src) *)
    let size_dest = vid_to_id ILong (ctx.ask (VarArraySize dest)) in
    let len_dest = vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength dest)) in
    let len_src = vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength src)) in

    (* add new relations if size(dest) > strlen(dest) + strlen(src) or (size(dest) >= strlen(dest) + n and n > strlen(src)) *)
    match ID.minimal size_dest, ID.maximal len_dest, ID.maximal len_src, n with
    (* strcat *)
    | Some min_size_dest, Some max_len_dest, Some max_len_src, None 
      when Z.gt min_size_dest (Z.add max_len_dest max_len_src) ->
      if Var.equal dest' src' then (* check only performed here to exclude strcat(x, x) for a string literal since it's undefined behavior *)
        t_without_dest
      else
        t_with_new_relations
    (* strncat *)
    | Some min_size_dest, Some max_len_dest, Some max_len_src, Some n 
      when Z.geq min_size_dest (Z.add max_len_dest (Z.of_int n)) && Z.gt (Z.of_int n) max_len_src ->
      if n <= 0 then
        t
      else if Var.equal dest' src' then
        t_without_dest
      else
        t_with_new_relations
    | _ -> t_without_dest

  let to_string_length ctx t s =
    let s' = varinfo_to_var s in
    (* collect all strings = s *)
    let t_eq = S.fold (fun (x, y) acc -> 
        if Var.equal x s' && S.mem (y, s') t.r_set then
          SetS.add y acc
        else
          acc)
        t.r_set SetS.empty in
    (* collect all strings <= s *)
    let t_leq = S.fold (fun (x, y) acc ->
        if Var.equal x s' then 
          SetS.add y acc
        else
          acc)
        t.r_set SetS.empty in
    (* collect all strings >= s *)
    let t_geq = S.fold (fun (x, y) acc -> 
        if Var.equal y s' then
          SetS.add x acc
        else
          acc)
        t.r_set SetS.empty in

    (* s = s' ==> strlen(s) = strlen(s') *)
    let len_eq = SetS.fold (fun x acc -> match V.to_cil_varinfo x with
        | Some x -> ID.meet (vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength x))) acc
        | None -> acc)
        t_eq (vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength s))) in
    (* s <= s' ==> strlen(s) <= strlen(s') *)
    let len_eq_leq = SetS.fold (fun x acc -> match V.to_cil_varinfo x with
        | Some x -> ID.meet (ID.le (ID.top_of !Cil.kindOfSizeOf) (vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength x)))) acc
        | None -> acc) 
        t_leq len_eq in
    (* s' <= s ==> strlen(s) >= strlen(s') *)
    SetS.fold (fun x acc -> match V.to_cil_varinfo x with
        | Some x -> ID.meet (ID.ge (ID.top_of !Cil.kindOfSizeOf) (vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength x)))) acc
        | None -> acc)
      t_geq len_eq_leq

  (* returns is_maybe_null_ptr, is_surely_offset_0 *)
  let substring_extraction ctx t lval haystack needle =
    let lval' = varinfo_to_var lval in
    let haystack' = varinfo_to_var haystack in
    let needle' = varinfo_to_var needle in
    (* if needle = haystack, strstr returns a pointer to haystack at offset 0 and lval = haystack = needle *)
    if S.mem (needle', haystack') t.r_set && S.mem (haystack', needle') t.r_set then
      let new_r_set =
        S.add (lval', haystack') t.r_set
        |> S.add (haystack', lval')
        |> S.add (lval', needle')
        |> S.add (needle', lval')
        |> transitive_closure in
      {r_set = new_r_set; env = t.env}, false, true
      (* if needle <= haystack, strstr returns a pointer to haystack with unknown offset and needle <= lval <= haystack *)
    else if S.mem (needle', haystack') t.r_set then
      let new_r_set =
        S.add (lval', haystack') t.r_set
        |> S.add (needle', lval')
        |> transitive_closure in
      {r_set = new_r_set; env = t.env}, false, false
      (* else strstr could return a pointer to haystack with unknown offset or a null_ptr *)
    else
      t, true, false

  let string_comparison ctx t s1 s2 n =
    let s1' = varinfo_to_var s1 in
    let s2' = varinfo_to_var s2 in
    (* strcmp *)
    match n with
    | None ->
      (* return 0 if s1 = s2 *)
      if S.mem (s1', s2') t.r_set && S.mem (s2', s1') t.r_set then
        ID.of_int IInt Z.zero
      else
        ID.top_of IInt
    (* strncmp *)
    | Some n ->
      let len_s1 = vid_to_id !Cil.kindOfSizeOf (ctx.ask (VarStringLength s1)) in
      begin match ID.maximal len_s1 with
        (* return 0 if s1 = s2 and n > strlen(s1) = strlen(s2) *)
        | Some max_len_s1 when Z.gt (Z.of_int n) max_len_s1 
                            && S.mem (s1', s2') t.r_set && S.mem (s2', s1') t.r_set -> ID.of_int IInt Z.zero
        | _ -> ID.top_of IInt
      end
end

