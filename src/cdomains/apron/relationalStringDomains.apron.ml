open GoblintCil
open Pretty
open Apron
open Analyses

module Var = SharedFunctions.PrintableVar
module V = RelationDomain.V (Var)

module type StringRelationDomain =
sig
  include RelationDomain.RD
  type idx

  val string_copy: Queries.ask -> t -> varinfo -> varinfo -> int option -> t
  val string_concat: Queries.ask -> t -> varinfo -> varinfo -> int option -> t
  val to_string_length: Queries.ask -> t  -> varinfo -> idx
  val substring_extraction: Queries.ask -> t -> varinfo -> varinfo -> bool * bool
  val string_comparison: Queries.ask -> t -> varinfo -> varinfo -> int option -> idx
end

module RelationalSubstring (Idx: IntDomain.Z): StringRelationDomain with type idx = Idx.t =
struct
  module Var = Var
  module V = V

  type var = Var.t
  type idx = Idx.t

  module Tracked = SharedFunctions.Tracked
  module EnvOps = SharedFunctions.EnvOps

  (* only track must substring relations *)
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

  let bot () = {r_set = S.bot (); env = Environment.make [||] [||]}
  let is_bot t = S.is_bot t.r_set

  let top () = {r_set = S.top (); env = Environment.make [||] [||]}
  let is_top t = S.is_top t.r_set

  let leq t1 t2 = S.leq t1.r_set t2.r_set

  let join t1 t2 = {r_set = S.join t1.r_set t2.r_set; env = Environment.lce t1.env t2.env}
  let meet t1 t2 = {r_set = S.meet t1.r_set t2.r_set; env = Environment.lce t1.env t2.env}
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

  (* string functions *)
  let string_copy ctx t dest src n = 
    let dest' = V.local dest in (* TODO: global instead? *)
    let t_without_dest = forget_vars t [dest'] in

    let size_dest = Idx.top () in (* TODO: ctx.ask *)
    let len_src = Idx.top () in (* TODO: ctx.ask *)
    match Idx.minimal size_dest, Idx.maximal len_src with
    | Some min_size_dest, Some max_len_src when Z.gt min_size_dest max_len_src -> 
      let src' = V.local src in (* TODO: global instead? *)
      {r_set = (S.fold (fun (x, y) acc -> if Var.equal x src then S.add (dest, y) acc else if Var.equal y src then S.add (x, dest) acc else acc) t_without_dest.r_set t_without_dest.r_set); env = t_without_dest.env}
        failwith "TODO"
    | _ -> t_without_dest (* TODO: no need to update ctx right? at least not here? *)

  let string_concat ctx t dest src n = failwith "TODO"

  let to_string_length ctx t s = failwith "TODO"

  let substring_extraction ctx t haystack needle = failwith "TODO"

  let string_comparison ctx t s1 s2 n = failwith "TODO"
end

