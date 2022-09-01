type operation = Delete | Insert | Unchanged
type diff = operation list

type ('a, 'b) unified_operation = UDelete of 'a | UInsert of 'b | UUnchanged of 'a * 'b
type ('a, 'b) unified_diff = ('a, 'b) unified_operation list

type ('a, 'b) myers_node = {
  x_i : int ;
  xs : 'a list ;
  ys : 'b list ;
  trace : operation list ;
}

(** Remove the longest common prefix (LCP) of two lists,
    return the length of the LCP and the two lists' tails *)
let rec remove_lcp ?(plus = 0) equal xs ys =
  match (xs, ys) with
  | x :: xs', y :: ys' when equal x y ->
      remove_lcp ~plus:(plus + 1) equal xs' ys'
  | _ -> plus, xs, ys

let prepend_n n x = List.rev_append (List.init n (Fun.const x))

(** "Slide" down a diagonal in the edit graph, i.e. take the longest common
    prefix of the current tails of the sequences and update the state appropriately. *)
let slide equal n =
  let lcp_len, xs, ys = remove_lcp equal n.xs n.ys in
  { x_i = n.x_i + lcp_len ; xs ; ys ; trace = prepend_n lcp_len Unchanged n.trace }

(** Move down in the edit graph, i.e. simulate an insertion operation. *)
let down n =
  match n.ys with
  | [] -> None
  | _ :: ys -> Some { n with ys ; trace = Insert :: n.trace }

(** Move right in the edit graph, i.e. simulate a deletion operation. *)
let right n =
  match n.xs with
  | [] -> None
  | _ :: xs -> Some { n with xs ; x_i = n.x_i + 1 ; trace = Delete :: n.trace }

(** Runs one step in Myers' algorithm. This corresponds to incrementing (by 1) the maximum
    length of the edit paths being considered, or D in Myers' paper. *)
let myers_step equal min_k (n1, ns) =
  let slide = slide equal in
  let rec impl k acc n1 =
    function
    | [] -> Option.(n1 |> down |> map slide |> to_list) @ acc
    | n2 :: ns ->
      let n' = match down n1, right n2 with
      | (None, Some n') | (Some n', None) -> n'
      | Some n1', Some n2' ->
        if n1'.x_i >= n2'.x_i then n1' else n2'
      | None, None -> failwith "(internal error) corner reached"
      in
      impl (k + 2) (slide n' :: acc) n2 ns
  in
  let n1_opt = n1 |> right in
  let ns' = impl min_k Option.(n1_opt |> map slide |> to_list) n1 ns |> List.rev in
  min_k + (if Option.is_some n1_opt then -1 else 1),
  ns'

(* TODO: this could be run during a myers step *)
let find_done ns = List.find_opt (function { xs = [] ; ys = [] ; _ } -> true | _ -> false) ns

let myers equal xs ys =
  let rec impl min_k (n1, ns) =
    match find_done (n1::ns) with
    | Some r -> List.rev r.trace
    | None ->
      match myers_step equal min_k (n1, ns) with
      | _, [] -> failwith "(internal error) something went wrong"
      | min_k', n1' :: ns' -> impl min_k' (n1', ns')
  in
  impl 0 (slide equal { x_i = 0 ; xs ; ys ; trace = [] }, [])

let myers' xs ys = myers (=) xs ys

let[@tail_mod_cons] rec unify xs ys diff =
  match xs, ys, diff with
  | x :: xs', _, Delete :: diff' -> UDelete x :: unify xs' ys diff'
  | _, y :: ys', Insert :: diff' -> UInsert y :: unify xs ys' diff'
  | x :: xs', y :: ys', Unchanged :: diff' -> UUnchanged (x, y) :: unify xs' ys' diff'
  | [], [], [] -> []
  | _ -> raise (Invalid_argument "diff does not match inputs")

let show_unified_operation show_a show_b =
  function
  | UDelete x -> "-" ^ show_a x
  | UInsert y -> "+" ^ show_b y
  | UUnchanged (x, _) -> " " ^ show_a x

let show_unified_operation' show = show_unified_operation show show

let show_unified_operation_str = show_unified_operation' Fun.id
