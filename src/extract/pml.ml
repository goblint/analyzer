open BatteriesExceptionless

type 'a e = 'a * string (* some expression and its promela representation *)
type _ t = (* promela types *)
  | Enum : 'a * ('a -> string) -> 'a t
  | Chan : 'a chan -> 'a chan t
  | Bool : bool -> bool t
  | Byte : int -> int t | Short : int -> int t | Int : int -> int t (* maybe do something like Bigarray.kind to avoid conflating ints of different size *)
  (* the following are for arguments, and can not be declared in promela *)
  | String : string -> string t
  | Void : unit t (* this is just for skipping uninteresting arguments *)
and 'a chan = { capacity : int e (* 0 = rendez-vous *); mutable content : 'a list; content_type : 'a t }

let enums : (string, string) Hashtbl.t = Hashtbl.create 123 (* value -> type (both as strings) *)
let of_enums = Hashtbl.create 123 (* enum type -> (enum value as int -> enum value as string option) *)
let enum of_enum show name =
  Hashtbl.add of_enums name (fun x -> Option.map show (of_enum x));
  let values = List.of_enum @@ (0 -- 13) //@ of_enum /@ show in
  List.iter (flip (Hashtbl.add enums) name) values;
  (), "mtype = { "^ String.concat ", " values ^" }"
let enum_type x = Hashtbl.find enums x
let is_enum = Option.is_some % Hashtbl.find enums
let rec is_declared : type a . a t -> bool = function
  | Enum (x,s) -> is_enum (s x)
  | Chan c -> is_declared c.content_type
  | _ -> true
let rec unbox : type a . a t -> string * (a -> string) * a = function (* type, show, value *)
  | Enum (x,s) -> "mtype", s, x
  | Chan c ->
    let t',s,x = unbox c.content_type in
    "chan", (fun c -> "["^snd c.capacity^"] of {"^t'^"}"), c
  | Bool x -> "bool", string_of_bool, x
  | Byte x -> "byte", string_of_int, x
  | Short x -> "short", string_of_int, x
  | Int x -> "int", string_of_int, x
  | String x -> "string", identity, x
  | Void -> "byte", (fun () -> assert false), ()

type _ var = (* variable with initial value and printer. type: content * get * set * mod *)
  | Var : { name : string; init : 'a t; value : 'a ref; show : 'a -> string } ->
    ('a e * 'a e            * ('a e -> unit e)         * unit e) var
  | Arr : { name : string; init : 'a t; value : 'a array; show : 'a -> string; show_all : 'a array -> string; length : int } ->
    ('a e * (int e -> 'a e) * (int e * 'a e -> unit e) * (int e -> unit e)) var

let var value name =
  assert (is_declared value);
  let t,s,v = unbox value in
  Var { name; init = value; value = ref v; show = s },
  t^" "^name^" = "^s v^";"
let arr length value name =
  assert (is_declared value);
  let t,s,v = unbox value in
  let show_arr a = Array.to_list a |> List.map s |> String.concat ", " in
  Arr { name; init = value; value = Array.create (fst length) v; show = s; show_all = show_arr; length = fst length },
  t^" "^name^"["^snd length^"] = "^s v^";"

let var_name : type x. x var -> string = function
  | Var v -> v.name
  | Arr v -> v.name

let get : type c g s m. (c*g*s*m) var -> g = function
  | Var v -> !(v.value), v.name
  | Arr v -> fun i ->
    assert (fst i >= 0 && fst i < v.length);
    Array.get v.value (fst i), v.name^"["^snd i^"]"
let set : type c g s m. (c*g*s*m) var -> s = function
  | Var v -> fun x -> (v.value := fst x), v.name^" = "^snd x^";"
  | Arr v -> fun (i,x) ->
    assert (fst i >= 0 && fst i < v.length);
    Array.set v.value (fst i) (fst x), v.name^"["^snd i^"] = "^snd x^";"

(* Var x: !x, x := v. Arr x: !x i, x := i, v *)
let (!) = get
let (:=) = set
let (@=) = set (* maybe use this since it has higher precedence than >> *)

(* we do not define `not` since this may tempt people to use !empty instead of nempty (same with nfull) *)

let _assert x = assert (fst x), "assert("^snd x^");"
let println x = (* print_endline (fst x) *) (), "printf(\""^snd x^"\\n\");"

let _true  = true,  "true"
let _false = false, "false"

let wait x = ignore (fst x = true), "("^snd x^");" (* a standalone expression in Promela is a statement that blocks until the expression is true *)
let break = (), "break" (* jumps to the end of the innermost do loop *)
let skip = wait _true
let nop = (), ""

module Chan = struct (* channels *)
  let create len content = Chan { capacity = len; content = []; content_type = content }

  let full   (c,s) = List.compare_length_with c.content (fst c.capacity) >= 0, "full("^s^")"
  let nfull  (c,s) = List.compare_length_with c.content (fst c.capacity) < 0, "nfull("^s^")"
  let empty  (c,s) = c.content = [], "empty("^s^")"
  let nempty (c,s) = c.content <> [], "nempty("^s^")"
  let len    (c,s) = List.length c.content, "len("^s^")"

  (*  ? = first, ?? = any, no <> = consume, <> = remain, TODO eval, !! *)
  let first c e = match c.content with x::_ when x = fst e -> true | _ -> false
  let any c e = List.mem (fst e) c.content
  let poll m (c,s) e = (* checks (no side-effects) if (the first|any) element is e *)
    let b,op = match m with
      | `First -> first c e, "?"
      | `Any   -> any c e,   "??"
    in
    b, s^op^"["^snd e^"]"
  let recv m (c,s) e = (* blocks until (the first|any) element is e (w/o changing e) and removes it from the channel *)
    let b,op = match m with
      | `First -> first c e, "?"
      | `Any   -> any c e,   "??"
    in
    c.content <- List.remove c.content (fst e);
    (), s^op^"eval("^snd e^")"
  let send (c,s) e = (* append|sorted *)
    fst (nfull (c,s)), s^"!"^snd e
end

(* OCaml's ; - sequential composition *)
let (>>) a b = fst b, snd a^"\n"^snd b
let bind x f = x >> f (fst x)
let (>>=) = bind
let (let*) = bind (* introduced in OCaml 4.08.0: https://ocaml.org/manual/bindingops.html *)
let return x = x (* ? *)

let indent x = String.split_on_string ~by:"\n" x |> List.map (fun x -> "  "^x) |> String.concat "\n"

let surround a b (v,s) = v, a^"\n"^indent s^"\n"^b
let _match xs =
  assert (xs <> []);
  let s = String.concat "\n" @@ List.map (fun (c,b) -> ":: "^snd c^" -> "^snd b) xs in
  (*List.find (fst%fst) xs, s*)
  (), s
let _do xs = surround "do" "od;" (_match xs)
let _if xs = surround "if" "fi;" (_match xs)
let _ifte b x y =
  (* TODO `else` fails for channel operations, also, how to implement in OCaml? *)
  let c = true, "else" in
  _if [b, x; c, y]
let _ift b x = _ifte b x skip
let _for (Var i) (Arr v) b = (* for (i in a) { b } *)
  surround ("for ("^i.name^" in "^v.name^") {") "}" b
let _foreach (Arr v as a) b = (* for (i in a) { `b i a[i]` } *)
  var (Byte 0) ("i_"^v.name) >>= (* TODO this doesn't work for nested loops on the same array! *)
  fun i -> _for i a (b !i (!a !i)) (* NOTE this already gives the values to the body instead of the variables TODO for interpreter *)


(* extraction *)
type ('a,'b,'c,'d,'e,'z) args = A0 of 'z e | A1 of 'a * ('a -> 'z e) | A2 of 'a*'b * ('a -> 'b -> 'z e) | A3 of 'a*'b*'c * ('a -> 'b -> 'c -> 'z e) | A4 of 'a*'b*'c*'d * ('a -> 'b -> 'c -> 'd -> 'z e) | A5 of 'a*'b*'c*'d*'e * ('a -> 'b -> 'c -> 'd -> 'e -> 'z e)

module Macro = struct
  let define name args =
    let unpack p = function
      | A0 z -> [], z
      | A1 (a,f) -> [p a], f a
      | A2 (a,b,f) -> [p a; p b], f a b
      | A3 (a,b,c,f) -> [p a; p b; p c], f a b c
      | A4 (a,b,c,d,f) -> [p a; p b; p c; p d], f a b c d
      | A5 (a,b,c,d,e,f) -> [p a; p b; p c; p d; p e], f a b c d e
    in
    let aa, body = unpack (fun (Var v) -> v.name) args in
    let args = String.concat ", " aa |> fun x -> if List.is_empty aa then x else "("^x^")" in
    (), "\n#define "^name^args^"    "^snd body

  (* let _if e body = (), "\n#if "^snd e^"\n"^indent body^"\n#endif" *)
  let _if e = (), "#if "^snd e
  let _endif = (), "#endif"
end

type eval = (* how to evaluate function arguments *)
  | EvalEnum of (int -> string option) | EvalInt | EvalString (* standard types *)
  | EvalSkip (* don't evaluate *)
  | AssignIdOfString of string * int (* kind * position: evaluate argument at position as a string, use it to generate an id of kind, and assign it to argument, map id to int for promela *)
(*| EvalSpecial of string*)
[@@deriving show]

let extract_funs = Hashtbl.create 123 (* name of function to list of how to evaluate arguments *)
let special_fun name = Hashtbl.find extract_funs name
let special_funs () = Hashtbl.keys extract_funs |> List.of_enum

let extract ?id fname args = (* generate code for function and register for extraction *)
  let unpack =
    let name (Var v) = v.name in
    let eval (type a b c) (Var v : (a*b*c*unit e) var) = match v.init with
      | Enum (x,s) -> EvalEnum (Option.get @@ Hashtbl.find of_enums (Option.get (enum_type (s x))))
      | Byte _ -> EvalInt
      | Short _ -> EvalInt
      | Int _ -> EvalInt
      | String _ -> EvalString
      | Void -> EvalSkip
      | _ -> failwith ("unsupported argument type for "^v.name)
    in
    let p a = name a, eval a in
    function
    | A0 z -> [], z
    | A1 (a,f) -> [p a], f a
    | A2 (a,b,f) -> [p a; p b], f a b
    | A3 (a,b,c,f) -> [p a; p b; p c], f a b c
    | A4 (a,b,c,d,f) -> [p a; p b; p c; p d], f a b c d
    | A5 (a,b,c,d,e,f) -> [p a; p b; p c; p d; p e], f a b c d e
  in
  let aa, body = unpack args in
  let aa' = List.filter (fun (n,e) -> e <> EvalSkip && e <> EvalString) aa in (* don't want skipped arguments or strings in definition *)
  let arg_names, arg_evals = List.map fst aa', List.map snd aa in
  let arg_evals = match id with
    | Some (dst, src, res) -> List.modify_at dst (fun _ -> AssignIdOfString (res, src)) arg_evals
    | None -> arg_evals
  in
  Hashtbl.add extract_funs fname arg_evals;
  (), "\ninline "^fname^"("^String.concat ", " arg_names^") { atomic {\n" ^
      indent (snd body) ^
      "\n}}\n"


(* overwrite remaining operators *)
let op o s x y = o (fst x) (fst y), snd x^" "^s^" "^snd y
let (==) x y = op (=)  "==" x y
let (!=) x y = op (<>) "!=" x y
let (<)  x y = op (<)  "<"  x y
let (>)  x y = op (>)  ">"  x y
let (<=) x y = op (<=) "<=" x y
let (>=) x y = op (>=) ">=" x y
let (+)  x y = op (+)  "+"  x y
let (-)  x y = op (-)  "-"  x y
let (/)  x y = op (/)  "/"  x y
let ( * ) x y = op ( * ) "*" x y
let (&&) x y = op (&&) "&&" x y
let (||) x y = op (||) "||" x y

let i i = i, string_of_int i (* int literal *)
let s s = s, s (* string literal *)
let (^) a b = fst a^fst b, snd a^snd b (* string concatenation *)
let fail m = println (s "FAIL: "^m) >> _assert _true (* TODO _assert _false evaluates too early :( *)
let i2s (i,s) = string_of_int i, s
let e x s = x, s x
let e2s x = s (snd x) (* TODO ? *)

let incr : type g s m. (int e*g*s*m) var -> m = function
  | Var _ as v -> set v (get v + i 1)
  | Arr _ as v -> fun j -> set v (j, (get v j + i 1))
let decr : type g s m. (int e*g*s*m) var -> m = function
  | Var _ as v -> set v (get v - i 1)
  | Arr _ as v -> fun j -> set v (j, (get v j - i 1))
