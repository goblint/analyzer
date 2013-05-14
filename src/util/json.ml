(** A small Json library. *)
open Pretty
open Num

(** General json exceprion *)
exception JsonE of string 

(** An json object -- this is just a map from strings *)
module Object = BatMap.Make (String)

(** Json data structure *)
type jvalue =
  | String of string
  | Number of num
  | Object of jvalue ref Object.t ref
  | Array  of jvalue ref list ref
  | True | False
  | Null

(** {3 Printf style printing. } *)

let identCount = ref 0

let rec printJson' ch = function
  | String x -> BatPrintf.fprintf ch "%S" x
  | Number i -> BatPrintf.fprintf ch "%s" (string_of_num i)
  | Object o -> print_object ch !o
  | Array a  -> BatList.print ~first:"[" ~last:"]" ~sep:", " (BatRef.print printJson') ch !a
  | True     -> BatPrintf.fprintf ch "true"
  | False    -> BatPrintf.fprintf ch "false"
  | Null     -> BatPrintf.fprintf ch "null"
  
and print_object ch = function
  | m when Object.is_empty m -> BatPrintf.fprintf ch "{}"
  | m -> 
      let prnt_str ch (x:string) = BatPrintf.fprintf ch "%S" x in
      let ident ch = for i = 1 to !identCount do BatPrintf.fprintf ch " " done in
      let n, i = Object.cardinal m, ref 1 in
      let print_one k v =
        let sep = if !i<n then "," else "" in
        incr i;
        BatPrintf.fprintf ch "%t%a : %a%s\n" ident prnt_str k (BatRef.print printJson')  v sep
      in
      identCount := !identCount + 2;
      BatPrintf.fprintf ch "{\n";
      Object.iter print_one m;
      identCount := !identCount - 2;
      BatPrintf.fprintf ch "%t}" ident
  
let printJson ch m = 
  identCount := 0;
  printJson' ch m;
  identCount := 0
  
(** {3 Pretty style printing. } *)


let rec prettyJson () = function
  | String x -> text "\"" ++ text x ++ text "\""
  | Number i -> text (string_of_num i)
  | Object o -> pretty_of_object !o
  | Array a  -> pretty_of_array !a
  | True  -> text "true"
  | False -> text "false"
  | Null  -> text "null"

and pretty_of_array = function 
  | [] -> text "[]"
  | x::xs -> List.fold_left (fun xs x -> xs ++ text ", " ++ prettyJson () !x) 
                (text "[ " ++ prettyJson () !x) xs ++ text " ]"

and pretty_of_object = function
  | m when Object.is_empty m -> text "{}"
  | m -> 
    let xs = Object.fold (fun k v o -> (text"\""++text k++text"\":"++prettyJson () !v)::o) m [] in
    break++align++text"{ " ++ seq (line++text", ") (fun x -> x) xs ++text"\n}"++unalign


let jsonString x = sprint 40 (prettyJson () x) 

(** {3 Helper functions } *)

(** Access an json value as a map. *)
let objekt = function
  | Object o -> o
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not an object."))

(** Access an json value as an array. *)
let array = function
  | Array o -> o
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not an array."))

(** Access an json value as a string. *)
let string = function
  | String s -> s
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not a string."))

(** Access an json value as a int. *)
let number = function
  | Number s -> int_of_num s
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not a number."))

(** Access an json value as a bool. *)
let bool = function
  | True  -> true
  | False -> false
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not an boolean."))

(** Select a field from an object. *)
let field v f = 
  try Object.find f !v
  with Not_found -> raise (JsonE ("Json Error: field '" ^f^ "' is not contained in object: '"^jsonString (Object v)^"'."))
  
(** Call to [merge x y] returns a [jvalue]  where [x] is updated with values in [y] *)
let rec merge (x:jvalue) (y:jvalue) : jvalue =
  match x, y with 
    | Object m1, Object m2 ->
        let merger k v1 v2 =
          match v1, v2 with
            | Some v1, Some v2 -> Some (ref (merge !v1 !v2))
            | None   , Some v
            | Some v , None    -> Some v
            | None   , None    -> None
        in
        let nm = Object.merge merger !m1 !m2 in
        Object (ref nm)
    | Array l1, Array l2 ->
        let rec zipWith' x y : jvalue ref list =
          match x, y with 
            | x::xs, y::ys    -> (ref (merge !x !y)) :: zipWith' xs ys
            | [], xs | xs, [] -> y
        in
        Array (ref (zipWith' !l1 !l2))
    | _ -> y
  
(** Json value generation. Added for compatibility with the old library. *)
module Build = (* backward compatibility *)
struct
  (** Generate an object form a assoc. list *)
  let objekt o = 
    let rec objekt' = function
      | [] -> Object.empty
      | (k,v)::xs -> Object.add k (ref v) (objekt' xs)
    in 
    Object (ref (objekt' o))
  
  (** Generate a boolean value. *)
  let bool = function
    | true  -> True
    | false -> False

  (** Generate a string value. *)
  let string x = String x 
  (** Generate a int value. *)
  let number n = Number (num_of_int n) 
  (** Generate a array value. *)
  let array x = Array (ref (List.map ref x))
  (** Generate a null. *)  
  let null = Null
end

(** Write a json value to a file. *)
let save_json fn j = 
  BatFile.with_file_out fn (fun c -> printJson c j)
  