open Big_int
open Pretty

exception JsonE of string 

module Object = Map.Make (String)

type jvalue =
  | String of string
  | Number of big_int
  | Object of jvalue ref Object.t
  | Array  of jvalue list ref
  | True | False
  | Null

let rec prettyJson = function
  | String x -> text "\"" ++ text x ++ text "\""
  | Number i -> text (string_of_big_int i)
  | Object o -> pretty_of_object o
  | Array a  -> pretty_of_array a
  | True  -> text "true"
  | False -> text "false"
  | Null  -> text "null"

and pretty_of_array x = match !x with
  | [] -> text "[]"
  | x::xs -> List.fold_left (fun xs x -> xs ++ text ", " ++ prettyJson x) (text "[ " ++ prettyJson x) xs ++ text " ]"

and pretty_of_object = function
  | m when Object.is_empty m -> text "{}"
  | m -> 
    let xs = Object.fold (fun k v o -> (text"\""++text k++text"\":"++prettyJson !v)::o) m [] in
    break++align++text"{ " ++ seq (line++text", ") (fun x -> x) xs ++text"\n}"++unalign


let jsonString x = sprint 40 (prettyJson x) 
    
let objekt = function
  | Object o -> o
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not an object."))

let array = function
  | Array o -> o
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not an array."))

let string = function
  | String s -> s
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not a string."))

let bool = function
  | True  -> true
  | False -> false
  | o -> raise (JsonE ("Json Error: '"^jsonString o^"' not an boolean."))

let field v f = 
  try Object.find f v
  with Not_found -> raise (JsonE ("Json Error: field '" ^f^ "' is not contained in object: '"^jsonString (Object v)^"'."))

module Build = (* backward compatibility *)
struct
  let objekt o = 
    let rec objekt' = function
      | [] -> Object.empty
      | (k,v)::xs -> Object.add k (ref v) (objekt' xs)
    in 
    Object (objekt' o)

  let bool = function
    | true  -> True
    | false -> False

  let string x = String x  
  
  let array x = Array (ref x)
end

let save_json fn j = 
  let c = open_out fn in
  output_string c (jsonString j);
  close_out c
  