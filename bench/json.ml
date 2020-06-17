open BatteriesExceptionless

(* add to_yojson to common data structures *)
module Set = struct
  include Set
  let to_yojson poly_a x = [%to_yojson: 'a list] (elements x)
end
module Map = struct
  include Map
  let to_yojson poly_k poly_v x = [%to_yojson: ('k * 'v) list] (bindings x)
end

module type Test = sig
  val name : string
  type t
  val d : t
  val to_yojson : t -> Yojson.Safe.t
end

module AssocList : Test = struct
  let name = "AssocList"
  type t = (int * string) list [@@deriving yojson]
  let d = [1, "foo"; 2, "bar"; 3, "bot"]
end

module PolyMap : Test = struct
  let name = "PolyMap"
  type t = (int, string) Map.t [@@deriving to_yojson]
  let d = Map.(add 1 "foo" @@ add 2 "bar" @@ add 3 "bot" @@ empty)
end

module Goblint : Test = struct
  let name = "Goblint"

  type t = (csvar, abs_state) Map.t
  and csvar = location * context
  and location = int
  and context = [`Top]
  and abs_state = [`Dead | `State of (cvar, abs_val) Map.t]
  and cvar = string
  and abs_val = [`Int of int | `Addr of cvar Set.t | `Top]
  [@@deriving to_yojson]

  let d : t =
    let open Map in
    let s1 = add "x" (`Int 1) @@ add "y" (`Addr (Set.add "x" Set.empty)) empty in
    let s2 = add "x" (`Int 2) @@ add "y" (`Addr (Set.add "x" Set.empty)) empty in
    add (1, `Top) (`State s1) @@
    add (2, `Top) (`State s2) @@
    add (3, `Top) `Dead @@
    empty
end

let rec bson_of_yojson = let open Bson in function
    | `Assoc x -> create_doc_element (List.fold_left (fun doc (k,v) -> add_element k (bson_of_yojson v) doc) empty x)
    | `Bool x -> create_boolean x
    | `Float x -> create_double x
    | `Int x -> create_int64 (Int64.of_int x)
    | `Intlit x -> failwith "TODO"
    | `List x -> create_list (List.map bson_of_yojson x)
    | `Null -> create_null ()
    | `String x -> create_string x
    | `Tuple x -> failwith "TODO"
    | `Variant x -> failwith "TODO"

let test (module X : Test) n =
  let open Printf in
  printf "\nTesting module %s with %d iterations\n" X.name n;
  let o,p = File.open_temporary_out ~mode:[`delete_on_exit] () in
  print_endline @@ "Start json from ocaml";
  let t = Unix.gettimeofday () in
  for i = 1 to n do
    ignore (X.to_yojson X.d)
  done;
  printf "Done in %f s\n" (Unix.gettimeofday () -. t);
  print_endline @@ "Start json string from ocaml";
  let t = Unix.gettimeofday () in
  for i = 1 to n do
    ignore (Yojson.Safe.to_string (X.to_yojson X.d))
  done;
  printf "Done in %f s\n" (Unix.gettimeofday () -. t);
  let s = Yojson.Safe.to_string (X.to_yojson X.d) in
  print_endline @@ "Start writing computed json string to " ^ p;
  let t = Unix.gettimeofday () in
  for i = 1 to n do
    fprintf o "%s\n" s
  done;
  printf "Done in %f s\n" (Unix.gettimeofday () -. t);
  let json = X.to_yojson X.d in
  print_endline @@ "Start generating bson from json";
  let t = Unix.gettimeofday () in
  for i = 1 to n do
    ignore (bson_of_yojson json)
  done;
  printf "Done in %f s\n" (Unix.gettimeofday () -. t)

let () =
  test (module AssocList) (10*1000*1000);
  test (module PolyMap)   (10*1000*1000);
  test (module Goblint)   (10*1000*1000)