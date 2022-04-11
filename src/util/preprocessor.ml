open Prelude

let bad_cpp_version_regexp = Str.regexp_case_fold "clang\\|apple\\|darwin"

let is_bad name =
  let cpp_in = Unix.open_process_in (name ^ " --version") in
  let cpp_version = IO.read_all cpp_in in
  let r = match Str.search_forward bad_cpp_version_regexp cpp_version 0 with
    | exception Not_found -> false
    | _ -> true
  in
  if GobConfig.get_bool "dbg.verbose" then
    Printf.printf "Preprocessor %s: is_bad=%B\n" name r;
  r

let compgen prefix =
  let bash_command = Filename.quote ("compgen -c " ^ prefix) in
  let compgen = Unix.open_process_in ("bash -c " ^ bash_command) in
  IO.lines_of compgen
  |> List.of_enum

let cpp =
  let is_good name = not (is_bad name) in
  let default = "cpp" in
  lazy (
    let cpp_path = GobConfig.get_string "exp.cpp-path" in
    if cpp_path <> "" then
      cpp_path (* explicit option overrides is_good check *)
    else if is_good default then
      default
    else (
      compgen "cpp-" (* only run compgen if default was bad *)
      |> List.find_exn is_good (Failure "No good preprocessor (cpp) found")
    )
  )

let get_cpp () = Lazy.force cpp

module FpathH = Hashtbl.Make (GobFpath)
let dependencies: bool Fpath.Map.t FpathH.t = FpathH.create 3 (* bool is system_header *)

let dependencies_to_yojson () =
  dependencies
  |> FpathH.enum
  |> Enum.map (fun (p, deps) ->
      let deps' =
        deps
        |> Fpath.Map.bindings
        |> List.filter_map (function
            | (dep, false) -> Some dep
            | (_, true) -> None
          )
        |> [%to_yojson: GobFpath.t list]
      in
      (Fpath.to_string p, deps')
    )
  |> List.of_enum
  |> (fun l -> `Assoc l)
