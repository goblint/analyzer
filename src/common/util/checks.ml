(**
    Implements a method described in this paper from Raphaël Monat,
    Abdelraouf Ouadjaout and Antoine Miné : https://inria.hal.science/hal-04652657v2

    This allows to track checks performed during the analysis, and to mark whether they are safe,
    unsafe or unknown (resp. Safe, Error, Warning).
*)

module Kind = struct
  type t =
    | Error
    | Warning
    | Safe
  [@@deriving hash, eq, show]

  let is_safe = function
    | Safe -> true
    | _ -> false

  let to_yojson x = `String (match x with
      | Error -> "error"
      | Warning -> "warning"
      | Safe -> "safe")

  let of_yojson = function
    | `String "error" -> Ok Error
    | `String "warning" -> Ok Warning
    | `String "safe" -> Ok Safe
    | kind -> Error ("Checks.Kind.of_yojson: Invalid kind: " ^ Yojson.Safe.to_string kind)
end

module Category = struct
  type t =
    | AssertionFailure
    | InvalidMemoryAccess
    | DivisionByZero
    | IntegerOverflow
    | InvalidPointerComparison
    | InvalidPointerSubtraction
    | DoubleFree
    | NegativeArraySize
    | StubCondition
  [@@deriving hash, eq]

  let to_yojson x = `String (match x with
      | AssertionFailure -> "Assertion failure"
      | InvalidMemoryAccess -> "Invalid memory access"
      | DivisionByZero -> "Division by zero"
      | IntegerOverflow -> "Integer overflow"
      | InvalidPointerComparison -> "Invalid pointer comparison"
      | InvalidPointerSubtraction -> "Invalid pointer subtraction"
      | DoubleFree -> "Double free"
      | NegativeArraySize -> "Negative array size"
      | StubCondition -> "Stub condition")

  let of_yojson = function
    | `String "Assertion failure" -> Ok AssertionFailure
    | `String "Invalid memory access" -> Ok InvalidMemoryAccess
    | `String "Division by zero" -> Ok DivisionByZero
    | `String "Integer overflow" -> Ok IntegerOverflow
    | `String "Invalid pointer comparison" -> Ok InvalidPointerComparison
    | `String "Invalid pointer subtraction" -> Ok InvalidPointerSubtraction
    | `String "Double free" -> Ok DoubleFree
    | `String "Negative array size" -> Ok NegativeArraySize
    | `String "Stub condition" -> Ok StubCondition
    | category -> Error ("Checks.Category.of_yojson: Invalid category: " ^ Yojson.Safe.to_string category)
end

module Check = struct
  type t = {
    kind: Kind.t;
    title: Category.t;
    range: CilType.Location.t option;
    messages: string;
  } [@@deriving make, hash, eq]

  let to_yojson check =
    `Assoc [
      ("kind", Kind.to_yojson check.kind);
      ("title", Category.to_yojson check.title);
      ("range", match check.range with
        | Some loc -> `Assoc [
            ("start", `Assoc [
                ("file", `String loc.file);
                ("line", `Int loc.line);
                ("column", `Int (loc.column - 1))
              ]);
            ("end", `Assoc [
                ("file", `String loc.file);
                ("line", `Int loc.endLine);
                ("column", `Int (loc.endColumn - 1))
              ])
          ]
        | None -> `Null);
      ("messages", `String check.messages)
    ]

  let pp fmt check =
    Format.fprintf fmt "Check: %a: %s at %a"
      Kind.pp check.kind
      check.messages
      (Format.pp_print_option CilType.Location.pp) check.range
end

module CheckMap = Hashtbl.Make (Check)

module CategoryLocationMap = Hashtbl.Make (struct
    type t = Category.t * CilType.Location.t [@@deriving hash, eq]
  end)


let checks_list : (bool ref * unit CheckMap.t) CategoryLocationMap.t = CategoryLocationMap.create 113
(** Store the list of checks raised by the analysis. The [bool ref] stores whether all checks in the map are safe.
    The [unit CheckMap.t] is the set of checks associated with the given [Category.t * CilType.Location.t] pair.
    The [bool] is a reference to avoid having to use the [update] function when updating the value. The [CheckMap.t] is mutable anyway
    so it is not a problem. *)

let add_check check =
  match check.Check.range with
  | Some range -> (
      (* Mark all ranges as synthetic for hash purposes *)
      let range = { range with synthetic = true } in
      let check = { check with range = Some range } in
      let check_key = (check.title, range) in
      match CategoryLocationMap.find_opt checks_list check_key with
      | Some (safe, existing_checks) ->
        if !safe && Kind.is_safe check.kind then
          CheckMap.replace existing_checks check ()
        else if not @@ Kind.is_safe check.kind then (
          if !safe then CheckMap.clear existing_checks;
          safe := false;
          CheckMap.replace existing_checks check ()
        )
      | None ->
        let table = CheckMap.create 10 in
        CheckMap.replace table check ();
        CategoryLocationMap.replace checks_list check_key (ref (Kind.is_safe check.kind), table))
  | None ->
    ()

let check kind title fmt =
  if !AnalysisState.should_warn then (
    let finish doc =
      let loc = Option.map UpdateCil0.getLoc !Node0.current_node in
      let messages = GobPretty.show doc in
      let check = Check.make ~kind ~title ?range:loc ~messages () in
      add_check check in
    GoblintCil.Pretty.gprintf finish fmt)
  else
    GobPretty.igprintf () fmt


let export () =
  `List (
    List.map Check.to_yojson @@ CategoryLocationMap.fold (
      fun _ (checks: (bool ref * unit CheckMap.t)) acc ->
        List.rev_append (CheckMap.to_seq_keys @@ snd checks |> List.of_seq) acc
    ) checks_list []
  )

let error category = check Kind.Error category

let warn category = check Kind.Warning category

let safe category = check Kind.Safe category ""

let safe_msg category = check Kind.Safe category
