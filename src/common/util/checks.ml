module Kind = struct
  type t =
    | Error
    | Warning
    | Safe

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
    | kind -> Error ("Checks.Kind.of_yojson: Invalid kind :" ^ Yojson.Safe.to_string kind)
end

module Category = struct
  type t = 
    | AssersionFaillure
    | InvalidMemoryAccess
    | DivisionByZero
    | IntegerOverflow
    | InvalidPointerComparison
    | InvalidPointerSubtraction
    | DoubleFree
    | NegativeArraySize
    | StubCondition

  let to_yojson x = `String (match x with
      | AssersionFaillure -> "Assertion failure"
      | InvalidMemoryAccess -> "Invalid memory access"
      | DivisionByZero -> "Division by zero"
      | IntegerOverflow -> "Integer overflow"
      | InvalidPointerComparison -> "Invalid pointer comparison"
      | InvalidPointerSubtraction -> "Invalid pointer subtraction"
      | DoubleFree -> "Double free"
      | NegativeArraySize -> "Negative array size"
      | StubCondition -> "Stub condition")

  let of_yojson = function
    | `String "Assertion failure" -> Ok AssersionFaillure
    | `String "Invalid memory access" -> Ok InvalidMemoryAccess
    | `String "Division by zero" -> Ok DivisionByZero
    | `String "Integer overflow" -> Ok IntegerOverflow
    | `String "Invalid pointer comparison" -> Ok InvalidPointerComparison
    | `String "Invalid pointer subtraction" -> Ok InvalidPointerSubtraction
    | `String "Double free" -> Ok DoubleFree
    | `String "Negative array size" -> Ok NegativeArraySize
    | category -> Error ("Checks.Category.of_yojson: Invalid category :" ^ Yojson.Safe.to_string category)
end

module Check = struct
  type t = {
    kind: Kind.t;
    title: Category.t;
    range: CilType.Location.t option;
    messages: string;
  } [@@deriving yojson, make]

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

  type key = (Category.t * CilType.Location.t option) 
  let checks : (key, t list) Hashtbl.t = Hashtbl.create 113

  let add_check check =
    if !AnalysisState.should_warn then (
      let check_key = (check.title, check.range) in
      match Hashtbl.find_opt checks check_key with
      | Some existing_checks ->
        if Kind.is_safe check.kind then
          ()
        else if Kind.is_safe (List.hd existing_checks).kind then
          Hashtbl.replace checks check_key [check]
        else
          Hashtbl.replace checks check_key (check :: existing_checks)
      | None ->
        Hashtbl.add checks check_key [check])

  let check kind title =
    let finish doc =
      let loc = Option.map UpdateCil0.getLoc !Node0.current_node in
      let messages = GobPretty.show doc in
      let check = make ~kind ~title ?range:loc ~messages () in
      add_check check
    in GoblintCil.Pretty.gprintf finish  


  let export () =
    `List (List.map to_yojson @@ Hashtbl.fold (fun _ checks acc -> List.rev_append checks acc) checks [])
end

let error category = Check.check Kind.Error category

let warn category = Check.check Kind.Warning category

let safe category = Check.check Kind.Safe category ""
