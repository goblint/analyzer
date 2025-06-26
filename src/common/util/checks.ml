module Kind = struct
  type t =
    | Error
    | Warning
    | Success

  let to_yojson x = `String (match x with
      | Error -> "error"
      | Warning -> "warning"
      | Success -> "success")

  let of_yojson = function
    | `String "error" -> Ok Error
    | `String "warning" -> Ok Warning
    | `String "success" -> Ok Success
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

  let to_yojson x = `String (match x with
      | AssersionFaillure -> "Assertion failure"
      | InvalidMemoryAccess -> "Invalid memory access"
      | DivisionByZero -> "Division by zero"
      | IntegerOverflow -> "Integer overflow"
      | InvalidPointerComparison -> "Invalid pointer comparison"
      | InvalidPointerSubtraction -> "Invalid pointer subtraction"
      | DoubleFree -> "Double free"
      | NegativeArraySize -> "Negative array size")

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
    category: Category.t;
    location: CilType.Location.t;
    message: string;
  } [@@deriving yojson, make]

  let checks = ref []

  let check kind category location message =
    let check = make ~kind ~category ~location ~message in
    checks := check :: !checks
end

let error = Check.check Kind.Error

let warning = Check.check Kind.Warning

let success = Check.check Kind.Success
