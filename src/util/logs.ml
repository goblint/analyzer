module type Kind =
sig
  type b
  type c
  val log: ('a, b, c, unit) format4 -> 'a
end

module PrettyKind =
struct
  open GoblintCil

  type b = unit
  type c = Pretty.doc
  let log fmt =
    (* Pretty.eprintf returns doc instead of unit *)
    let finish doc =
      Pretty.fprint stderr ~width:max_int doc;
      prerr_newline ()
    in
    Pretty.gprintf finish fmt
end

module FormatKind =
struct
  type b = Format.formatter
  type c = unit
  let log fmt =
    let finish ppf =
      Format.fprintf ppf "\n%!"
    in
    Format.kfprintf finish Format.err_formatter fmt
end


module MakeKind (Kind: Kind) =
struct
  open Kind

  let log = log

  let debug = log
  let info = log
  let warn = log
  let error = log
end

module Pretty = MakeKind (PrettyKind)
module Format = MakeKind (FormatKind)

include Pretty (* current default *)
