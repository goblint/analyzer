open GoblintCil

let show = Pretty.sprint ~width:max_int

let sprint f x = show (f () x)

let sprintf (fmt: ('a, unit, Pretty.doc, string) format4): 'a =
  Pretty.gprintf show fmt


open Pretty

(* Parses a format string to generate a nop-function of the correct type. *)
let igprintf (finish: 'b) (format : ('a, unit, doc, 'b) format4) : 'a =
  let format = string_of_format format in
  let flen    = String.length format in
  let fget    = String.unsafe_get format in
  let rec literal acc i =
    let rec skipChars j =
      if j >= flen || (match fget j with '%' | '@' | '\n' -> true | _ -> false) then
        collect nil j
      else
        skipChars (succ j)
    in
    skipChars (succ i)
  and collect (acc: doc) (i: int) =
    if i >= flen then begin
      Obj.magic finish
    end else begin
      let c = fget i in
      if c = '%' then begin
        let j = skip_args (succ i) in
        match fget j with
        '%' -> literal acc j
        | ',' -> collect acc (succ j)
        | 's' | 'c' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u'
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'b' | 'B' ->
          Obj.magic(fun b -> collect nil (succ j))
        | 'L' | 'l' | 'n' -> Obj.magic(fun n -> collect nil (succ (succ j)))
        | 'a' -> Obj.magic(fun pprinter arg -> collect nil (succ j))
        | 't' -> Obj.magic(fun pprinter -> collect nil (succ j))
        | c -> invalid_arg ("dprintf: unknown format %s" ^ String.make 1 c)
      end else if c = '@' then begin
        if i + 1 < flen then begin
          match fget (succ i) with
            '[' | ']' | '!' | '?' | '^' | '@' -> collect nil (i + 2)
          | '<' | '>' -> collect nil (i + 1)
          | c -> invalid_arg ("dprintf: unknown format @" ^ String.make 1 c)
        end else
          invalid_arg "dprintf: incomplete format @"
      end else if c = '\n' then begin
        collect nil (i + 1)
      end else
        literal acc i
    end
  and skip_args j =
    match String.unsafe_get format j with
    '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
               | c -> j
  in
  collect nil 0
