module MutexKind =
struct
  include Printable.Std

  type t = NonRec | Recursive [@@deriving eq, ord, hash, to_yojson]
  let name () = "mutexKind"
  let show x = match x with
    | NonRec -> "fast/error_checking"
    | Recursive -> "recursive"

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)
end

include Lattice.Flat(MutexKind) (struct let bot_name = "Uninitialized" let top_name = "Top" end)


let of_int z =
  if Z.equal z Z.zero then
    `Lifted MutexKind.NonRec
  else if Z.equal z Z.one then
    `Lifted MutexKind.Recursive
  else
    `Top
