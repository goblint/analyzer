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

(* Needed because OS X is weird and assigns different constants than normal systems... :( *)
let recursive_int = lazy (
  let res = ref None in
  GoblintCil.iterGlobals !Cilfacade.current_file (function
      | GEnumTag (einfo, _) ->
        List.iter (fun (name, exp, _) ->
            if name = "PTHREAD_MUTEX_RECURSIVE" then
              res := GoblintCil.getInteger exp
          ) einfo.eitems
      | _ -> ()
    );
  !res
)


let of_int z =
  if Z.equal z Z.zero then
    `Lifted MutexKind.NonRec
  else
    let recursive_int = Lazy.force recursive_int in
    match recursive_int with
    | Some r when Z.equal z r -> `Lifted MutexKind.Recursive
    | _ -> `Top
