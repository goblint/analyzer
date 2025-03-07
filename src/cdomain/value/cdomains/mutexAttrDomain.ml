(** Mutex attribute type domain. *)

module MutexKind =
struct
  include Printable.StdLeaf

  (* NonRec represents any of PTHREAD_MUTEX_ERRORCHECK / PTHREAD_MUTEX_NORMAL / PTHREAD_MUTEX_DEFAULT *)
  (* Once Goblint supports the notion of failing lock operations, this should be replaced with more precise definitions *)
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

include Lattice.FlatConf (struct include Printable.DefaultConf let bot_name = "Uninitialized" let top_name = "Top" end) (MutexKind)

(* Needed because OS X is weird and assigns different constants than normal systems... :( *)
let recursive_int = lazy (
  let res = ref (Z.of_int 2) in (* Use OS X as the default, it doesn't have the enum *)
  GoblintCil.iterGlobals !Cilfacade.current_file (function
      | GEnumTag (einfo, _) ->
        List.iter (fun (name, _, exp, _) ->
            if name = "PTHREAD_MUTEX_RECURSIVE" then
              res := Option.get @@ GoblintCil.getInteger exp
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
    if Z.equal z recursive_int then
      `Lifted MutexKind.Recursive
    else
      `Top
