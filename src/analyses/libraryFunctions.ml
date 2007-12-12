open Cil

(* transformation of argument list *)
type t 
  = Drop of int
  | Keep of (int list)
  | Safe
  | Unsafe

(* converts the tranformation to function on argument list *)
let interpret (x : t) (args : exp list) : (exp list) =
  match x with
    (* first n are safe *)
    | Drop n -> begin
        let rec drop n xs =
          match n,xs with
            | (0, _) -> xs
            | (_, y :: ys) -> drop (n - 1) ys
            | _ -> []
        in
          drop n args
      end
    (* all is safe: Safe == Drop 0 *)
    | Safe -> []
    (* which arguments must to be invalidated *)
    | Keep ns -> begin
        let rec go n =
          function
            | [] -> []
            | y :: ys -> if List.mem n ns
              then y :: go (n + 1) ys
              else      go (n + 1) ys
        in
          go 1 args
      end
    (* Unsafe == Keep [] *)
    | Unsafe -> args

(* just add your library functions here *)
let invalidate_actions = [
  ("atoi", Safe);
  ("bzero", Keep [1]);
  ("connect", Safe);
  ("fclose", Safe);
  ("fflush", Unsafe);
  ("fopen", Safe);
  ("fprintf", Keep [1]);
  ("fread", Keep [1]);
  ("free", Unsafe);
  ("fwrite", Safe);
  ("getopt", Keep [2]);
  ("localtime", Safe);
  ("memcpy", Keep [1]);
  ("memset", Unsafe);
  ("printf", Safe);
  ("pthread_mutex_lock", Safe);
  ("pthread_mutex_unlock", Safe);
  ("read", Keep [2]);
  ("recv", Keep [2]);
  ("scanf",  Drop 1);
  ("send", Safe);
  ("snprintf", Keep [1]);
  ("sprintf", Keep [1]);
  ("sscanf", Drop 2);
  ("strcmp", Safe);
  ("strftime", Keep [1]);
  ("strlen", Safe);
  ("strncmp", Safe);
  ("strncpy", Keep [1]);
  ("strstr", Safe);
  ("time", Unsafe);
  ("vfprintf", Keep [1]);
  ("vprintf", Safe);
  ("vsprintf", Keep [1]);
  ("write", Safe);
  ("__builtin_va_arg", Safe);
  ("__builtin_va_end", Safe);
  ("__builtin_va_start", Safe);
  ("__ctype_b_loc", Safe);
  ("__errno", Safe);
  ("__errno_location", Safe);
]

(* used by get_invalidate_action to make sure
 * that hash of invalidates is built only once
 *
 * Hashtable from strings to functions of type (exp list -> exp list)
 *)
let processed_table = ref None

let get_invalidate_action name =
  let tbl = match !processed_table with
    | None -> begin
        let hash = Hashtbl.create 113 in
        let f (k, v) = Hashtbl.add hash k (interpret v) in
          List.iter f invalidate_actions;
          processed_table := (Some hash);
          hash
      end
    | Some x -> x
  in
    if Hashtbl.mem tbl name
    then Some (Hashtbl.find tbl name)
    else None
