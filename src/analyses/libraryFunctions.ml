(** Tools for dealing with library functions. *)

open Prelude.Ana
open GobConfig

module M = Messages

type categories = [
  | `Malloc       of exp
  | `Calloc       of exp * exp
  | `Realloc      of exp * exp
  | `Assert       of exp
  | `Lock         of bool * bool * bool  (* try? * write? * return  on success *)
  | `Unlock
  | `ThreadCreate of exp * exp * exp (* id * f  * x       *)
  | `ThreadJoin   of exp * exp (* id * ret_var *)
  | `Unknown      of string ]

let osek_renames = ref false

let classify' fn exps =
  let strange_arguments () =
    M.warn "%s arguments are strange!" fn;
    `Unknown fn
  in
  match fn with
  | "pthread_create" ->
    begin match exps with
      | [id;_;fn;x] -> `ThreadCreate (id, fn, x)
      | _ -> strange_arguments ()
    end
  | "pthread_join" ->
    begin match exps with
      | [id; ret_var] -> `ThreadJoin (id, ret_var)
      | _ -> strange_arguments ()
    end
  | "malloc" | "kmalloc" | "__kmalloc" | "usb_alloc_urb" | "__builtin_alloca" ->
    begin match exps with
      | size::_ -> `Malloc size
      | _ -> strange_arguments ()
    end
  | "kzalloc" ->
    begin match exps with
      | size::_ -> `Calloc (Cil.one, size)
      | _ -> strange_arguments ()
    end
  | "calloc" ->
    begin match exps with
      | n::size::_ -> `Calloc (n, size)
      | _ -> strange_arguments ()
    end
  | "realloc" ->
    begin match exps with
      | p::size::_ -> `Realloc (p, size)
      | _ -> strange_arguments ()
    end
  | "assert" ->
    begin match exps with
      | [e] -> `Assert e
      | _ -> M.warn "Assert argument mismatch!"; `Unknown fn
    end
  | "_spin_trylock" | "spin_trylock" | "mutex_trylock" | "_spin_trylock_irqsave"
    -> `Lock(true, true, true)
  | "pthread_mutex_trylock" | "pthread_rwlock_trywrlock"
    -> `Lock (true, true, false)
  | "GetSpinlock" -> `Lock (false, true, true)
  | "ReleaseSpinlock" -> `Unlock
  | "LAP_Se_WaitSemaphore" (* TODO: only handle those when arinc analysis is enabled? *)
  | "_spin_lock" | "_spin_lock_irqsave" | "_spin_lock_bh" | "down_write"
  | "mutex_lock" | "mutex_lock_interruptible" | "_write_lock" | "_raw_write_lock"
  | "pthread_rwlock_wrlock" | "GetResource" | "_raw_spin_lock"
  | "_raw_spin_lock_flags" | "_raw_spin_lock_irqsave"
    -> `Lock (get_bool "sem.lock.fail", true, true)
  | "pthread_mutex_lock" | "__pthread_mutex_lock"
    -> `Lock (get_bool "sem.lock.fail", true, false)
  | "pthread_rwlock_tryrdlock" | "pthread_rwlock_rdlock" | "_read_lock"  | "_raw_read_lock"
  | "down_read"
    -> `Lock (get_bool "sem.lock.fail", false, true)
  | "LAP_Se_SignalSemaphore"
  | "__raw_read_unlock" | "__raw_write_unlock"  | "raw_spin_unlock"
  | "_spin_unlock" | "spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
  | "mutex_unlock" | "ReleaseResource" | "_write_unlock" | "_read_unlock"
  | "pthread_mutex_unlock" | "__pthread_mutex_unlock" | "spin_unlock_irqrestore" | "up_read" | "up_write"
    -> `Unlock
  | x -> `Unknown x

let classify fn exps =
  if not(!osek_renames) then classify' fn exps else classify' (OilUtil.get_api_names fn) exps

type action = [ `Write | `Read ]

module Invalidate =
struct
  [@@@warning "-unused-value-declaration"] (* some functions are not used below *)

  let drop = List.drop
  let keep ns = List.filteri (fun i _ -> List.mem i ns)

  let partition ns x =
    let rec go n =
      function
      | [] -> ([],[])
      | y :: ys ->
        let (i,o) = go (n + 1) ys in
        if List.mem n ns
        then (y::i,   o)
        else (   i,y::o)
    in
    go 1 x

  let writesAllButFirst n f a x =
    match a with
    | `Write -> f a x @ drop n x
    | `Read  -> f a x

  let readsAllButFirst n f a x =
    match a with
    | `Write -> f a x
    | `Read  -> f a x @ drop n x

  let reads ns a x =
    let i, o = partition ns x in
    match a with
    | `Write -> o
    | `Read  -> i

  let writes ns a x =
    let i, o = partition ns x in
    match a with
    | `Write -> i
    | `Read  -> o

  let onlyReads ns a x =
    match a with
    | `Write -> []
    | `Read  -> keep ns x

  let onlyWrites ns a x =
    match a with
    | `Write -> keep ns x
    | `Read  -> []

  let readsWrites rs ws a x =
    match a with
    | `Write -> keep ws x
    | `Read  -> keep rs x

  let readsAll a x =
    match a with
    | `Write -> []
    | `Read  -> x

  let writesAll a x =
    match a with
    | `Write -> x
    | `Read  -> []
end

open Invalidate

(* Data races: which arguments are read/written?
 * We assume that no known functions that are reachable are executed/spawned. For that we use ThreadCreate above. *)
(* WTF: why are argument numbers 1-indexed (in partition)? *)
let invalidate_actions = [
    "GetResource", readsAll;
    "ReleaseResource", readsAll;
    "GetSpinlock", readsAll;
    "ReleaseSpinlock", readsAll;
    "atoi", readsAll;             (*safe*)
    "__builtin_ctz", readsAll;
    "__builtin_ctzl", readsAll;
    "__builtin_ctzll", readsAll;
    "__builtin_clz", readsAll;
    "bzero", writes [1]; (*keep 1*)
    "connect", readsAll;          (*safe*)
    "fclose", readsAll;           (*safe*)
    "fflush", writesAll;          (*unsafe*)
    "fopen", readsAll;            (*safe*)
    "fdopen", writes [1];            (*safe*)
    "setvbuf", writes[2];
    "fprintf", writes [1];          (*keep [1]*)
    "__fprintf_chk", writes [1];    (*keep [1]*)
    "fread", writes [1];            (*keep [1]*)
    "__fread_alias", writes [1];            (*keep [1]*)
    "__fread_chk", writes [1];            (*keep [1]*)
    "utimensat", readsAll;
    "free", writesAll; (*unsafe*)
    "fwrite", readsAll;(*safe*)
    "getopt", writes [2];(*keep [2]*)
    "localtime", readsAll;(*safe*)
    "memcpy", writes [1];(*keep [1]*)
    "__builtin_memcpy", writes [1];(*keep [1]*)
    "mempcpy", writes [1];(*keep [1]*)
    "__builtin___memcpy_chk", writes [1];
    "__builtin___mempcpy_chk", writes [1];
    "memset", writesAll;(*unsafe*)
    "__builtin_memset", writesAll;(*unsafe*)
    "__builtin___memset_chk", writesAll;
    "printf", readsAll;(*safe*)
    "__printf_chk", readsAll;(*safe*)
    "printk", readsAll;(*safe*)
    "perror", readsAll;(*safe*)
    "pthread_mutex_lock", readsAll;(*safe*)
    "pthread_mutex_trylock", readsAll;
    "pthread_mutex_unlock", readsAll;(*safe*)
    "__pthread_mutex_lock", readsAll;(*safe*)
    "__pthread_mutex_trylock", readsAll;
    "__pthread_mutex_unlock", readsAll;(*safe*)
    "__mutex_init", readsAll;(*safe*)
    "mutex_init", readsAll;(*safe*)
    "mutex_lock", readsAll;(*safe*)
    "mutex_lock_interruptible", readsAll;(*safe*)
    "mutex_unlock", readsAll;(*safe*)
    "_spin_lock", readsAll;(*safe*)
    "_spin_unlock", readsAll;(*safe*)
    "_spin_lock_irqsave", readsAll;(*safe*)
    "_spin_unlock_irqrestore", readsAll;(*safe*)
    "pthread_mutex_init", readsAll;(*safe*)
    "pthread_mutex_destroy", readsAll;(*safe*)
    "pthread_mutexattr_settype", readsAll;(*safe*)
    "pthread_mutexattr_init", readsAll;(*safe*)
    "pthread_self", readsAll;(*safe*)
    "read", writes [2];(*keep [2]*)
    "recv", writes [2];(*keep [2]*)
    "scanf",  writesAllButFirst 1 readsAll;(*drop 1*)
    "send", readsAll;(*safe*)
    "snprintf", writes [1];(*keep [1]*)
    "__builtin___snprintf_chk", writes [1];(*keep [1]*)
    "sprintf", writes [1];(*keep [1]*)
    "sscanf", writesAllButFirst 2 readsAll;(*drop 2*)
    "strcmp", readsAll;(*safe*)
    "strftime", writes [1];(*keep [1]*)
    "strlen", readsAll;(*safe*)
    "strncmp", readsAll;(*safe*)
    "strncpy", writes [1];(*keep [1]*)
    "strstr", readsAll;(*safe*)
    "strdup", readsAll;(*safe*)
    "toupper", readsAll;(*safe*)
    "tolower", readsAll;(*safe*)
    "time", writesAll;(*unsafe*)
    "vfprintf", writes [1];(*keep [1]*)
    "__vfprintf_chk", writes [1];(*keep [1]*)
    "vprintf", readsAll;(*safe*)
    "vsprintf", writes [1];(*keep [1]*)
    "write", readsAll;(*safe*)
    "__builtin_va_arg", readsAll;(*safe*)
    "__builtin_va_end", readsAll;(*safe*)
    "__builtin_va_start", readsAll;(*safe*)
    "__ctype_b_loc", readsAll;(*safe*)
    "__errno", readsAll;(*safe*)
    "__errno_location", readsAll;(*safe*)
    "sigfillset", writesAll; (*unsafe*)
    "sigprocmask", writesAll; (*unsafe*)
    "uname", writesAll;(*unsafe*)
    "__builtin_strcmp", readsAll;(*safe*)
    "getopt_long", writesAllButFirst 2 readsAll;(*drop 2*)
    "__strdup", readsAll;(*safe*)
    "strtoul__extinline", readsAll;(*safe*)
    "strtol", writes [2];
    "geteuid", readsAll;(*safe*)
    "opendir", readsAll;  (*safe*)
    "readdir_r", writesAll;(*unsafe*)
    "atoi__extinline", readsAll;(*safe*)
    "getpid", readsAll;(*safe*)
    "fgetc", writesAll;(*unsafe*)
    "getc", writesAll;(*unsafe*)
    "_IO_getc", writesAll;(*unsafe*)
    "closedir", writesAll;(*unsafe*)
    "setrlimit", readsAll;(*safe*)
    "chdir", readsAll;(*safe*)
    "pipe", writesAll;(*unsafe*)
    "close", writesAll;(*unsafe*)
    "setsid", readsAll;(*safe*)
    "strerror_r", writesAll;(*unsafe*)
    "pthread_attr_init", writesAll; (*unsafe*)
    "pthread_attr_setdetachstate", writesAll;(*unsafe*)
    "pthread_attr_setstacksize", writesAll;(*unsafe*)
    "pthread_attr_setscope", writesAll;(*unsafe*)
    "pthread_cond_init", readsAll; (*safe*)
    "pthread_cond_wait", readsAll; (*safe*)
    "pthread_cond_signal", readsAll;(*safe*)
    "pthread_cond_broadcast", readsAll;(*safe*)
    "pthread_cond_destroy", readsAll;(*safe*)
    "__pthread_cond_init", readsAll; (*safe*)
    "__pthread_cond_wait", readsAll; (*safe*)
    "__pthread_cond_signal", readsAll;(*safe*)
    "__pthread_cond_broadcast", readsAll;(*safe*)
    "__pthread_cond_destroy", readsAll;(*safe*)
    "pthread_key_create", writesAll;(*unsafe*)
    "sigemptyset", writesAll;(*unsafe*)
    "sigaddset", writesAll;(*unsafe*)
    "pthread_sigmask", writesAllButFirst 2 readsAll;(*unsafe*)
    "raise", writesAll;(*unsafe*)
    "_strlen", readsAll;(*safe*)
    "__builtin_alloca", readsAll;(*safe*)
    "dlopen", readsAll;(*safe*)
    "dlsym", readsAll;(*safe*)
    "dlclose", readsAll;(*safe*)
    "dlerror", readsAll;(*safe*)
    "stat__extinline", writesAllButFirst 1 readsAll;(*drop 1*)
    "lstat__extinline", writesAllButFirst 1 readsAll;(*drop 1*)
    "__builtin_strchr", readsAll;(*safe*)
    "strcpy", writes [1];(*keep [1]*)
    "__builtin___strcpy", writes [1];(*keep [1]*)
    "__builtin___strcpy_chk", writes [1];(*keep [1]*)
    "strcat", writes [2];(*keep [2]*)
    "getpgrp", readsAll;(*safe*)
    "umount2", readsAll;(*safe*)
    "memchr", readsAll;(*safe*)
    "memmove", writes [2;3];(*keep [2;3]*)
    "__builtin_memmove", writes [2;3];(*keep [2;3]*)
    "__builtin___memmove_chk", writes [2;3];(*keep [2;3]*)
    "waitpid", readsAll;(*safe*)
    "statfs", writes [1;3;4];(*keep [1;3;4]*)
    "mkdir", readsAll;(*safe*)
    "mount", readsAll;(*safe*)
    "open", readsAll;(*safe*)
    "__open_alias", readsAll;(*safe*)
    "__open_2", readsAll;(*safe*)
    "fcntl", readsAll;(*safe*)
    "ioctl", writesAll;(*unsafe*)
    "fstat__extinline", writesAll;(*unsafe*)
    "umount", readsAll;(*safe*)
    "rmdir", readsAll;(*safe*)
    "strrchr", readsAll;(*safe*)
    "scandir", writes [1;3;4];(*keep [1;3;4]*)
    "unlink", readsAll;(*safe*)
    "sched_yield", readsAll;(*safe*)
    "nanosleep", writesAllButFirst 1 readsAll;(*drop 1*)
    "sigdelset", readsAll;(*safe*)
    "sigwait", writesAllButFirst 1 readsAll;(*drop 1*)
    "setlocale", readsAll;(*safe*)
    "bindtextdomain", readsAll;(*safe*)
    "textdomain", readsAll;(*safe*)
    "dcgettext", readsAll;(*safe*)
    "syscall", writesAllButFirst 1 readsAll;(*drop 1*)
    "sysconf", readsAll;
    "fputs", readsAll;(*safe*)
    "fputc", readsAll;(*safe*)
    "fseek", writes[1];
    "fileno", readsAll;
    "ferror", readsAll;
    "ftell", readsAll;
    "putc", readsAll;(*safe*)
    "putw", readsAll;(*safe*)
    "putchar", readsAll;(*safe*)
    "feof", readsAll;(*safe*)
    "__getdelim", writes [3];(*keep [3]*)
    "vsyslog", readsAll;(*safe*)
    "gethostbyname_r", readsAll;(*safe*)
    "__h_errno_location", readsAll;(*safe*)
    "__fxstat", readsAll;(*safe*)
    "getuid", readsAll;(*safe*)
    "strerror", readsAll;(*safe*)
    "readdir", readsAll;(*safe*)
    "openlog", readsAll;(*safe*)
    "getdtablesize", readsAll;(*safe*)
    "umask", readsAll;(*safe*)
    "socket", readsAll;(*safe*)
    "clntudp_create", writesAllButFirst 3 readsAll;(*drop 3*)
    "svctcp_create", readsAll;(*safe*)
    "clntudp_bufcreate", writesAll;(*unsafe*)
    "authunix_create_default", readsAll;(*safe*)
    "writev", readsAll;(*safe*)
    "clnt_broadcast", writesAll;(*unsafe*)
    "clnt_sperrno", readsAll;(*safe*)
    "pmap_unset", writesAll;(*unsafe*)
    "bind", readsAll;(*safe*)
    "svcudp_create", readsAll;(*safe*)
    "svc_register", writesAll;(*unsafe*)
    "sleep", readsAll;(*safe*)
    "usleep", readsAll;
    "svc_run", writesAll;(*unsafe*)
    "dup", readsAll; (*safe*)
    "__builtin_expect", readsAll; (*safe*)
    "vsnprintf", writesAllButFirst 3 readsAll; (*drop 3*)
    "__builtin___vsnprintf", writesAllButFirst 3 readsAll; (*drop 3*)
    "__builtin___vsnprintf_chk", writesAllButFirst 3 readsAll; (*drop 3*)
    "syslog", readsAll; (*safe*)
    "strcasecmp", readsAll; (*safe*)
    "strchr", readsAll; (*safe*)
    "getservbyname", readsAll; (*safe*)
    "__error", readsAll; (*safe*)
    "__maskrune", writesAll; (*unsafe*)
    "inet_addr", readsAll; (*safe*)
    "gethostbyname", readsAll; (*safe*)
    "__builtin_bzero", writes [1]; (*keep [1]*)
    "setsockopt", readsAll; (*safe*)
    "listen", readsAll; (*safe*)
    "getsockname", writes [1;3]; (*keep [1;3]*)
    "getenv", readsAll; (*safe*)
    "execl", readsAll; (*safe*)
    "select", writes [1;5]; (*keep [1;5]*)
    "accept", writesAll; (*keep [1]*)
    "getpeername", writes [1]; (*keep [1]*)
    "times", writesAll; (*unsafe*)
    "timespec_get", writes [1];
    "fgets", writes [1;3]; (*keep [3]*)
    "__fgets_alias", writes [1;3]; (*keep [3]*)
    "__fgets_chk", writes [1;3]; (*keep [3]*)
    "strtoul", readsAll; (*safe*)
    "__tolower", readsAll; (*safe*)
    "signal", writesAll; (*unsafe*)
    "strsignal", readsAll;
    "popen", readsAll; (*safe*)
    "BF_cfb64_encrypt", writes [1;3;4;5]; (*keep [1;3;4,5]*)
    "BZ2_bzBuffToBuffDecompress", writes [3;4]; (*keep [3;4]*)
    "uncompress", writes [3;4]; (*keep [3;4]*)
    "stat", writes [1]; (*keep [1]*)
    "__xstat", writes [1]; (*keep [1]*)
    "__lxstat", writes [1]; (*keep [1]*)
    "remove", readsAll;
    "BZ2_bzBuffToBuffCompress", writes [3;4]; (*keep [3;4]*)
    "compress2", writes [3]; (*keep [3]*)
    "__toupper", readsAll; (*safe*)
    "BF_set_key", writes [3]; (*keep [3]*)
    "memcmp", readsAll; (*safe*)
    "sendto", writes [2;4]; (*keep [2;4]*)
    "recvfrom", writes [4;5]; (*keep [4;5]*)
    "srand", readsAll; (*safe*)
    "rand", readsAll; (*safe*)
    "gethostname", writesAll; (*unsafe*)
    "fork", readsAll; (*safe*)
    "realloc", writesAll;(*unsafe*)
    "setrlimit", readsAll; (*safe*)
    "getrlimit", writes [2]; (*keep [2]*)
    "sem_init", readsAll; (*safe*)
    "sem_destroy", readsAll; (*safe*)
    "sem_wait", readsAll; (*safe*)
    "sem_post", readsAll; (*safe*)
    "PL_NewHashTable", readsAll; (*safe*)
    "__assert_fail", readsAll; (*safe*)
    "assert_failed", readsAll; (*safe*)
    "htonl", readsAll; (*safe*)
    "htons", readsAll; (*safe*)
    "ntohl", readsAll; (*safe*)
    "htons", readsAll; (*safe*)
    "munmap", readsAll;(*safe*)
    "mmap", readsAll;(*safe*)
    "clock", readsAll;
    "pthread_rwlock_wrlock", readsAll;
    "pthread_rwlock_trywrlock", readsAll;
    "pthread_rwlock_rdlock", readsAll;
    "pthread_rwlock_tryrdlock", readsAll;
    "pthread_rwlockattr_destroy", writesAll;
    "pthread_rwlockattr_init", writesAll;
    "pthread_rwlock_destroy", readsAll;
    "pthread_rwlock_init", readsAll;
    "pthread_rwlock_unlock", readsAll;
    "__builtin_object_size", readsAll;
    "__builtin_bswap16", readsAll;
    "__builtin_bswap32", readsAll;
    "__builtin_bswap64", readsAll;
    "__builtin_bswap128", readsAll;
    "__builtin_va_arg_pack_len", readsAll;
    "__open_too_many_args", readsAll;
    "usb_submit_urb", readsAll; (* first argument is written to but according to specification must not be read from anymore *)
    "dev_driver_string", readsAll;
    "dev_driver_string", readsAll;
    "__spin_lock_init", writes [1];
    "kmem_cache_create", readsAll;
    "pthread_create", onlyWrites [0; 2]; (* TODO: onlyWrites/keep is 0-indexed now, WTF? *)
    "__builtin_prefetch", readsAll;
    "idr_pre_get", readsAll;
    "zil_replay", writes [1;2;3;5];
    "__VERIFIER_nondet_int", readsAll; (* no args, declare invalidate actions to prevent invalidating globals when extern in regression tests *)
    (* no args, declare invalidate actions to prevent invalidating globals *)
    "__VERIFIER_atomic_begin", readsAll;
    "__VERIFIER_atomic_end", readsAll;
    (* prevent base from spawning ARINC processes early, handled by arinc/extract_arinc *)
    (* "LAP_Se_SetPartitionMode", writes [2]; *)
    "LAP_Se_CreateProcess", writes [2; 3];
    "LAP_Se_CreateErrorHandler", writes [2; 3];
    "isatty", readsAll;
    "setpriority", readsAll;
    "getpriority", readsAll;
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
        let f (k, v) = Hashtbl.add hash k v in
        List.iter f invalidate_actions;
        processed_table := (Some hash);
        hash
      end
    | Some x -> x
  in
  if Hashtbl.mem tbl name
  then Some (Hashtbl.find tbl name)
  else None

let threadSafe =
  let rec threadSafe n ns xs =
    match ns, xs with
    | n'::ns, x::xs when n=n' -> mone::threadSafe (n+1) ns xs
    | n'::ns, x::xs -> x::threadSafe (n+1) (n'::ns) xs
    | _ -> xs
  in
  threadSafe 1

let thread_safe_fn =
  ["strerror", threadSafe [1];
   "fprintf",  threadSafe [1];
   "fgets",    threadSafe [3];
   "strerror_r", threadSafe [1];
   "fclose", threadSafe [1]
  ]

let get_threadsafe_inv_ac name =
  try
    let f = List.assoc name thread_safe_fn in
    match get_invalidate_action name with
    | Some g -> Some (fun a xs -> g a (f xs))
    | None -> Some (fun a xs -> f xs)
  with Not_found -> get_invalidate_action name



let lib_funs = ref (Set.String.of_list ["list_empty"; "__raw_read_unlock"; "__raw_write_unlock"; "spin_trylock"])
let add_lib_funs funs = lib_funs := List.fold_right Set.String.add funs !lib_funs
let use_special fn_name = Set.String.mem fn_name !lib_funs

let effects: (string -> Cil.exp list -> (Cil.lval * ValueDomain.Compound.t) list option) list ref = ref []
let add_effects f = effects := f :: !effects
let effects_for fname args = List.filter_map (fun f -> f fname args) !effects

let kernel_safe_uncalled = Set.String.of_list ["__inittest"; "init_module"; "__exittest"; "cleanup_module"]
let kernel_safe_uncalled_regex = List.map Str.regexp ["__check_.*"]
let is_safe_uncalled fn_name =
  Set.String.mem fn_name kernel_safe_uncalled ||
  List.exists (fun r -> Str.string_match r fn_name 0) kernel_safe_uncalled_regex
