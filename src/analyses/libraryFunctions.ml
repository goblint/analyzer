(** Tools for dealing with library functions. *)

open Cil
open GobConfig
open Goblintutil

module M = Messages

type categories = [
  | `Malloc       
  | `Calloc       
  | `Assert       of exp
  | `Lock         of bool * bool (* try? * write? *)
  | `Unlock       
  | `ThreadCreate of exp * exp (* f  * x       *)
  | `ThreadJoin   of exp * exp (* id * ret_var *)
  | `Unknown      of string ]
 
let osek_renames = ref false
 
let classify' fn exps =
  match fn with
    | "pthread_create" -> 
        begin match exps with
          | [_;_;fn;x] -> `ThreadCreate (fn, x)
          | _ -> M.bailwith "pthread_create arguments are strange."
        end 
    | "pthread_join" -> 
        begin match exps with
          | [id; ret_var] -> `ThreadJoin (id, ret_var)
          | _ -> M.bailwith "pthread_join arguments are strange!"
        end
    | "malloc" | "kmalloc"  | "kzalloc" | "__kmalloc" | "usb_alloc_urb" -> `Malloc 
    | "calloc" -> `Calloc
    | "assert" ->  
        begin match exps with
            | [e] -> `Assert e 
            | _ -> M.bailwith "Assert argument mismatch!"
        end
    | "_spin_trylock" | "_spin_trylock_irqsave" | "pthread_mutex_trylock" 
    | "pthread_rwlock_trywrlock" | "mutex_trylock"
        -> `Lock (true, true)
    | "LAP_Se_WaitSemaphore"
    | "_spin_lock" | "_spin_lock_irqsave" | "_spin_lock_bh" | "down_write"
    | "mutex_lock" | "mutex_lock_interruptible" | "_write_lock" | "_raw_write_lock"
    | "pthread_mutex_lock" | "__pthread_mutex_lock" | "pthread_rwlock_wrlock" | "GetResource" 
    | "_raw_spin_lock" | "_raw_spin_lock_flags" | "_raw_spin_lock_irqsave" 
        -> `Lock (get_bool "exp.failing-locks", true) 
    | "pthread_rwlock_tryrdlock" | "pthread_rwlock_rdlock" | "_read_lock"  | "_raw_read_lock"
    | "down_read"
        -> `Lock (get_bool "exp.failing-locks", false) 
    | "LAP_Se_SignalSemaphore"
    | "__raw_read_unlock" | "__raw_write_unlock"  | "raw_spin_unlock"
    | "_spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
    | "mutex_unlock" | "ReleaseResource" | "_write_unlock" | "_read_unlock"
    | "pthread_mutex_unlock" | "__pthread_mutex_unlock" | "spin_unlock_irqrestore" | "up_read" | "up_write"
        -> `Unlock        
    | x -> `Unknown x

let classify fn exps =
  if not(!osek_renames) then classify' fn exps else classify' (OilUtil.get_api_names fn) exps

type action = [ `Write | `Read ]
  
let safe   x = []
let unsafe x = x
let rec drop n xs =
  match n,xs with
    | (0, _) -> xs
    | (_, y :: ys) -> drop (n - 1) ys
    | _ -> []
      

let keep ns x = 
  let rec go n =
    function
      | [] -> []
      | y :: ys -> if List.mem n ns
        then y :: go (n + 1) ys
        else      go (n + 1) ys
  in
    go 1 x

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

(* just add your library functions here *)
let invalidate_actions = [
  ("GetResource", readsAll);
  ("ReleaseResource", readsAll);
  ("atoi", readsAll);             (*safe*)
  ("bzero", writes [1]); (*keep 1*)
  ("connect", readsAll);          (*safe*)
  ("fclose", readsAll);           (*safe*)
  ("fflush", writesAll);          (*unsafe*)
  ("fopen", readsAll);            (*safe*)
  ("fprintf", writes [1]);          (*keep [1]*)
  ("fread", writes [1]);            (*keep [1]*)
  ("free", writesAll); (*unsafe*)
  ("fwrite", readsAll);(*safe*)
  ("getopt", writes [2]);(*keep [2]*)
  ("localtime", readsAll);(*safe*)
  ("memcpy", writes [1]);(*keep [1]*)
  ("__builtin___memcpy_chk", writes [1]);
  ("memset", writesAll);(*unsafe*)
  ("printf", readsAll);(*safe*)
  ("printk", readsAll);(*safe*)
  ("perror", readsAll);(*safe*)
  ("pthread_mutex_lock", readsAll);(*safe*)
  ("pthread_mutex_trylock", readsAll);
  ("pthread_mutex_unlock", readsAll);(*safe*)
  ("__pthread_mutex_lock", readsAll);(*safe*)
  ("__pthread_mutex_trylock", readsAll);
  ("__pthread_mutex_unlock", readsAll);(*safe*)
  ("__mutex_init", readsAll);(*safe*)
  ("mutex_init", readsAll);(*safe*)
  ("mutex_lock", readsAll);(*safe*)
  ("mutex_lock_interruptible", readsAll);(*safe*)
  ("mutex_unlock", readsAll);(*safe*)
  ("_spin_lock", readsAll);(*safe*)
  ("_spin_unlock", readsAll);(*safe*)
  ("_spin_lock_irqsave", readsAll);(*safe*)
  ("_spin_unlock_irqrestore", readsAll);(*safe*)
  ("pthread_mutex_init", readsAll);(*safe*)
  ("pthread_mutex_destroy", readsAll);(*safe*)
  ("pthread_self", readsAll);(*safe*)
  ("read", writes [2]);(*keep [2]*)
  ("recv", writes [2]);(*keep [2]*)
  ("scanf",  writesAllButFirst 1 readsAll);(*drop 1*)
  ("send", readsAll);(*safe*)
  ("snprintf", writes [1]);(*keep [1]*)
  ("sprintf", writes [1]);(*keep [1]*)
  ("sscanf", writesAllButFirst 2 readsAll);(*drop 2*)
  ("strcmp", readsAll);(*safe*)
  ("strftime", writes [1]);(*keep [1]*)
  ("strlen", readsAll);(*safe*)
  ("strncmp", readsAll);(*safe*)
  ("strncpy", writes [1]);(*keep [1]*)
  ("strstr", readsAll);(*safe*)
  ("strdup", readsAll);(*safe*)
  ("toupper", readsAll);(*safe*)
  ("tolower", readsAll);(*safe*)
  ("time", writesAll);(*unsafe*)
  ("vfprintf", writes [1]);(*keep [1]*)
  ("vprintf", readsAll);(*safe*)
  ("vsprintf", writes [1]);(*keep [1]*)
  ("write", readsAll);(*safe*)
  ("__builtin_va_arg", readsAll);(*safe*)
  ("__builtin_va_end", readsAll);(*safe*)
  ("__builtin_va_start", readsAll);(*safe*)
  ("__ctype_b_loc", readsAll);(*safe*)
  ("__errno", readsAll);(*safe*)
  ("__errno_location", readsAll);(*safe*)
  ("sigfillset", writesAll); (*unsafe*)
  ("sigprocmask", writesAll); (*unsafe*)
  ("uname", writesAll);(*unsafe*)
  ("__builtin_strcmp", readsAll);(*safe*)
  ("getopt_long", writesAllButFirst 2 readsAll);(*drop 2*)
  ("__strdup", readsAll);(*safe*)
  ("strtoul__extinline", readsAll);(*safe*)
  ("geteuid", readsAll);(*safe*)
  ("opendir", readsAll);  (*safe*)
  ("readdir_r", writesAll);(*unsafe*)
  ("atoi__extinline", readsAll);(*safe*)
  ("getpid", readsAll);(*safe*)
  ("fgetc", writesAll);(*unsafe*)
  ("closedir", writesAll);(*unsafe*)
  ("setrlimit", readsAll);(*safe*)
  ("chdir", readsAll);(*safe*)
  ("pipe", writesAll);(*unsafe*)
  ("close", writesAll);(*unsafe*)
  ("setsid", readsAll);(*safe*)
  ("strerror_r", writesAll);(*unsafe*)
  ("pthread_attr_init", writesAll); (*unsafe*)
  ("pthread_attr_setdetachstate", writesAll);(*unsafe*)
  ("pthread_attr_setstacksize", writesAll);(*unsafe*)
  ("pthread_attr_setscope", writesAll);(*unsafe*)
  ("pthread_cond_init", readsAll); (*safe*)
  ("pthread_cond_wait", readsAll); (*safe*)
  ("pthread_cond_signal", readsAll);(*safe*)
  ("pthread_cond_broadcast", readsAll);(*safe*)
  ("__pthread_cond_init", readsAll); (*safe*)
  ("__pthread_cond_wait", readsAll); (*safe*)
  ("__pthread_cond_signal", readsAll);(*safe*)
  ("__pthread_cond_broadcast", readsAll);(*safe*)
  ("pthread_key_create", writesAll);(*unsafe*)
  ("sigemptyset", writesAll);(*unsafe*)
  ("sigaddset", writesAll);(*unsafe*)
  ("pthread_sigmask", writesAllButFirst 2 readsAll);(*unsafe*)
  ("raise", writesAll);(*unsafe*)
  ("_strlen", readsAll);(*safe*)
  ("__builtin_alloca", readsAll);(*safe*)
  ("dlopen", readsAll);(*safe*)
  ("dlsym", readsAll);(*safe*)
  ("dlclose", readsAll);(*safe*)
  ("dlerror", readsAll);(*safe*)
  ("stat__extinline", writesAllButFirst 1 readsAll);(*drop 1*)
  ("lstat__extinline", writesAllButFirst 1 readsAll);(*drop 1*)
  ("__builtin_strchr", readsAll);(*safe*)
  ("strcpy", writes [1]);(*keep [1]*)
  ("strcat", writes [2]);(*keep [2]*)
  ("getpgrp", readsAll);(*safe*)
  ("umount2", readsAll);(*safe*)
  ("memchr", readsAll);(*safe*)
  ("memmove", writes [2;3]);(*keep [2;3]*)
  ("waitpid", readsAll);(*safe*)
  ("statfs", writes [1;3;4]);(*keep [1;3;4]*)
  ("mkdir", readsAll);(*safe*)
  ("mount", readsAll);(*safe*)
  ("open", readsAll);(*safe*)
  ("fcntl", readsAll);(*safe*)
  ("ioctl", writesAll);(*unsafe*)
  ("fstat__extinline", writesAll);(*unsafe*)
  ("umount", readsAll);(*safe*)
  ("rmdir", readsAll);(*safe*)
  ("strrchr", readsAll);(*safe*)
  ("scandir", writes [1;3;4]);(*keep [1;3;4]*)
  ("unlink", readsAll);(*safe*)
  ("sched_yield", readsAll);(*safe*)
  ("nanosleep", writesAllButFirst 1 readsAll);(*drop 1*)
  ("sigdelset", readsAll);(*safe*)
  ("sigwait", writesAllButFirst 1 readsAll);(*drop 1*)
  ("setlocale", readsAll);(*safe*)
  ("bindtextdomain", readsAll);(*safe*)
  ("textdomain", readsAll);(*safe*)
  ("dcgettext", readsAll);(*safe*)
  ("syscall", writesAllButFirst 1 readsAll);(*drop 1*)
  ("fputs", readsAll);(*safe*)
  ("fputc", readsAll);(*safe*)
  ("putc", readsAll);(*safe*)
  ("putw", readsAll);(*safe*)
  ("putchar", readsAll);(*safe*)
  ("feof", readsAll);(*safe*)
  ("__getdelim", writes [3]);(*keep [3]*)
  ("vsyslog", readsAll);(*safe*)
  ("gethostbyname_r", readsAll);(*safe*)
  ("__h_errno_location", readsAll);(*safe*)
  ("__fxstat", readsAll);(*safe*)
  ("getuid", readsAll);(*safe*)
  ("strerror", readsAll);(*safe*)
  ("readdir", readsAll);(*safe*)
  ("openlog", readsAll);(*safe*)
  ("getdtablesize", readsAll);(*safe*)
  ("umask", readsAll);(*safe*)
  ("socket", readsAll);(*safe*)
  ("clntudp_create", writesAllButFirst 3 readsAll);(*drop 3*)
  ("svctcp_create", readsAll);(*safe*)
  ("clntudp_bufcreate", writesAll);(*unsafe*)
  ("authunix_create_default", readsAll);(*safe*)
  ("writev", readsAll);(*safe*)
  ("clnt_broadcast", writesAll);(*unsafe*)
  ("clnt_sperrno", readsAll);(*safe*)
  ("pmap_unset", writesAll);(*unsafe*)
  ("bind", readsAll);(*safe*)
  ("svcudp_create", readsAll);(*safe*)
  ("svc_register", writesAll);(*unsafe*)
  ("sleep", readsAll);(*safe*)
  ("svc_run", writesAll);(*unsafe*)
  ("dup", readsAll); (*safe*)
  ("__builtin_expect", readsAll); (*safe*)
  ("vsnprintf", writesAllButFirst 3 readsAll); (*drop 3*)
  ("syslog", readsAll); (*safe*)
  ("strcasecmp", readsAll); (*safe*)
  ("strchr", readsAll); (*safe*)
  ("getservbyname", readsAll); (*safe*)
  ("__error", readsAll); (*safe*)
  ("__maskrune", writesAll); (*unsafe*)
  ("inet_addr", readsAll); (*safe*)
  ("gethostbyname", readsAll); (*safe*)
  ("__builtin_bzero", writes [1]); (*keep [1]*)
  ("setsockopt", readsAll); (*safe*)
  ("listen", readsAll); (*safe*)
  ("getsockname", writes [1;3]); (*keep [1;3]*)
  ("getenv", readsAll); (*safe*)
  ("execl", readsAll); (*safe*)
  ("select", writes [1;5]); (*keep [1;5]*)
  ("accept", writesAll); (*keep [1]*)
  ("getpeername", writes [1]); (*keep [1]*)
  ("times", writesAll); (*unsafe*)
  ("fgets", writes [1;3]); (*keep [3]*)
  ("strtoul", readsAll); (*safe*)
  ("__tolower", readsAll); (*safe*)
  ("signal", writesAll); (*unsafe*)
  ("popen", readsAll); (*safe*)
  ("BF_cfb64_encrypt", writes [1;3;4;5]); (*keep [1;3;4,5]*)
  ("BZ2_bzBuffToBuffDecompress", writes [3;4]); (*keep [3;4]*)
  ("uncompress", writes [3;4]); (*keep [3;4]*)
  ("stat", writes [1]); (*keep [1]*)
  ("BZ2_bzBuffToBuffCompress", writes [3;4]); (*keep [3;4]*)
  ("compress2", writes [3]); (*keep [3]*)
  ("__toupper", readsAll); (*safe*)
  ("BF_set_key", writes [3]); (*keep [3]*)
  ("memcmp", readsAll); (*safe*)
  ("sendto", writes [2;4]); (*keep [2;4]*)
  ("recvfrom", writes [4;5]); (*keep [4;5]*)
  ("srand", readsAll); (*safe*)
  ("rand", readsAll); (*safe*)
  ("gethostname", writesAll); (*unsafe*)
  ("fork", readsAll); (*safe*)
  ("realloc", writesAll);(*unsafe*)
  ("setrlimit", readsAll); (*safe*)
  ("getrlimit", writes [2]); (*keep [2]*)
  ("sem_init", readsAll); (*safe*)
  ("sem_destroy", readsAll); (*safe*)
  ("sem_wait", readsAll); (*safe*)
  ("sem_post", readsAll); (*safe*)
  ("PL_NewHashTable", readsAll); (*safe*)
  ("__assert_fail", readsAll); (*safe*)
  ("assert_failed", readsAll); (*safe*)
  ("htonl", readsAll); (*safe*)
  ("htons", readsAll); (*safe*)
  ("ntohl", readsAll); (*safe*)
  ("htons", readsAll); (*safe*)
  ("munmap", readsAll);(*safe*)
  ("mmap", readsAll);(*safe*)
  ("pthread_rwlock_wrlock", readsAll);
  ("pthread_rwlock_trywrlock", readsAll);
  ("pthread_rwlock_rdlock", readsAll);
  ("pthread_rwlock_tryrdlock", readsAll);
  ("pthread_rwlockattr_destroy", writesAll);
  ("pthread_rwlockattr_init", writesAll);
  ("pthread_rwlock_destroy", readsAll);
  ("pthread_rwlock_init", readsAll);
  ("pthread_rwlock_unlock", readsAll);
  ("__builtin_object_size", readsAll);
  ("usb_submit_urb", readsAll); (* first argument is written to but according to specification must not be read from anymore *)
  ("dev_driver_string", readsAll);
  ("dev_driver_string", readsAll);
  ("__spin_lock_init", writes [1]);
  ("kmem_cache_create", readsAll);
  ("pthread_create", writes [1]);
  ("__builtin_prefetch", readsAll);
  ("idr_pre_get", readsAll);
  ("zil_replay", writes [1;2;3;5]);
  ("LAP_Se_Start", writesAll)
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


module StringSet = Set.Make(String)
let lib_funs = ref (List.fold_right StringSet.add ["list_empty"; "kzalloc"; "kmalloc"; "__raw_read_unlock"; "__raw_write_unlock"; "spinlock_check"; "spin_unlock_irqrestore"] StringSet.empty)

let use_special fn_name = StringSet.mem fn_name !lib_funs

let add_lib_funs funs = lib_funs := List.fold_right StringSet.add funs !lib_funs

let _ = add_lib_funs ["LAP_Se_TimedWait";"LAP_Se_RaiseApplicationError";"LAP_Se_GetErrorStatus";"LAP_Se_CreateErrorHandler";"LAP_Se_GetEventStatus";"LAP_Se_GetEventId";"LAP_Se_WaitEvent";"LAP_Se_ResetEvent";"LAP_Se_GetSemaphoreStatus";"LAP_Se_GetSemaphoreId";"LAP_Se_SignalSemaphore";"LAP_Se_WaitSemaphore";"LAP_Se_CreateSemaphore";"LAP_Se_GetBlackboardStatus";"LAP_Se_GetBlackboardId";"LAP_Se_ClearBlackboard";"LAP_Se_ReadBlackboard";"LAP_Se_DisplayBlackboard";"LAP_Se_CreateBlackboard";"LAP_Se_GetBufferStatus";"LAP_Se_GetBufferId";"LAP_Se_ReceiveBuffer";"LAP_Se_SendBuffer";"LAP_Se_CreateBuffer";"LAP_Se_GetQueuingPortStatus";"LAP_Se_GetQueuingPortId";"LAP_Se_ReceiveQueuingMessage";"LAP_Se_SendQueuingMessage";"LAP_Se_CreateQueuingPort";"LAP_Se_GetSamplingPortStatus";"LAP_Se_GetSamplingPortId";"LAP_Se_ReadSamplingMessage";"LAP_Se_WriteSamplingMessage";"LAP_Se_CreateSamplingPort";"LAP_Se_GetLogBookStatus";"LAP_Se_GetLogbookId";"LAP_Se_ClearLogBook";"LAP_Se_WriteLogBook";"LAP_Se_ReadLogBook";"LAP_Se_CreateLogBook";"LAP_Se_GetPartitionStartCondition";"LAP_Se_SetPartitionMode";"LAP_Se_GetPartitionStatus";"LAP_Se_GetProcessStatus";"LAP_Se_GetProcessId";"LAP_Se_GetMyId";"LAP_Se_UnlockPreemption";"LAP_Se_LockPreemption";"LAP_Se_DelayedStart";"LAP_Se_Start";"LAP_Se_Stop";"LAP_Se_StopSelf";"LAP_Se_Resume";"LAP_Se_Suspend";"LAP_Se_SetPriority";"LAP_Se_CreateProcess";"LAP_Se_ReplenishAperiodic";"LAP_Se_GetTime";"LAP_Se_PeriodicWait";"LAP_Se_TimedWait"]