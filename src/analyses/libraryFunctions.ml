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
  ("sigfillset", Unsafe); 
  ("sigprocmask", Unsafe); 
  ("uname",Unsafe);
  ("__builtin_strcmp",Safe);
  ("getopt_long",Drop 2);
  ("__strdup",Safe);
  ("strtoul__extinline",Safe);
  ("geteuid",Safe);
  ("opendir",Safe);  
  ("readdir_r",Unsafe);
  ("atoi__extinline",Safe);
  ("getpid",Safe);
  ("fgetc",Unsafe);
  ("closedir",Unsafe);
  ("setrlimit",Safe);
  ("chdir",Safe);
  ("pipe",Unsafe);
  ("close",Unsafe);
  ("setsid",Safe);
  ("strerror_r",Unsafe);
  ("pthread_attr_init",Unsafe); 
  ("pthread_attr_setdetachstate",Unsafe);
  ("pthread_attr_setstacksize",Unsafe);
  ("pthread_key_create",Unsafe);
  ("sigemptyset",Unsafe);
  ("sigaddset",Unsafe);
  ("pthread_sigmask",Unsafe);
  ("raise",Unsafe);
  ("_strlen",Safe);
  ("__builtin_alloca",Safe);
  ("dlopen",Safe);
  ("dlsym",Safe);
  ("dlclose",Safe);
  ("dlerror",Safe);
  ("stat__extinline",Drop 1);
  ("lstat__extinline",Drop 1);
  ("__builtin_strchr",Safe);
  ("strcpy",Safe);
  ("strcat",Keep [2]);
  ("getpgrp",Safe);
  ("umount2",Safe);
  ("memchr",Safe);
  ("memmove",Keep [2;3]);
  ("waitpid",Safe);
  ("statfs",Keep [1;3;4]);
  ("mkdir",Safe);
  ("mount",Safe);
  ("open",Safe);
  ("fcntl",Safe);
  ("ioctl",Unsafe);
  ("fstat__extinline",Unsafe);
  ("umount",Safe);
  ("rmdir",Safe);
  ("strrchr",Safe);
  ("scandir",Keep [1;3;4]);
  ("unlink",Safe);
  ("sched_yield",Safe);
  ("nanosleep",Drop 1);
  ("sigdelset",Safe);
  ("sigwait",Drop 1);
  ("setlocale",Safe);
  ("bindtextdomain",Safe);
  ("textdomain",Safe);
  ("dcgettext",Safe);
  ("fputs",Safe);
  ("syscall",Drop 1);
  ("fputc",Safe);
  ("feof",Safe);
  ("__getdelim",Keep [3]);
  ("vsyslog",Safe);
  ("gethostbyname_r",Safe);
  ("__h_errno_location",Safe);
  ("getuid",Safe);
  ("strerror",Safe);
  ("readdir",Safe);
  ("openlog",Safe);
  ("getdtablesize",Safe);
  ("umask",Safe);
  ("socket",Safe);
  ("clntudp_create", Drop 3);
  ("svctcp_create", Safe);
  ("clntudp_bufcreate", Unsafe);
  ("authunix_create_default",Safe);
  ("writev",Safe);
  ("clnt_broadcast",Unsafe);
  ("clnt_sperrno",Safe);
  ("pmap_unset",Unsafe);
  ("bind",Safe);
  ("svcudp_create",Safe);
  ("svc_register",Unsafe);
  ("sleep",Safe);
  ("svc_run",Unsafe);
  ("dup",Safe); 
  ("__builtin_expect",Safe); (* only stoopid people use __builtin_expect *) 
  ("vsnprintf",Drop 3); 
  ("syslog",Safe); 
  ("strcasecmp",Safe); 
  ("strchr",Safe); 
  ("getservbyname",Safe); 
  ("__error",Safe); 
  ("__maskrune",Unsafe); 
  ("inet_addr",Safe); 
  ("gethostbyname",Safe); 
  ("__builtin_bzero",Keep [1]); 
  ("setsockopt",Safe); 
  ("listen",Safe); 
  ("getsockname",Keep [1;3]); 
  ("getenv",Safe); 
  ("execl",Safe); 
  ("select",Keep [1;5]); 
  ("accept",Keep [1]); 
  ("getpeername",Keep [1]); 
  ("times",Unsafe); 
  ("fgets",Keep [3]); 
  ("strtoul",Safe); 
  ("__tolower",Safe); 
  ("signal",Unsafe); 
  ("popen",Safe); 
  ("BF_cfb64_encrypt",Keep [1;3;4;5]); 
  ("BZ2_bzBuffToBuffDecompress",Keep [3;4]); 
  ("uncompress",Keep [3;4]); 
  ("stat",Keep [1]); 
  ("BZ2_bzBuffToBuffCompress",Keep [3;4]); 
  ("compress2",Keep [3]); 
  ("__toupper",Safe); 
  ("BF_set_key",Keep [3]); 
  ("memcmp",Safe); 
  ("sendto",Keep [2;4]); 
  ("recvfrom",Keep [4;5]); 
  ("srand",Safe); 
  ("rand",Safe); 
  ("gethostname",Unsafe); 
  ("pthread_mutex_init",Safe); 
  ("fork",Safe); 
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
