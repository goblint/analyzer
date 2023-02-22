(** Tools for dealing with library functions. *)

open Prelude.Ana
open GobConfig

module M = Messages

(** C standard library functions.
    These are specified by the C standard. *)
let c_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("memset", special [__ "dest" [w]; __ "ch" []; __ "count" []] @@ fun dest ch count -> Memset { dest; ch; count; });
    ("__builtin_memset", special [__ "dest" [w]; __ "ch" []; __ "count" []] @@ fun dest ch count -> Memset { dest; ch; count; });
    ("__builtin___memset_chk", special [__ "dest" [w]; __ "ch" []; __ "count" []; drop "os" []] @@ fun dest ch count -> Memset { dest; ch; count; });
    ("memcpy", special [__ "dest" [w]; __ "src" [r]; drop "n" []] @@ fun dest src -> Memcpy { dest; src });
    ("__builtin_memcpy", special [__ "dest" [w]; __ "src" [r]; drop "n" []] @@ fun dest src -> Memcpy { dest; src });
    ("__builtin___memcpy_chk", special [__ "dest" [w]; __ "src" [r]; drop "n" []; drop "os" []] @@ fun dest src -> Memcpy { dest; src });
    ("strncpy", special [__ "dest" [w]; __ "src" [r]; drop "n" []] @@ fun dest src -> Strcpy { dest; src });
    ("strcpy", special [__ "dest" [w]; __ "src" [r]] @@ fun dest src -> Strcpy { dest; src });
    ("malloc", special [__ "size" []] @@ fun size -> Malloc size);
    ("realloc", special [__ "ptr" [r; f]; __ "size" []] @@ fun ptr size -> Realloc { ptr; size });
    ("abort", special [] Abort);
    ("exit", special [drop "exit_code" []] Abort);
    ("ungetc", unknown [drop "c" []; drop "stream" [r; w]]);
    ("fscanf", unknown ((drop "stream" [r; w]) :: (drop "format" [r]) :: (VarArgs (drop' [w]))));
    ("__freading", unknown [drop "stream" [r]]);
    ("mbsinit", unknown [drop "ps" [r]]);
    ("mbrtowc", unknown [drop "pwc" [w]; drop "s" [r]; drop "n" []; drop "ps" [r; w]]);
    ("iswspace", unknown [drop "wc" []]);
    ("iswalnum", unknown [drop "wc" []]);
    ("iswprint", unknown [drop "wc" []]);
    ("rename" , unknown [drop "oldpath" [r]; drop "newpath" [r];]);
    ("puts", unknown [drop "s" [r]]);
    ("strspn", unknown [drop "s" [r]; drop "accept" [r]]);
    ("strcspn", unknown [drop "s" [r]; drop "accept" [r]]);
    ("strtod", unknown [drop "nptr" [r]; drop "endptr" [w]]);
    ("strtol", unknown [drop "nptr" [r]; drop "endptr" [w]; drop "base" []]);
    ("__strtol_internal", unknown [drop "nptr" [r]; drop "endptr" [w]; drop "base" []; drop "group" []]);
    ("strtoll", unknown [drop "nptr" [r]; drop "endptr" [w]; drop "base" []]);
    ("strtoul", unknown [drop "nptr" [r]; drop "endptr" [w]; drop "base" []]);
    ("strtoull", unknown [drop "nptr" [r]; drop "endptr" [w]; drop "base" []]);
    ("mktime", unknown [drop "tm" [r;w]]);
    ("ctime", unknown [drop "rm" [r]]);
    ("clearerr", unknown [drop "stream" [w]]);
    ("setbuf", unknown [drop "stream" [w]; drop "buf" [w]]);
    ("swprintf", unknown (drop "wcs" [w] :: drop "maxlen" [] :: drop "fmt" [r] :: VarArgs (drop' [])));
    ("assert", special [__ "exp" []] @@ fun exp -> Assert { exp; check = true; refine = get_bool "sem.assert.refine" }); (* only used if assert is used without include, e.g. in transformed files *)
    ("difftime", unknown [drop "time1" []; drop "time2" []]);
    ("system", unknown [drop "command" [r]]);
    ("wcscat", unknown [drop "dest" [r; w]; drop "src" [r]]);
    ("abs", unknown [drop "j" []]);
    ("localtime_r", unknown [drop "timep" [r]; drop "result" [w]]);
    ("strsep", unknown [drop "stringp" [r_deep; w]; drop "delim" [r]]);
    ("strcasestr", unknown [drop "haystack" [r]; drop "needle" [r]]);
    ("strpbrk", unknown [drop "s" [r]; drop "accept" [r]]);
  ]

(** C POSIX library functions.
    These are {e not} specified by the C standard, but available on POSIX systems. *)
let posix_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("bzero", special [__ "dest" [w]; __ "count" []] @@ fun dest count -> Bzero { dest; count; });
    ("__builtin_bzero", special [__ "dest" [w]; __ "count" []] @@ fun dest count -> Bzero { dest; count; });
    ("explicit_bzero", special [__ "dest" [w]; __ "count" []] @@ fun dest count -> Bzero { dest; count; });
    ("__explicit_bzero_chk", special [__ "dest" [w]; __ "count" []; drop "os" []] @@ fun dest count -> Bzero { dest; count; });
    ("nl_langinfo", unknown [drop "item" []]);
    ("nl_langinfo_l", unknown [drop "item" []; drop "locale" [r_deep]]);
    ("getc_unlocked", unknown [drop "stream" [w]]);
    ("getchar_unlocked", unknown []);
    ("putc_unlocked", unknown [drop "c" []; drop "stream" [w]]);
    ("putchar_unlocked", unknown [drop "c" []]);
    ("lseek", unknown [drop "fd" []; drop "offset" []; drop "whence" []]);
    ("fseeko", unknown [drop "stream" [w]; drop "offset" []; drop "whence" []]);
    ("iconv_open", unknown [drop "tocode" [r]; drop "fromcode" [r]]);
    ("iconv", unknown [drop "cd" [r]; drop "inbuf" [r]; drop "inbytesleft" [r;w]; drop "outbuf" [w]; drop "outbytesleft" [r;w]]);
    ("iconv_close", unknown [drop "cd" [f]]);
    ("strnlen", unknown [drop "s" [r]; drop "maxlen" []]);
    ("chmod", unknown [drop "pathname" [r]; drop "mode" []]);
    ("fchmod", unknown [drop "fd" []; drop "mode" []]);
    ("fchown", unknown [drop "fd" []; drop "owner" []; drop "group" []]);
    ("lchown", unknown [drop "pathname" [r]; drop "owner" []; drop "group" []]);
    ("clock_gettime", unknown [drop "clockid" []; drop "tp" [w]]);
    ("gettimeofday", unknown [drop "tv" [w]; drop "tz" [w]]);
    ("futimens", unknown [drop "fd" []; drop "times" [r]]);
    ("utimes", unknown [drop "filename" [r]; drop "times" [r]]);
    ("linkat", unknown [drop "olddirfd" []; drop "oldpath" [r]; drop "newdirfd" []; drop "newpath" [r]; drop "flags" []]);
    ("dirfd", unknown [drop "dirp" [r]]);
    ("fdopendir", unknown [drop "fd" []]);
    ("pathconf", unknown [drop "path" [r]; drop "name" []]);
    ("symlink" , unknown [drop "oldpath" [r]; drop "newpath" [r];]);
    ("ftruncate", unknown [drop "fd" []; drop "length" []]);
    ("mkfifo", unknown [drop "pathname" [r]; drop "mode" []]);
    ("ntohs", unknown [drop "netshort" []]);
    ("alarm", unknown [drop "seconds" []]);
    ("pwrite", unknown [drop "fd" []; drop "buf" [r]; drop "count" []; drop "offset" []]);
    ("hstrerror", unknown [drop "err" []]);
    ("inet_ntoa", unknown [drop "in" []]);
    ("getsockopt", unknown [drop "sockfd" []; drop "level" []; drop "optname" []; drop "optval" [w]; drop "optlen" [w]]);
    ("gethostbyaddr", unknown [drop "addr" [r_deep]; drop "len" []; drop "type" []]);
    ("gethostbyaddr_r", unknown [drop "addr" [r_deep]; drop "len" []; drop "type" []; drop "ret" [w_deep]; drop "buf" [w]; drop "buflen" []; drop "result" [w]; drop "h_errnop" [w]]);
    ("sigaction", unknown [drop "signum" []; drop "act" [r_deep; s_deep]; drop "oldact" [w_deep]]);
    ("tcgetattr", unknown [drop "fd" []; drop "termios_p" [r_deep]]);
    ("tcsetattr", unknown [drop "fd" []; drop "optional_actions" []; drop "termios_p" [w_deep]]);
    ("access", unknown [drop "pathname" [r]; drop "mode" []]);
    ("ttyname", unknown [drop "fd" []]);
    ("shm_open", unknown [drop "name" [r]; drop "oflag" []; drop "mode" []]);
    ("sched_get_priority_max", unknown [drop "policy" []]);
    ("mprotect", unknown [drop "addr" []; drop "len" []; drop "prot" []]);
    ("ftime", unknown [drop "tp" [w]]);
    ("timer_create", unknown [drop "clockid" []; drop "sevp" [r; w; s]; drop "timerid" [w]]);
    ("timer_settime", unknown [drop "timerid" []; drop "flags" []; drop "new_value" [r_deep]; drop "old_value" [w_deep]]);
    ("timer_gettime", unknown [drop "timerid" []; drop "curr_value" [w_deep]]);
    ("timer_getoverrun", unknown [drop "timerid" []]);
    ("lstat", unknown [drop "pathname" [r]; drop "statbuf" [w]]);
    ("getpwnam", unknown [drop "name" [r]]);
    ("strndup", unknown [drop "s" [r]; drop "n" []]);
    ("freeaddrinfo", unknown [drop "res" [f_deep]]);
    ("getgid", unknown []);
    ("pselect", unknown [drop "nfds" []; drop "readdfs" [r]; drop "writedfs" [r]; drop "exceptfds" [r]; drop "timeout" [r]; drop "sigmask" [r]]);
    ("strncasecmp", unknown [drop "s1" [r]; drop "s2" [r]; drop "n" []]);
    ("getnameinfo", unknown [drop "addr" [r_deep]; drop "addrlen" []; drop "host" [w]; drop "hostlen" []; drop "serv" [w]; drop "servlen" []; drop "flags" []]);
    ("strtok_r", unknown [drop "str" [r; w]; drop "delim" [r]; drop "saveptr" [w]]);
    ("kill", unknown [drop "pid" []; drop "sig" []]);
    ("closelog", unknown []);
    ("dirname", unknown [drop "path" [r]]);
    ("setpgid", unknown [drop "pid" []; drop "pgid" []]);
    ("dup2", unknown [drop "oldfd" []; drop "newfd" []]);
    ("pclose", unknown [drop "stream" [w; f]]);
    ("getcwd", unknown [drop "buf" [w]; drop "size" []]);
    ("inet_pton", unknown [drop "af" []; drop "src" [r]; drop "dst" [w]]);
    ("inet_ntop", unknown [drop "af" []; drop "src" [r]; drop "dst" [w]; drop "size" []]);
    ("inet_aton", unknown [drop "cp" [r]; drop "inp" [w]]);
    ("gethostent", unknown []);
    ("poll", unknown [drop "fds" [r]; drop "nfds" []; drop "timeout" []]);
    ("semget", unknown [drop "key" []; drop "nsems" []; drop "semflg" []]);
    ("semctl", unknown (drop "semid" [] :: drop "semnum" [] :: drop "cmd" [] :: VarArgs (drop "semun" [r_deep])));
    ("semop", unknown [drop "semid" []; drop "sops" [r]; drop "nsops" []]);
  ]

(** Pthread functions. *)
let pthread_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("pthread_create", special [__ "thread" [w]; drop "attr" [r]; __ "start_routine" [s]; __ "arg" []] @@ fun thread start_routine arg -> ThreadCreate { thread; start_routine; arg }); (* For precision purposes arg is not considered accessed here. Instead all accesses (if any) come from actually analyzing start_routine. *)
    ("pthread_exit", special [__ "retval" []] @@ fun retval -> ThreadExit { ret_val = retval }); (* Doesn't dereference the void* itself, but just passes to pthread_join. *)
    ("pthread_cond_signal", special [__ "cond" []] @@ fun cond -> Signal cond);
    ("pthread_cond_broadcast", special [__ "cond" []] @@ fun cond -> Broadcast cond);
    ("pthread_cond_wait", special [__ "cond" []; __ "mutex" []] @@ fun cond mutex -> Wait {cond; mutex});
    ("pthread_cond_timedwait", special [__ "cond" []; __ "mutex" []; __ "abstime" [r]] @@ fun cond mutex abstime -> TimedWait {cond; mutex; abstime});
    ("pthread_attr_destroy", unknown [drop "attr" [f]]);
    ("pthread_setspecific", unknown ~attrs:[InvalidateGlobals] [drop "key" []; drop "value" [w_deep]]);
    ("pthread_getspecific", unknown ~attrs:[InvalidateGlobals] [drop "key" []]);
    ("pthread_key_delete", unknown [drop "key" [f]]);
    ("pthread_cancel", unknown [drop "thread" []]);
    ("pthread_setcanceltype", unknown [drop "type" []; drop "oldtype" [w]]);
    ("pthread_detach", unknown [drop "thread" []]);
    ("pthread_attr_setschedpolicy", unknown [drop "attr" [r; w]; drop "policy" []]);
    ("pthread_condattr_init", unknown [drop "attr" [w]]);
    ("pthread_condattr_setclock", unknown [drop "attr" [w]; drop "clock_id" []]);
    ("pthread_mutexattr_destroy", unknown [drop "attr" [f]]);
    ("pthread_attr_setschedparam", unknown [drop "attr" [r; w]; drop "param" [r]]);
    ("sem_timedwait", unknown [drop "sem" [r]; drop "abs_timeout" [r]]); (* no write accesses to sem because sync primitive itself has no race *)
  ]

(** GCC builtin functions.
    These are not builtin versions of functions from other lists. *)
let gcc_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("__builtin_object_size", unknown [drop "ptr" [r]; drop' []]);
    ("__builtin_prefetch", unknown (drop "addr" [] :: VarArgs (drop' [])));
    ("__builtin_expect", special [__ "exp" []; drop' []] @@ fun exp -> Identity exp); (* Identity, because just compiler optimization annotation. *)
    ("__builtin_unreachable", special' [] @@ fun () -> if get_bool "sem.builtin_unreachable.dead_code" then Abort else Unknown); (* https://github.com/sosy-lab/sv-benchmarks/issues/1296 *)
    ("__assert_rtn", special [drop "func" [r]; drop "file" [r]; drop "line" []; drop "exp" [r]] @@ Abort); (* gcc's built-in assert *)
    ("__builtin_return_address", unknown [drop "level" []]);
    ("__builtin___sprintf_chk", unknown (drop "s" [w] :: drop "flag" [] :: drop "os" [] :: drop "fmt" [r] :: VarArgs (drop' [])));
    ("__builtin_add_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_sadd_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_saddl_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_saddll_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_uadd_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_uaddl_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_uaddll_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_sub_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_ssub_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_ssubl_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_ssubll_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_usub_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_usubl_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_usubll_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_mul_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_smul_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_smull_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_smulll_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_umul_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_umull_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_umulll_overflow", unknown [drop "a" []; drop "b" []; drop "c" [w]]);
    ("__builtin_add_overflow_p", unknown [drop "a" []; drop "b" []; drop "c" []]);
    ("__builtin_sub_overflow_p", unknown [drop "a" []; drop "b" []; drop "c" []]);
    ("__builtin_mul_overflow_p", unknown [drop "a" []; drop "b" []; drop "c" []]);
    ("__builtin_popcount", unknown [drop "x" []]);
    ("__builtin_popcountl", unknown [drop "x" []]);
    ("__builtin_popcountll", unknown [drop "x" []]);
    ("__atomic_store_n", unknown [drop "ptr" [w]; drop "val" []; drop "memorder" []]);
    ("__atomic_load_n", unknown [drop "ptr" [r]; drop "memorder" []]);
    ("__sync_fetch_and_add", unknown (drop "ptr" [r; w] :: drop "value" [] :: VarArgs (drop' [])));
    ("__sync_fetch_and_sub", unknown (drop "ptr" [r; w] :: drop "value" [] :: VarArgs (drop' [])));
    ("__builtin_va_copy", unknown [drop "dest" [w]; drop "src" [r]]);
  ]

let glibc_desc_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("fputs_unlocked", unknown [drop "s" [r]; drop "stream" [w]]);
    ("futimesat", unknown [drop "dirfd" [w]; drop "pathname" [r]; drop "times" [r]]);
    ("error", unknown ((drop "status" []):: (drop "errnum" []) :: (drop "format" [r]) :: (VarArgs (drop' [r]))));
    ("gettext", unknown [drop "msgid" [r]]);
    ("euidaccess", unknown [drop "pathname" [r]; drop "mode" []]);
    ("rpmatch", unknown [drop "response" [r]]);
    ("getpagesize", unknown []);
    ("__read_chk", unknown [drop "__fd" []; drop "__buf" [w]; drop "__nbytes" []; drop "__buflen" []]);
    ("__read_alias", unknown [drop "__fd" []; drop "__buf" [w]; drop "__nbytes" []]);
    ("__readlink_chk", unknown [drop "path" [r]; drop "buf" [w]; drop "len" []; drop "buflen" []]);
    ("__readlink_alias", unknown [drop "path" [r]; drop "buf" [w]; drop "len" []]);
    ("__overflow", unknown [drop "f" [r]; drop "ch" []]);
    ("__ctype_get_mb_cur_max", unknown []);
    ("__xmknod", unknown [drop "ver" []; drop "path" [r]; drop "mode" []; drop "dev" [r; w]]);
    ("yp_get_default_domain", unknown [drop "outdomain" [w]]);
    ("__nss_configure_lookup", unknown [drop "db" [r]; drop "service_line" [r]]);
    ("xdr_string", unknown [drop "xdrs" [r_deep; w_deep]; drop "sp" [r; w]; drop "maxsize" []]);
    ("xdr_enum", unknown [drop "xdrs" [r_deep; w_deep]; drop "ep" [r; w]]);
    ("xdr_u_int", unknown [drop "xdrs" [r_deep; w_deep]; drop "up" [r; w]]);
    ("xdr_opaque", unknown [drop "xdrs" [r_deep; w_deep]; drop "cp" [r; w]; drop "cnt" []]);
    ("xdr_free", unknown [drop "proc" [s]; drop "objp" [f_deep]]);
    ("svcerr_noproc", unknown [drop "xprt" [r_deep; w_deep]]);
    ("svcerr_decode", unknown [drop "xprt" [r_deep; w_deep]]);
    ("svcerr_systemerr", unknown [drop "xprt" [r_deep; w_deep]]);
    ("svc_sendreply", unknown [drop "xprt" [r_deep; w_deep]; drop "outproc" [s]; drop "out" [r]]);
    ("shutdown", unknown [drop "socket" []; drop "how" []]);
    ("getaddrinfo_a", unknown [drop "mode" []; drop "list" [w_deep]; drop "nitems" []; drop "sevp" [r; w; s]]);
    ("__uflow", unknown [drop "file" [r; w]]);
    ("getservbyname_r", unknown [drop "name" [r]; drop "proto" [r]; drop "result_buf" [w_deep]; drop "buf" [w]; drop "buflen" []; drop "result" [w]]);
  ]

let linux_userspace_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    (* ("prctl", unknown [drop "option" []; drop "arg2" []; drop "arg3" []; drop "arg4" []; drop "arg5" []]); *)
    ("prctl", unknown (drop "option" [] :: VarArgs (drop' []))); (* man page has 5 arguments, but header has varargs and real-world programs may call with <5 *)
    ("__ctype_tolower_loc", unknown []);
    ("__ctype_toupper_loc", unknown []);
    ("epoll_create", unknown [drop "size" []]);
    ("epoll_ctl", unknown [drop "epfd" []; drop "op" []; drop "fd" []; drop "event" [w]]);
    ("epoll_wait", unknown [drop "epfd" []; drop "events" [w]; drop "maxevents" []; drop "timeout" []]);
    ("sysinfo", unknown [drop "info" [w_deep]]);
    ("__xpg_basename", unknown [drop "path" [r]]);
  ]

let big_kernel_lock = AddrOf (Cil.var (Goblintutil.create_var (makeGlobalVar "[big kernel lock]" intType)))
let console_sem = AddrOf (Cil.var (Goblintutil.create_var (makeGlobalVar "[console semaphore]" intType)))

(** Linux kernel functions. *)
(* TODO: conditional on kernel option *)
let linux_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("spin_lock_irqsave", special [__ "lock" []; drop "flags" []] @@ fun lock -> Lock { lock; try_ = get_bool "sem.lock.fail"; write = true; return_on_success = true });
    ("spin_unlock_irqrestore", special [__ "lock" []; drop "flags" []] @@ fun lock -> Unlock lock);
    ("_raw_spin_unlock_irqrestore", special [__ "lock" []; drop "flags" []] @@ fun lock -> Unlock lock);
    ("spinlock_check", special [__ "lock" []] @@ fun lock -> Identity lock);  (* Identity, because we don't want lock internals. *)
    ("_lock_kernel", special [drop "func" [r]; drop "file" [r]; drop "line" []] @@ Lock { lock = big_kernel_lock; try_ = false; write = true; return_on_success = true });
    ("_unlock_kernel", special [drop "func" [r]; drop "file" [r]; drop "line" []] @@ Unlock big_kernel_lock);
    ("acquire_console_sem", special [] @@ Lock { lock = console_sem; try_ = false; write = true; return_on_success = true });
    ("release_console_sem", special [] @@ Unlock console_sem);
    ("misc_deregister", unknown [drop "misc" [r_deep]]);
    ("__bad_percpu_size", special [] Abort); (* these do not have definitions so the linker will fail if they are actually called *)
    ("__bad_size_call_parameter", special [] Abort);
    ("__xchg_wrong_size", special [] Abort);
    ("__cmpxchg_wrong_size", special [] Abort);
    ("__xadd_wrong_size", special [] Abort);
    ("__put_user_bad", special [] Abort);
  ]

(** Goblint functions. *)
let goblint_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("__goblint_unknown", unknown [drop' [w]]);
    ("__goblint_check", special [__ "exp" []] @@ fun exp -> Assert { exp; check = true; refine = false });
    ("__goblint_assume", special [__ "exp" []] @@ fun exp -> Assert { exp; check = false; refine = true });
    ("__goblint_assert", special [__ "exp" []] @@ fun exp -> Assert { exp; check = true; refine = get_bool "sem.assert.refine" });
    ("__goblint_split_begin", unknown [drop "exp" []]);
    ("__goblint_split_end", unknown [drop "exp" []]);
  ]

(** zstd functions.
    Only used with extraspecials. *)
let zstd_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("ZSTD_customMalloc", special [__ "size" []; drop "customMem" [r]] @@ fun size -> Malloc size);
    ("ZSTD_customCalloc", special [__ "size" []; drop "customMem" [r]] @@ fun size -> Calloc { size; count = Cil.one });
    ("ZSTD_customFree", unknown [drop "ptr" [f]; drop "customMem" [r]]);
  ]

(** math functions.
    Functions and builtin versions of function and macros defined in math.h. *)
let math_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("__builtin_nan", special [__ "str" []] @@ fun str -> Math { fun_args = (Nan (FDouble, str)) });
    ("nan", special [__ "str" []] @@ fun str -> Math { fun_args = (Nan (FDouble, str)) });
    ("__builtin_nanf", special [__ "str" []] @@ fun str -> Math { fun_args = (Nan (FFloat, str)) });
    ("nanf", special [__ "str" []] @@ fun str -> Math { fun_args = (Nan (FFloat, str)) });
    ("__builtin_nanl", special [__ "str" []] @@ fun str -> Math { fun_args = (Nan (FLongDouble, str)) });
    ("nanl", special [__ "str" []] @@ fun str -> Math { fun_args = (Nan (FLongDouble, str)) });
    ("__builtin_inf", special [] @@ Math { fun_args = Inf FDouble});
    ("__builtin_huge_val", special [] @@ Math { fun_args = Inf FDouble}); (* we assume the target format can represent infinities *)
    ("__builtin_inff", special [] @@ Math { fun_args = Inf FFloat});
    ("__builtin_huge_valf", special [] @@ Math { fun_args = Inf FFloat}); (* we assume the target format can represent infinities *)
    ("__builtin_infl", special [] @@ Math { fun_args = Inf FLongDouble});
    ("__builtin_huge_vall", special [] @@ Math { fun_args = Inf FLongDouble});  (* we assume the target format can represent infinities *)
    ("__builtin_isfinite", special [__ "x" []] @@ fun x -> Math { fun_args = (Isfinite x) });
    ("__finite", special [__ "x" []] @@ fun x -> Math { fun_args = (Isfinite x) });
    ("__finitef", special [__ "x" []] @@ fun x -> Math { fun_args = (Isfinite x) });
    ("__finitel", special [__ "x" []] @@ fun x -> Math { fun_args = (Isfinite x) });
    ("__builtin_isinf", special [__ "x" []] @@ fun x -> Math { fun_args = (Isinf x) });
    ("__isinf", special [__ "x" []] @@ fun x -> Math { fun_args = (Isinf x) });
    ("__isinff", special [__ "x" []] @@ fun x -> Math { fun_args = (Isinf x) });
    ("__isinfl", special [__ "x" []] @@ fun x -> Math { fun_args = (Isinf x) });
    ("__builtin_isinf_sign", special [__ "x" []] @@ fun x -> Math { fun_args = (Isinf x) });
    ("__builtin_isnan", special [__ "x" []] @@ fun x -> Math { fun_args = (Isnan x) });
    ("__isnan", special [__ "x" []] @@ fun x -> Math { fun_args = (Isnan x) });
    ("__isnanf", special [__ "x" []] @@ fun x -> Math { fun_args = (Isnan x) });
    ("__isnanl", special [__ "x" []] @@ fun x -> Math { fun_args = (Isnan x) });
    ("__builtin_isnormal", special [__ "x" []] @@ fun x -> Math { fun_args = (Isnormal x) });
    ("__builtin_signbit", special [__ "x" []] @@ fun x -> Math { fun_args = (Signbit x) });
    ("__signbit", special [__ "x" []] @@ fun x -> Math { fun_args = (Signbit x) });
    ("__signbitf", special [__ "x" []] @@ fun x -> Math { fun_args = (Signbit x) });
    ("__signbitl", special [__ "x" []] @@ fun x -> Math { fun_args = (Signbit x) });
    ("__builtin_fabs", special [__ "x" []] @@ fun x -> Math { fun_args = (Fabs (FDouble, x)) });
    ("__builtin_fabsf", special [__ "x" []] @@ fun x -> Math { fun_args = (Fabs (FFloat, x)) });
    ("__builtin_fabsl", special [__ "x" []] @@ fun x -> Math { fun_args = (Fabs (FLongDouble, x)) });
    ("__builtin_isgreater", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Isgreater (x,y)) });
    ("__builtin_isgreaterequal", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Isgreaterequal (x,y)) });
    ("__builtin_isless", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Isless (x,y)) });
    ("__builtin_islessequal", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Islessequal (x,y)) });
    ("__builtin_islessgreater", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Islessgreater (x,y)) });
    ("__builtin_isunordered", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Isunordered (x,y)) });
    ("ceil", special [__ "x" []] @@ fun x -> Math { fun_args = (Ceil (FDouble, x)) });
    ("ceilf", special [__ "x" []] @@ fun x -> Math { fun_args = (Ceil (FFloat, x)) });
    ("ceill", special [__ "x" []] @@ fun x -> Math { fun_args = (Ceil (FLongDouble, x)) });
    ("floor", special [__ "x" []] @@ fun x -> Math { fun_args = (Floor (FDouble, x)) });
    ("floorf", special [__ "x" []] @@ fun x -> Math { fun_args = (Floor (FFloat, x)) });
    ("floorl", special [__ "x" []] @@ fun x -> Math { fun_args = (Floor (FLongDouble, x)) });
    ("fabs", special [__ "x" []] @@ fun x -> Math { fun_args = (Fabs (FDouble, x)) });
    ("fabsf", special [__ "x" []] @@ fun x -> Math { fun_args = (Fabs (FFloat, x)) });
    ("fabsl", special [__ "x" []] @@ fun x -> Math { fun_args = (Fabs (FLongDouble, x)) });
    ("fmax", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Fmax (FDouble, x, y)) });
    ("fmaxf", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Fmax (FFloat, x, y)) });
    ("fmaxl", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Fmax (FLongDouble, x, y)) });
    ("fmin", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Fmin (FDouble, x, y)) });
    ("fminf", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Fmin (FFloat, x, y)) });
    ("fminl", special [__ "x" []; __ "y" []] @@ fun x y -> Math { fun_args = (Fmin (FLongDouble, x, y)) });
    ("__builtin_acos", special [__ "x" []] @@ fun x -> Math { fun_args = (Acos (FDouble, x)) });
    ("acos", special [__ "x" []] @@ fun x -> Math { fun_args = (Acos (FDouble, x)) });
    ("acosf", special [__ "x" []] @@ fun x -> Math { fun_args = (Acos (FFloat, x)) });
    ("acosl", special [__ "x" []] @@ fun x -> Math { fun_args = (Acos (FLongDouble, x)) });
    ("__builtin_asin", special [__ "x" []] @@ fun x -> Math { fun_args = (Asin (FDouble, x)) });
    ("asin", special [__ "x" []] @@ fun x -> Math { fun_args = (Asin (FDouble, x)) });
    ("asinf", special [__ "x" []] @@ fun x -> Math { fun_args = (Asin (FFloat, x)) });
    ("asinl", special [__ "x" []] @@ fun x -> Math { fun_args = (Asin (FLongDouble, x)) });
    ("__builtin_atan", special [__ "x" []] @@ fun x -> Math { fun_args = (Atan (FDouble, x)) });
    ("atan", special [__ "x" []] @@ fun x -> Math { fun_args = (Atan (FDouble, x)) });
    ("atanf", special [__ "x" []] @@ fun x -> Math { fun_args = (Atan (FFloat, x)) });
    ("atanl", special [__ "x" []] @@ fun x -> Math { fun_args = (Atan (FLongDouble, x)) });
    ("__builtin_atan2", special [__ "y" []; __ "x" []] @@ fun y x -> Math { fun_args = (Atan2 (FDouble, y, x)) });
    ("atan2", special [__ "y" []; __ "x" []] @@ fun y x -> Math { fun_args = (Atan2 (FDouble, y, x)) });
    ("atan2f", special [__ "y" []; __ "x" []] @@ fun y x -> Math { fun_args = (Atan2 (FFloat, y, x)) });
    ("atan2l", special [__ "y" []; __ "x" []] @@ fun y x -> Math { fun_args = (Atan2 (FLongDouble, y, x)) });
    ("__builtin_cos", special [__ "x" []] @@ fun x -> Math { fun_args = (Cos (FDouble, x)) });
    ("cos", special [__ "x" []] @@ fun x -> Math { fun_args = (Cos (FDouble, x)) });
    ("cosf", special [__ "x" []] @@ fun x -> Math { fun_args = (Cos (FFloat, x)) });
    ("cosl", special [__ "x" []] @@ fun x -> Math { fun_args = (Cos (FLongDouble, x)) });
    ("__builtin_sin", special [__ "x" []] @@ fun x -> Math { fun_args = (Sin (FDouble, x)) });
    ("sin", special [__ "x" []] @@ fun x -> Math { fun_args = (Sin (FDouble, x)) });
    ("sinf", special [__ "x" []] @@ fun x -> Math { fun_args = (Sin (FFloat, x)) });
    ("sinl", special [__ "x" []] @@ fun x -> Math { fun_args = (Sin (FLongDouble, x)) });
    ("__builtin_tan", special [__ "x" []] @@ fun x -> Math { fun_args = (Tan (FDouble, x)) });
    ("tan", special [__ "x" []] @@ fun x -> Math { fun_args = (Tan (FDouble, x)) });
    ("tanf", special [__ "x" []] @@ fun x -> Math { fun_args = (Tan (FFloat, x)) });
    ("tanl", special [__ "x" []] @@ fun x -> Math { fun_args = (Tan (FLongDouble, x)) });
    ("fegetround", unknown []);
    ("fesetround", unknown [drop "round" []]); (* Our float domain is rounding agnostic *)
    ("__builtin_fpclassify", unknown [drop "nan" []; drop "infinite" []; drop "normal" []; drop "subnormal" []; drop "zero" []; drop "x" []]); (* TODO: We could do better here *)
    ("__builtin_fpclassifyf", unknown [drop "nan" []; drop "infinite" []; drop "normal" []; drop "subnormal" []; drop "zero" []; drop "x" []]);
    ("__builtin_fpclassifyl", unknown [drop "nan" []; drop "infinite" []; drop "normal" []; drop "subnormal" []; drop "zero" []; drop "x" []]);
    ("__fpclassify", unknown [drop "x" []]);
    ("__fpclassifyd", unknown [drop "x" []]);
    ("__fpclassifyf", unknown [drop "x" []]);
    ("__fpclassifyl", unknown [drop "x" []]);
  ]

let verifier_atomic_var = Goblintutil.create_var (makeGlobalVar "[__VERIFIER_atomic]" intType)
let verifier_atomic = AddrOf (Cil.var (Goblintutil.create_var verifier_atomic_var))

(** SV-COMP functions.
    Just the ones that require special handling and cannot be stubbed. *)
(* TODO: conditional on ana.sv-comp.functions option *)
let svcomp_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("__VERIFIER_atomic_begin", special [] @@ Lock { lock = verifier_atomic; try_ = false; write = true; return_on_success = true });
    ("__VERIFIER_atomic_end", special [] @@ Unlock verifier_atomic);
    ("__VERIFIER_nondet_loff_t", unknown []); (* cannot give it in sv-comp.c without including stdlib or similar *)
  ]

let ncurses_descs_list: (string * LibraryDesc.t) list = LibraryDsl.[
    ("echo", unknown []);
    ("noecho", unknown []);
    ("wattrset", unknown [drop "win" [r_deep; w_deep]; drop "attrs" []]);
    ("endwin", unknown []);
    ("wgetch", unknown [drop "win" [r_deep; w_deep]]);
    ("wmove", unknown [drop "win" [r_deep; w_deep]; drop "y" []; drop "x" []]);
    ("waddch", unknown [drop "win" [r_deep; w_deep]; drop "ch" []]);
    ("waddnwstr", unknown [drop "win" [r_deep; w_deep]; drop "wstr" [r]; drop "n" []]);
    ("wattr_on", unknown [drop "win" [r_deep; w_deep]; drop "attrs" []; drop "opts" []]); (* opts argument currently not used *)
    ("wrefresh", unknown [drop "win" [r_deep; w_deep]]);
    ("mvprintw", unknown (drop "win" [r_deep; w_deep] :: drop "y" [] :: drop "x" [] :: drop "fmt" [r] :: VarArgs (drop' [r])));
    ("initscr", unknown []);
    ("curs_set", unknown [drop "visibility" []]);
    ("wtimeout", unknown [drop "win" [r_deep; w_deep]; drop "delay" []]);
    ("start_color", unknown []);
    ("use_default_colors", unknown []);
    ("wclear", unknown [drop "win" [r_deep; w_deep]]);
    ("can_change_color", unknown []);
    ("init_color", unknown [drop "color" []; drop "red" []; drop "green" []; drop "blue" []]);
    ("init_pair", unknown [drop "pair" []; drop "f" [r]; drop "b" [r]]);
    ("wbkgd", unknown [drop "win" [r_deep; w_deep]; drop "ch" []]);
  ]

(* TODO: allow selecting which lists to use *)
let library_descs = Hashtbl.of_list (List.concat [
    c_descs_list;
    posix_descs_list;
    pthread_descs_list;
    gcc_descs_list;
    glibc_desc_list;
    linux_userspace_descs_list;
    linux_descs_list;
    goblint_descs_list;
    zstd_descs_list;
    math_descs_list;
    svcomp_descs_list;
    ncurses_descs_list;
  ])


type categories = [
  | `Malloc       of exp
  | `Calloc       of exp * exp
  | `Realloc      of exp * exp
  | `Lock         of bool * bool * bool  (* try? * write? * return  on success *)
  | `Unlock
  | `ThreadCreate of exp * exp * exp (* id * f  * x       *)
  | `ThreadJoin   of exp * exp (* id * ret_var *)
  | `Unknown      of string ]


let classify fn exps: categories =
  let strange_arguments () =
    M.warn ~category:Program "%s arguments are strange!" fn;
    `Unknown fn
  in
  match fn with
  | "pthread_join" ->
    begin match exps with
      | [id; ret_var] -> `ThreadJoin (id, ret_var)
      | _ -> strange_arguments ()
    end
  | "kmalloc" | "__kmalloc" | "usb_alloc_urb" | "__builtin_alloca" ->
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
  | "_spin_trylock" | "spin_trylock" | "mutex_trylock" | "_spin_trylock_irqsave"
  | "down_trylock"
    -> `Lock(true, true, true)
  | "pthread_mutex_trylock" | "pthread_rwlock_trywrlock" | "pthread_spin_trylock"
    -> `Lock (true, true, false)
  | "_spin_lock" | "_spin_lock_irqsave" | "_spin_lock_bh" | "down_write"
  | "mutex_lock" | "mutex_lock_interruptible" | "_write_lock" | "_raw_write_lock"
  | "pthread_rwlock_wrlock" | "GetResource" | "_raw_spin_lock"
  | "_raw_spin_lock_flags" | "_raw_spin_lock_irqsave" | "_raw_spin_lock_irq" | "_raw_spin_lock_bh"
  | "spin_lock" | "pthread_spin_lock"
    -> `Lock (get_bool "sem.lock.fail", true, true)
  | "pthread_mutex_lock" | "__pthread_mutex_lock"
    -> `Lock (get_bool "sem.lock.fail", true, false)
  | "pthread_rwlock_tryrdlock" | "pthread_rwlock_rdlock" | "_read_lock"  | "_raw_read_lock"
  | "down_read"
    -> `Lock (get_bool "sem.lock.fail", false, true)
  | "__raw_read_unlock" | "__raw_write_unlock"  | "raw_spin_unlock"
  | "_spin_unlock" | "spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh" | "_raw_spin_unlock_bh"
  | "mutex_unlock" | "_write_unlock" | "_read_unlock"
  | "pthread_mutex_unlock" | "__pthread_mutex_unlock" | "up_read" | "up_write"
  | "up" | "pthread_spin_unlock"
    -> `Unlock
  | x -> `Unknown x


module Invalidate =
struct
  [@@@warning "-unused-value-declaration"] (* some functions are not used below *)
  open AccessKind

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
    | Write | Spawn -> f a x @ drop n x
    | Read  -> f a x
    | Free  -> []

  let readsAllButFirst n f a x =
    match a with
    | Write | Spawn -> f a x
    | Read  -> f a x @ drop n x
    | Free  -> []

  let reads ns a x =
    let i, o = partition ns x in
    match a with
    | Write | Spawn -> o
    | Read  -> i
    | Free  -> []

  let writes ns a x =
    let i, o = partition ns x in
    match a with
    | Write | Spawn -> i
    | Read  -> o
    | Free  -> []

  let frees ns a x =
    let i, o = partition ns x in
    match a with
    | Write | Spawn -> []
    | Read  -> o
    | Free  -> i

  let readsFrees rs fs a x =
    match a with
    | Write | Spawn -> []
    | Read  -> keep rs x
    | Free  -> keep fs x

  let onlyReads ns a x =
    match a with
    | Write | Spawn -> []
    | Read  -> keep ns x
    | Free  -> []

  let onlyWrites ns a x =
    match a with
    | Write | Spawn -> keep ns x
    | Read  -> []
    | Free  -> []

  let readsWrites rs ws a x =
    match a with
    | Write | Spawn -> keep ws x
    | Read  -> keep rs x
    | Free  -> []

  let readsAll a x =
    match a with
    | Write | Spawn -> []
    | Read  -> x
    | Free  -> []

  let writesAll a x =
    match a with
    | Write | Spawn -> x
    | Read  -> []
    | Free  -> []
end

open Invalidate

(* Data races: which arguments are read/written?
 * We assume that no known functions that are reachable are executed/spawned. For that we use ThreadCreate above. *)
(* WTF: why are argument numbers 1-indexed (in partition)? *)
let invalidate_actions = [
    "atoi", readsAll;             (*safe*)
    "__builtin_ctz", readsAll;
    "__builtin_ctzl", readsAll;
    "__builtin_ctzll", readsAll;
    "__builtin_clz", readsAll;
    "connect", readsAll;          (*safe*)
    "fclose", readsAll;           (*safe*)
    "fflush", writesAll;          (*unsafe*)
    "fopen", readsAll;            (*safe*)
    "fdopen", readsAll;           (*safe*)
    "setvbuf", writes[1;2];       (* TODO: if this is used to set an input buffer, the buffer (second argument) would need to remain TOP, *)
                                  (* as any future write (or flush) of the stream could result in a write to the buffer *)
    "fprintf", writes [1];          (*keep [1]*)
    "__fprintf_chk", writes [1];    (*keep [1]*)
    "fread", writes [1;4];
    "__fread_alias", writes [1;4];
    "__fread_chk", writes [1;4];
    "utimensat", readsAll;
    "free", frees [1]; (*unsafe*)
    "fwrite", readsAll;(*safe*)
    "getopt", writes [2];(*keep [2]*)
    "localtime", readsAll;(*safe*)
    "mempcpy", writes [1];(*keep [1]*)
    "__builtin___mempcpy_chk", writes [1];
    "printf", readsAll;(*safe*)
    "__printf_chk", readsAll;(*safe*)
    "printk", readsAll;(*safe*)
    "perror", readsAll;(*safe*)
    "pthread_mutex_lock", readsAll;(*safe*)
    "pthread_mutex_trylock", readsAll;
    "pthread_mutex_unlock", readsAll;(*safe*)
    "pthread_spin_lock", readsAll;(*safe*)
    "pthread_spin_trylock", readsAll;
    "pthread_spin_unlock", readsAll;(*safe*)
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
    "pthread_spin_init", readsAll;(*safe*)
    "pthread_spin_destroy", readsAll;(*safe*)
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
    "strncat", writes [1];(*keep [1]*)
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
    "pthread_attr_getdetachstate", readsAll;(*safe*)
    "pthread_attr_getstacksize", readsAll;(*safe*)
    "pthread_attr_getscope", readsAll;(*safe*)
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
    "__builtin___strcpy", writes [1];(*keep [1]*)
    "__builtin___strcpy_chk", writes [1];(*keep [1]*)
    "strcat", writes [1];(*keep [1]*)
    "strtok", readsAll;(*safe*)
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
    "rewind", writesAll;
    "fileno", readsAll;
    "ferror", readsAll;
    "ftell", readsAll;
    "putc", readsAll;(*safe*)
    "putw", readsAll;(*safe*)
    "putchar", readsAll;(*safe*)
    "getchar", readsAll;(*safe*)
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
    "stat", writes [2]; (*keep [1]*)
    "__xstat", writes [3]; (*keep [1]*)
    "__lxstat", writes [3]; (*keep [1]*)
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
    "idr_pre_get", readsAll;
    "zil_replay", writes [1;2;3;5];
    "__VERIFIER_nondet_int", readsAll; (* no args, declare invalidate actions to prevent invalidating globals when extern in regression tests *)
    (* no args, declare invalidate actions to prevent invalidating globals *)
    "__VERIFIER_atomic_begin", readsAll;
    "__VERIFIER_atomic_end", readsAll;
    "isatty", readsAll;
    "setpriority", readsAll;
    "getpriority", readsAll;
    (* ddverify *)
    "spin_lock_init", readsAll;
    "spin_lock", readsAll;
    "spin_unlock", readsAll;
    "sema_init", readsAll;
    "down_trylock", readsAll;
    "up", readsAll;
    "acos", readsAll;
    "acosf", readsAll;
    "acosh", readsAll;
    "acoshf", readsAll;
    "acoshl", readsAll;
    "acosl", readsAll;
    "asin", readsAll;
    "asinf", readsAll;
    "asinh", readsAll;
    "asinhf", readsAll;
    "asinhl", readsAll;
    "asinl", readsAll;
    "atan", readsAll;
    "atan2", readsAll;
    "atan2f", readsAll;
    "atan2l", readsAll;
    "atanf", readsAll;
    "atanh", readsAll;
    "atanhf", readsAll;
    "atanhl", readsAll;
    "atanl", readsAll;
    "cbrt", readsAll;
    "cbrtf", readsAll;
    "cbrtl", readsAll;
    "ceil", readsAll;
    "ceilf", readsAll;
    "ceill", readsAll;
    "copysign", readsAll;
    "copysignf", readsAll;
    "copysignl", readsAll;
    "cos", readsAll;
    "cosf", readsAll;
    "cosh", readsAll;
    "coshf", readsAll;
    "coshl", readsAll;
    "cosl", readsAll;
    "erf", readsAll;
    "erfc", readsAll;
    "erfcf", readsAll;
    "erfcl", readsAll;
    "erff", readsAll;
    "erfl", readsAll;
    "exp", readsAll;
    "exp2", readsAll;
    "exp2f", readsAll;
    "exp2l", readsAll;
    "expf", readsAll;
    "expl", readsAll;
    "expm1", readsAll;
    "expm1f", readsAll;
    "expm1l", readsAll;
    "fdim", readsAll;
    "fdimf", readsAll;
    "fdiml", readsAll;
    "fma", readsAll;
    "fmaf", readsAll;
    "fmal", readsAll;
    "fmax", readsAll;
    "fmaxf", readsAll;
    "fmaxl", readsAll;
    "fmin", readsAll;
    "fminf", readsAll;
    "fminl", readsAll;
    "fmod", readsAll;
    "fmodf", readsAll;
    "fmodl", readsAll;
    "frexp", readsAll;
    "frexpf", readsAll;
    "frexpl", readsAll;
    "hypot", readsAll;
    "hypotf", readsAll;
    "hypotl", readsAll;
    "ilogb", readsAll;
    "ilogbf", readsAll;
    "ilogbl", readsAll;
    "j0", readsAll;
    "j1", readsAll;
    "jn", readsAll;
    "ldexp", readsAll;
    "ldexpf", readsAll;
    "ldexpl", readsAll;
    "lgamma", readsAll;
    "lgammaf", readsAll;
    "lgammal", readsAll;
    "llrint", readsAll;
    "llrintf", readsAll;
    "llrintl", readsAll;
    "llround", readsAll;
    "llroundf", readsAll;
    "llroundl", readsAll;
    "log", readsAll;
    "log10", readsAll;
    "log10f", readsAll;
    "log10l", readsAll;
    "log1p", readsAll;
    "log1pf", readsAll;
    "log1pl", readsAll;
    "log2", readsAll;
    "log2f", readsAll;
    "log2l", readsAll;
    "logb", readsAll;
    "logbf", readsAll;
    "logbl", readsAll;
    "logf", readsAll;
    "logl", readsAll;
    "lrint", readsAll;
    "lrintf", readsAll;
    "lrintl", readsAll;
    "lround", readsAll;
    "lroundf", readsAll;
    "lroundl", readsAll;
    "modf", readsAll;
    "modff", readsAll;
    "modfl", readsAll;
    "nan", readsAll;
    "nanf", readsAll;
    "nanl", readsAll;
    "nearbyint", readsAll;
    "nearbyintf", readsAll;
    "nearbyintl", readsAll;
    "nextafter", readsAll;
    "nextafterf", readsAll;
    "nextafterl", readsAll;
    "nexttoward", readsAll;
    "nexttowardf", readsAll;
    "nexttowardl", readsAll;
    "pow", readsAll;
    "powf", readsAll;
    "powl", readsAll;
    "remainder", readsAll;
    "remainderf", readsAll;
    "remainderl", readsAll;
    "remquo", readsAll;
    "remquof", readsAll;
    "remquol", readsAll;
    "rint", readsAll;
    "rintf", readsAll;
    "rintl", readsAll;
    "round", readsAll;
    "roundf", readsAll;
    "roundl", readsAll;
    "scalbln", readsAll;
    "scalblnf", readsAll;
    "scalblnl", readsAll;
    "scalbn", readsAll;
    "scalbnf", readsAll;
    "scalbnl", readsAll;
    "sin", readsAll;
    "sinf", readsAll;
    "sinh", readsAll;
    "sinhf", readsAll;
    "sinhl", readsAll;
    "sinl", readsAll;
    "sqrt", readsAll;
    "sqrtf", readsAll;
    "sqrtl", readsAll;
    "tan", readsAll;
    "tanf", readsAll;
    "tanh", readsAll;
    "tanhf", readsAll;
    "tanhl", readsAll;
    "tanl", readsAll;
    "tgamma", readsAll;
    "tgammaf", readsAll;
    "tgammal", readsAll;
    "trunc", readsAll;
    "truncf", readsAll;
    "truncl", readsAll;
    "y0", readsAll;
    "y1", readsAll;
    "yn", readsAll;
    "__goblint_assume_join", readsAll;
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


let lib_funs = ref (Set.String.of_list ["__raw_read_unlock"; "__raw_write_unlock"; "spin_trylock"])
let add_lib_funs funs = lib_funs := List.fold_right Set.String.add funs !lib_funs
let use_special fn_name = Set.String.mem fn_name !lib_funs

let kernel_safe_uncalled = Set.String.of_list ["__inittest"; "init_module"; "__exittest"; "cleanup_module"]
let kernel_safe_uncalled_regex = List.map Str.regexp ["__check_.*"]
let is_safe_uncalled fn_name =
  Set.String.mem fn_name kernel_safe_uncalled ||
  List.exists (fun r -> Str.string_match r fn_name 0) kernel_safe_uncalled_regex


let unknown_desc ~f name = (* TODO: remove name argument, unknown function shouldn't have classify *)
  let old_accesses (kind: AccessKind.t) args = match kind with
    | Write when GobConfig.get_bool "sem.unknown_function.invalidate.args" -> args
    | Write -> []
    | Read when GobConfig.get_bool "sem.unknown_function.read.args" -> args
    | Read -> []
    | Free -> []
    | Spawn when get_bool "sem.unknown_function.spawn" -> args
    | Spawn -> []
  in
  let attrs: LibraryDesc.attr list =
    if GobConfig.get_bool "sem.unknown_function.invalidate.globals" then
      [InvalidateGlobals]
    else
      []
  in
  let classify_name args =
    match classify name args with
    | `Unknown _ as category ->
      (* TODO: remove hack when all classify are migrated *)
      if not (CilType.Varinfo.equal f dummyFunDec.svar) && not (use_special f.vname) then
        M.error ~category:Imprecise ~tags:[Category Unsound] "Function definition missing for %s" f.vname;
      category
    | category -> category
  in
  LibraryDesc.of_old ~attrs old_accesses classify_name

let find f =
  let name = f.vname in
  match Hashtbl.find_option library_descs name with
  | Some desc -> desc
  | None ->
    match get_invalidate_action name with
    | Some old_accesses ->
      LibraryDesc.of_old old_accesses (classify name)
    | None ->
      unknown_desc ~f name


let is_special fv =
  if use_special fv.vname then
    true
  else
    match Cilfacade.find_varinfo_fundec fv with
    | _ -> false
    | exception Not_found -> true
