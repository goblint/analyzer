  $ goblint --enable warn.deterministic 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test ancient solvers:

  $ goblint --enable warn.deterministic --set solver WL 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver effectWConEq 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9


Test topdown solvers:

  $ goblint --enable warn.deterministic --set solver topdown_deprecated 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9

  $ goblint --enable warn.deterministic --set solver topdown_term 01-assert.c
  [Error] Fixpoint not reached at L:entry state of main (299) on 01-assert.c:4:1-15:1
   Solver computed:
   bot
   Right-Hand-Side:
   HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                                                                                   mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                                                                                   base:({
                                                                                                                           }, {}, {}, {}),
                                                                                                                   threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                                                                                   threadflag:Singlethreaded,
                                                                                                                   threadreturn:true,
                                                                                                                   escape:{},
                                                                                                                   mutexEvents:(),
                                                                                                                   access:(),
                                                                                                                   mutex:(lockset:{}, multiplicity:{}),
                                                                                                                   race:(),
                                                                                                                   mhp:(),
                                                                                                                   assert:(),
                                                                                                                   pthreadMutexType:()], map:{})}
   Difference: HConsed lifted PathSensitive (ProjectiveSet (MCP.D * map)):PathSensitive (ProjectiveSet (MCP.D * map)):{(MCP.D:[expRelation:(),
                                                                                                                               mallocWrapper:(wrapper call:Unknown node, unique calls:{}),
                                                                                                                               base:({
                                                                                                                                       }, {}, {}, {}),
                                                                                                                               threadid:(wrapper call:unknown node, Thread:[main], created:(current function:bot, callees:bot)),
                                                                                                                               threadflag:Singlethreaded,
                                                                                                                               threadreturn:true,
                                                                                                                               escape:{},
                                                                                                                               mutexEvents:(),
                                                                                                                               access:(),
                                                                                                                               mutex:(lockset:{}, multiplicity:{}),
                                                                                                                               race:(),
                                                                                                                               mhp:(),
                                                                                                                               assert:(),
                                                                                                                               pthreadMutexType:()], map:{})} instead of bot
  
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 4..7 (01-assert.c:4-7)
    on lines 10..14 (01-assert.c:10-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 0
    dead: 9
    total lines: 9
  [Error][Unsound] Fixpoint not reached
  [3]

  $ goblint --enable warn.deterministic --set solver topdown_space_cache_term 01-assert.c
  [Error] Cannot find value 'solvers.wp.restore' in
  {"files":["01-assert.c"],"outfile":"","justcil":false,"justcfg":false,"verify":true,"mainfun":["main"],"exitfun":[],"otherfun":[],"allglobs":false,"kernel":false,"dump_globs":false,"result":"none","solver":"topdown_space_cache_term","comparesolver":"","allfuns":false,"nonstatic":false,"colors":"auto","g2html":false,"save_run":"","load_run":"","compare_runs":[],"warn_at":"post","gobview":false,"jobs":1,"goblint-dir":".goblint","pre":{"enabled":true,"keep":false,"exist":false,"includes":[],"kernel_includes":[],"custom_includes":[],"kernel-root":"","cppflags":[],"compdb":{"original-path":"","split":false},"transform-paths":true},"cil":{"merge":{"inlines":true},"cstd":"c99","gnu89inline":false,"addNestedScopeAttr":false},"server":{"enabled":false,"mode":"stdio","unix-socket":"goblint.sock","reparse":false},"ana":{"activated":["expRelation","base","threadid","threadflag","threadreturn","escape","mutexEvents","mutex","access","race","mallocWrapper","mhp","assert","pthreadMutexType"],"path_sens":["mutex","malloc_null","uninit","expsplit","activeSetjmp","memLeak","apron","affeq","lin2vareq"],"ctx_insens":["stack_loc","stack_trace_set"],"ctx_sens":[],"setjmp":{"split":"precise"},"int":{"def_exc":true,"interval":false,"interval_set":false,"enums":false,"congruence":false,"refinement":"never","def_exc_widen_by_join":false,"interval_narrow_by_meet":false,"interval_threshold_widening":false,"interval_threshold_widening_constants":"all"},"float":{"interval":false,"evaluate_math_functions":false},"pml":{"debug":true},"opt":{"hashcons":true,"equal":true},"autotune":{"enabled":false,"activated":["congruence","singleThreaded","specification","mallocWrappers","noRecursiveIntervals","enums","loopUnrollHeuristic","arrayDomain","octagon","wideningThresholds","memsafetySpecification","termination","tmpSpecialAnalysis"]},"sv-comp":{"enabled":false,"functions":false},"specification":"","wp":false,"arrayoob":false,"base":{"context":{"non-ptr":true,"int":true,"interval":true,"interval_set":true},"strings":{"domain":"flat"},"partition-arrays":{"keep-expr":"first","partition-by-const-on-return":false,"smart-join":false},"arrays":{"domain":"trivial","unrolling-factor":0,"nullbytes":false},"structs":{"domain":"simple","key":{"forward":true,"avoid-ints":true,"prefer-ptrs":true}},"privatization":"protection-read","priv":{"not-started":true,"must-joined":true},"invariant":{"enabled":true,"blobs":false,"unassume":"once","int":{"simplify":"all"}},"eval":{"deep-query":true}},"malloc":{"wrappers":["kmalloc","__kmalloc","usb_alloc_urb","__builtin_alloca","kzalloc"],"unique_address_count":0},"apron":{"strengthening":false,"domain":"octagon","threshold_widening":false,"threshold_widening_constants":"all","invariant":{"diff-box":false}},"relation":{"context":true,"privatization":"mutex-meet","priv":{"not-started":true,"must-joined":true},"invariant":{"one-var":false,"local":true,"global":true}},"context":{"widen":false,"gas_value":-1,"gas_scope":"global","callString_length":2},"thread":{"domain":"history","include-node":true,"wrappers":[],"unique_thread_id_count":0,"context":{"create-edges":true}},"race":{"free":true,"call":true,"direct-arithmetic":false,"volatile":true},"dead-code":{"lines":true,"branches":true,"functions":true},"extract-pthread":{"assume_success":true,"ignore_assign":true},"widen":{"tokens":false},"unassume":{"precheck":false}},"incremental":{"load":false,"load-dir":"incremental_data","only-rename":false,"save":false,"save-dir":"incremental_data","stable":true,"wpoint":false,"reluctant":{"enabled":false},"compare":"ast","detect-renames":true,"force-reanalyze":{"funs":[]},"restart":{"sided":{"enabled":false,"vars":"all","fuel":-1,"fuel-only-global":false},"list":[],"write-only":true},"postsolver":{"enabled":true,"superstable-reached":false}},"lib":{"activated":["c","posix","pthread","gcc","glibc","linux-userspace","goblint","ncurses","legacy"]},"sem":{"unknown_function":{"spawn":true,"call":true,"invalidate":{"globals":true,"args":true},"read":{"args":true}},"builtin_unreachable":{"dead_code":false},"noreturn":{"dead_code":false},"int":{"signed_overflow":"assume_top"},"null-pointer":{"dereference":"assume_none"},"malloc":{"fail":false},"lock":{"fail":false},"assert":{"refine":true},"atexit":{"ignore":false}},"trans":{"activated":[],"expeval":{"query_file_name":""},"output":"transformed.c","assert":{"function":"__VERIFIER_assert","wrap-atomic":true}},"annotation":{"int":{"enabled":false,"privglobs":true},"float":{"enabled":false},"goblint_context":{"__additional__":[]},"goblint_precision":{"__additional__":[]},"goblint_array_domain":false,"goblint_relation_track":false},"exp":{"priv-prec-dump":"","priv-distr-init":false,"relation":{"prec-dump":""},"cfgdot":false,"mincfg":false,"earlyglobs":false,"region-offsets":false,"unique":[],"forward":false,"volatiles_are_top":true,"single-threaded":false,"globs_are_top":false,"exclude_from_earlyglobs":[],"exclude_from_invalidation":[],"g2html_path":"","extraspecials":[],"no-narrow":false,"basic-blocks":false,"fast_global_inits":true,"architecture":"64bit","gcc_path":"/usr/bin/gcc","cpp-path":"","unrolling-factor":0,"hide-std-globals":true,"arg":{"enabled":false,"dot":{"path":"","node-label":"node"}}},"dbg":{"level":"info","timing":{"enabled":false,"tef":""},"trace":{"context":false},"dump":"","cilout":"","justcil-printer":"default","timeout":"0","solver-stats-interval":10,"solver-signal":"sigusr1","backtrace-signal":"sigusr2","solver-progress":false,"print_wpoints":false,"slice":{"on":false,"n":10},"limit":{"widen":0},"warn_with_context":false,"regression":false,"test":{"domain":false},"cilcfgdot":false,"cfg":{"loop-clusters":false,"loop-unrolling":false},"compare_runs":{"globsys":false,"eqsys":true,"global":false,"node":false,"diff":false},"print_tids":false,"print_protection":false,"run_cil_check":false,"full-output":false},"warn":{"assert":true,"behavior":true,"call":true,"integer":true,"float":true,"cast":true,"race":true,"deadlock":true,"deadcode":true,"analyzer":true,"unsound":true,"imprecise":true,"witness":true,"program":true,"termination":true,"unknown":true,"error":true,"warning":true,"info":true,"debug":false,"success":true,"quote-code":false,"race-threshold":0,"deterministic":true,"memleak":{"memcleanup":false,"memtrack":false}},"solvers":{"td3":{"term":true,"side_widen":"sides","space":false,"space_cache":true,"space_restore":true,"narrow-reuse":true,"remove-wpoint":true,"skip-unchanged-rhs":false,"restart":{"wpoint":{"enabled":false,"once":false}},"verify":false},"slr4":{"restart_count":1}},"witness":{"graphml":{"enabled":false,"path":"witness.graphml","id":"node","minimize":false,"uncil":false,"stack":true,"unknown":true},"invariant":{"loop-head":true,"after-lock":true,"other":true,"split-conjunction":true,"accessed":false,"full":true,"exact":true,"inexact-type-bounds":false,"exclude-vars":["tmp\\(___[0-9]+\\)?","cond","RETURN"],"all-locals":true,"goblint":false,"typedefs":true},"yaml":{"enabled":false,"format-version":"0.1","entry-types":["location_invariant","loop_invariant","flow_insensitive_invariant","loop_invariant_certificate","precondition_loop_invariant_certificate","invariant_set"],"invariant-types":["location_invariant","loop_invariant"],"path":"witness.yml","validate":"","strict":false,"unassume":"","certificate":""}}}
  Did You forget to add default values to options.schema.json?
  
  [Info] runtime: 00:00:00.061
  [Info] vars: 2, evals: 12
  [Info] max updates: 1 for var L:call of main (299) on 01-assert.c:4:1-15:1
  
  
  Memory statistics: total=23.99MB, max=7.06MB, minor=21.98MB, major=6.53MB, promoted=4.52MB
      minor collections=10  major collections=1 compactions=0
  
  
  Fatal error: exception Failure("get_path_string")
  [2]

  $ goblint --enable warn.deterministic --set solver td3 01-assert.c
  [Error][Assert] Assertion "fail" will fail. (01-assert.c:12:3-12:25)
  [Warning][Assert] Assertion "unknown == 4" is unknown. (01-assert.c:11:3-11:33)
  [Success][Assert] Assertion "success" will succeed (01-assert.c:10:3-10:28)
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 13..14 (01-assert.c:13-14)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 2
    total lines: 9
