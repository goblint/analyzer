open PrecisionUtil
open GobConfig

type config_options =
  | AnaIntIntervalThresholdWidening
  | AnaIntIntervalNarrowByMeet
  | AnaIntDefExcWidenByJoin
  | AnaIntIntervalThresholdWideningConstants
  | AnaIntRefinement
  | AnaBaseEvalDeepQuery
  | AnaBaseInvariantEnabled
  | AnaBasePartitionArraysPartitionByConstOnReturn
  | AnaBasePrivNotStarted
  | AnaBasePrivMustJoined
  | AnaBasePrivatization
  | AnnotationIntPrivGlobs
  | AnnotationIntEnabled
  | AnnotationGoblintArrayDomain
  | ExpEarlyGlobs
  | ExpVolatilesAreTop
  | ExpGlobsAreTop
  | ExpPrivDistrInit
  | ExpPrivPrecDump
  | Kernel
  | SemUnknownFunctionSpawn
  | SemMallocFail
  | SemIntSignedOverflow
  | SemNullPointerDereference
  | DbgPrintProtection

(** Define a record that holds mutable variables representing certain Configuration values.
  * These values are used to keep track of whether or not the corresponding Config values are en-/disabled  *)
  type ana_int_config_values = {
    mutable interval_threshold_widening : float_precision option;
    mutable interval_narrow_by_meet : float_precision option;
    mutable def_exc_widen_by_join : float_precision option;
    mutable interval_threshold_widening_constants : string option;
    mutable refinement : string option;
  }

  type ana_base_config_values = {
    mutable deep_query : float_precision option;
    mutable invariant_enabled : float_precision option;
    mutable partition_by_const_on_return : float_precision option;
    mutable priv_not_started : float_precision option;
    mutable priv_must_joined : float_precision option;
    mutable privatization : string option;
  }

  type annotation_config_values = {
    mutable int_privglobs : float_precision option;
    mutable int_enabled : float_precision option;
    mutable goblint_array_domain : float_precision option;
  }

  type exp_config_values = {
    mutable earlyglobs : float_precision option;
    mutable volatiles_are_top : float_precision option;
    mutable globs_are_top : float_precision option;
    mutable priv_distr_init : float_precision option;
    mutable priv_prec_dump : string option;
  }

  type kernel_config_values = {
    mutable kernel : float_precision option;
  }

  type sem_config_values = {
    mutable unknown_function_spawn : float_precision option;  
    mutable malloc_fail : float_precision option;
    mutable signed_overflow : string option;
    mutable null_pointer_dereference: string option;
  }
     
  type dbg_config_values = {
    mutable print_protection : float_precision option;
  }
  
  
  let ana_int_config: ana_int_config_values = {
    interval_threshold_widening = None;
    interval_narrow_by_meet = None;
    def_exc_widen_by_join = None;
    interval_threshold_widening_constants = None;
    refinement = None;
  }

  let ana_base_config: ana_base_config_values = {
    deep_query = None;
    invariant_enabled = None;
    partition_by_const_on_return = None;
    priv_not_started = None;
    priv_must_joined = None;
    privatization = None;
  }

  let annotation_config: annotation_config_values = {
    int_privglobs = None;
    int_enabled = None;
    goblint_array_domain = None;
  }

  let exp_config: exp_config_values = {
    earlyglobs = None;
    volatiles_are_top = None;
    globs_are_top = None;
    priv_distr_init = None;
    priv_prec_dump = None;
  }

  let kernel_config: kernel_config_values = {
    kernel = None;
  }

  let sem_config: sem_config_values = {
    unknown_function_spawn = None;
    malloc_fail = None;
    signed_overflow = None;
    null_pointer_dereference = None;
  }

  let dbg_config: dbg_config_values = {
    print_protection = None;
  }
  

  let extract_from_option x =
      match x with
      | None -> failwith "Config option not read"
      | Some x -> x
  
  let get_interval_threshold_widening () =
    if ana_int_config.interval_threshold_widening = None then 
      ana_int_config.interval_threshold_widening <- Some (get_bool "ana.int.interval_threshold_widening");
    ana_int_config.interval_threshold_widening
          
  let get_interval_narrow_by_meet () =
    if ana_int_config.interval_narrow_by_meet = None then
      ana_int_config.interval_narrow_by_meet <- Some (get_bool "ana.int.interval_narrow_by_meet");
    ana_int_config.interval_narrow_by_meet
  
  let get_def_exc_widen_by_join () = 
    if ana_int_config.def_exc_widen_by_join = None then 
      ana_int_config.def_exc_widen_by_join <- Some (get_bool "ana.int.def_exc_widen_by_join");
    ana_int_config.def_exc_widen_by_join
  
  let get_signed_overflow () =
    if sem_config.signed_overflow = None then
      sem_config.signed_overflow <- Some (get_string "sem.int.signed_overflow");
    sem_config.signed_overflow
  
  let get_interval_threshold_widening_constants () =
    if ana_int_config.interval_threshold_widening_constants = None then
      ana_int_config.interval_threshold_widening_constants <- Some (get_string "ana.int.interval_threshold_widening_constants");
    ana_int_config.interval_threshold_widening_constants
  
  let get_refinement () =
    if ana_int_config.refinement = None then
      ana_int_config.refinement <- Some (get_string "ana.int.refinement");
    ana_int_config.refinement

  let get_dereference () =
    if sem_config.null_pointer_dereference = None then
      sem_config.null_pointer_dereference <- Some (get_string "sem.null-pointer.dereference");
    sem_config.null_pointer_dereference

  let get_privglobs () =
    if annotation_config.int_privglobs = None then
      annotation_config.int_privglobs <- Some (get_bool "annotation.int.privglobs");
    annotation_config.int_privglobs

  let get_annotation_int_enabled () =
    if annotation_config.int_enabled = None then
      annotation_config.int_enabled <- Some (get_bool "annotation.int.enabled");
    annotation_config.int_enabled

  let get_goblint_array_domain () =
    if annotation_config.goblint_array_domain = None then
      annotation_config.goblint_array_domain <- Some (get_bool "annotation.goblint_array_domain");
    annotation_config.goblint_array_domain

  let get_deep_query () =
    if ana_base_config.deep_query = None then
      ana_base_config.deep_query <- Some (get_bool "ana.base.eval.deep-query");
    ana_base_config.deep_query

  let get_ana_base_invariant_enabled () =
    if ana_base_config.invariant_enabled = None then
      ana_base_config.invariant_enabled <- Some (get_bool "ana.base.invariant.enabled");
    ana_base_config.invariant_enabled

  let get_exp_earlyglobs () =
    if exp_config.earlyglobs = None then
      exp_config.earlyglobs <- Some (get_bool "exp.earlyglobs");
    exp_config.earlyglobs

  let get_volatiles_are_top () =
    if exp_config.volatiles_are_top = None then
      exp_config.volatiles_are_top <- Some (get_bool "exp.volatiles_are_top");
    exp_config.volatiles_are_top

  let get_globs_are_top () =
    if exp_config.globs_are_top = None then
      exp_config.globs_are_top <- Some (get_bool "exp.globs_are_top");
    exp_config.globs_are_top

  let get_partition_by_const_on_return () =
    if ana_base_config.partition_by_const_on_return = None then
      ana_base_config.partition_by_const_on_return <- Some (get_bool "ana.base.partition-arrays.partition-by-const-on-return");
    ana_base_config.partition_by_const_on_return

  let get_kernel () =
    if kernel_config.kernel = None then
      kernel_config.kernel <- Some (get_bool "kernel");
    kernel_config.kernel
  
  let get_unknown_function_spawn () =
    if sem_config.unknown_function_spawn = None then
      sem_config.unknown_function_spawn <- Some (get_bool "sem.unknown_function.spawn");
    sem_config.unknown_function_spawn

  let get_sem_malloc_fail () =
    if sem_config.malloc_fail = None then
      sem_config.malloc_fail <- Some (get_bool "sem.malloc.fail");
    sem_config.malloc_fail
  
  let get_priv_prec_dump () =
    if exp_config.priv_prec_dump = None then
      exp_config.priv_prec_dump <- Some (get_string "exp.priv-prec-dump");
    exp_config.priv_prec_dump

  let get_ana_base_privatization () =
    if ana_base_config.privatization = None then
      ana_base_config.privatization <- Some (get_string "ana.base.privatization");
    ana_base_config.privatization

  let get_priv_not_started () =
    if ana_base_config.priv_not_started = None then
      ana_base_config.priv_not_started <- Some (get_bool "ana.base.priv.not-started");
    ana_base_config.priv_not_started

  let get_priv_must_joined () =
    if ana_base_config.priv_must_joined = None then
      ana_base_config.priv_must_joined <- Some (get_bool "ana.base.priv.must-joined");
    ana_base_config.priv_must_joined

  let get_priv_distr_init () =
    if exp_config.priv_distr_init = None then
      exp_config.priv_distr_init <- Some (get_bool "exp.priv-distr-init");
    exp_config.priv_distr_init

  let get_print_protection () =
    if dbg_config.print_protection = None then
      dbg_config.print_protection <- Some (get_bool "dbg.print_protection");
    dbg_config.print_protection

  let get_bool_config_value conf : float_precision option =
    match conf with
    | AnaIntIntervalThresholdWidening -> get_interval_threshold_widening ()
    | AnaIntIntervalNarrowByMeet -> get_interval_narrow_by_meet ()      
    | AnaIntDefExcWidenByJoin -> get_def_exc_widen_by_join ()
    | AnnotationIntPrivGlobs -> get_privglobs ()
    | AnnotationIntEnabled -> get_annotation_int_enabled ()
    | AnnotationGoblintArrayDomain -> get_goblint_array_domain ()
    | AnaBaseEvalDeepQuery -> get_deep_query ()
    | AnaBaseInvariantEnabled -> get_ana_base_invariant_enabled ()
    | ExpEarlyGlobs -> get_exp_earlyglobs ()
    | ExpVolatilesAreTop -> get_volatiles_are_top ()
    | ExpGlobsAreTop -> get_globs_are_top ()
    | AnaBasePartitionArraysPartitionByConstOnReturn -> get_partition_by_const_on_return ()
    | Kernel -> get_kernel ()
    | SemUnknownFunctionSpawn -> get_unknown_function_spawn ()
    | SemMallocFail -> get_sem_malloc_fail ()
    | AnaBasePrivNotStarted -> get_priv_not_started ()
    | AnaBasePrivMustJoined -> get_priv_must_joined ()
    | ExpPrivDistrInit -> get_priv_distr_init ()
    | DbgPrintProtection -> get_print_protection ()
    | _ -> failwith "Tried to read invalid config value"
  
  let get_string_config_value conf : string option =
    match conf with
    | SemIntSignedOverflow -> get_signed_overflow ()
    | AnaIntIntervalThresholdWideningConstants -> get_interval_threshold_widening_constants ()
    | AnaIntRefinement -> get_refinement ()
    | SemNullPointerDereference -> get_dereference ()
    | ExpPrivPrecDump -> get_priv_prec_dump ()
    | AnaBasePrivatization -> get_ana_base_privatization ()
    | _ -> failwith "Tried to read invalid config value"