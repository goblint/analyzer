open PrecisionUtil
open GobConfig

(** Define a record that holds mutable variables representing certain Configuration values.
  * These values are used to keep track of whether or not the corresponding Config values are en-/disabled  *)
  type ana_config_values = {
    mutable interval_threshold_widening : float_precision option;
    mutable interval_narrow_by_meet : float_precision option;
    mutable def_exc_widen_by_join : float_precision option;
    mutable signed_overflow : string option;
    mutable interval_threshold_widening_constants : string option;
    mutable refinement : string option;
    mutable dereference: string option;
    mutable privglobs : float_precision option;
    mutable annotation_int_enabled : float_precision option;
    mutable goblint_array_domain : float_precision option;
    mutable deep_query : float_precision option;
    mutable ana_base_invariant_enabled : float_precision option;
    mutable exp_earlyglobs : float_precision option;
    mutable volatiles_are_top : float_precision option;
    mutable globs_are_top : float_precision option;
    mutable partition_by_const_on_return : float_precision option;
    mutable kernel : float_precision option;
    mutable unknown_function_spawn : float_precision option;
    mutable sem_malloc_fail : float_precision option;
    mutable priv_prec_dump : string option;
    mutable ana_base_privatization : string option;
    mutable priv_not_started : float_precision option;
    mutable priv_must_joined : float_precision option;
    mutable priv_distr_init : float_precision option;
    mutable print_protection : float_precision option;
  }
  
  let conf_values: ana_config_values = {
    interval_threshold_widening = None;
    interval_narrow_by_meet = None;
    def_exc_widen_by_join = None;
    signed_overflow = None;
    interval_threshold_widening_constants = None;
    refinement = None;
    dereference = None;
    privglobs = None;
    annotation_int_enabled = None;
    goblint_array_domain = None;
    deep_query = None;
    ana_base_invariant_enabled = None;
    exp_earlyglobs = None;
    volatiles_are_top = None;
    globs_are_top = None;
    partition_by_const_on_return = None;
    kernel = None;
    unknown_function_spawn = None;
    sem_malloc_fail = None;
    priv_prec_dump = None;
    ana_base_privatization = None;
    priv_not_started = None;
    priv_must_joined = None;
    priv_distr_init = None;
    print_protection = None;
  }
  
  let extract_from_option x =
      match x with
      | None -> failwith "Config option not read"
      | Some x -> x
  
  let get_interval_threshold_widening () =
    if conf_values.interval_threshold_widening = None then 
      conf_values.interval_threshold_widening <- Some (get_bool "ana.int.interval_threshold_widening");
    conf_values.interval_threshold_widening
          
  let get_interval_narrow_by_meet () =
    if conf_values.interval_narrow_by_meet = None then
      conf_values.interval_narrow_by_meet <- Some (get_bool "ana.int.interval_narrow_by_meet");
    conf_values.interval_narrow_by_meet
  
  let get_def_exc_widen_by_join () = 
    if conf_values.def_exc_widen_by_join = None then 
      conf_values.def_exc_widen_by_join <- Some (get_bool "ana.int.def_exc_widen_by_join");
    conf_values.def_exc_widen_by_join
  
  let get_signed_overflow () =
    if conf_values.signed_overflow = None then
      conf_values.signed_overflow <- Some (get_string "sem.int.signed_overflow");
    conf_values.signed_overflow
  
  let get_interval_threshold_widening_constants () =
    if conf_values.interval_threshold_widening_constants = None then
      conf_values.interval_threshold_widening_constants <- Some (get_string "ana.int.interval_threshold_widening_constants");
    conf_values.interval_threshold_widening_constants
  
  let get_refinement () =
    if conf_values.refinement = None then
      conf_values.refinement <- Some (get_string "ana.int.refinement");
    conf_values.refinement

  let get_dereference () =
    if conf_values.dereference = None then
      conf_values.dereference <- Some (get_string "sem.null-pointer.dereference");
    conf_values.dereference

  let get_privglobs () =
    if conf_values.privglobs = None then
      conf_values.privglobs <- Some (get_bool "annotation.int.privglobs");
    conf_values.privglobs

  let get_annotation_int_enabled () =
    if conf_values.annotation_int_enabled = None then
      conf_values.annotation_int_enabled <- Some (get_bool "annotation.int.enabled");
    conf_values.annotation_int_enabled

  let get_goblint_array_domain () =
    if conf_values.goblint_array_domain = None then
      conf_values.goblint_array_domain <- Some (get_bool "annotation.goblint_array_domain");
    conf_values.goblint_array_domain

  let get_deep_query () =
    if conf_values.deep_query = None then
      conf_values.deep_query <- Some (get_bool "ana.base.eval.deep-query");
    conf_values.deep_query

  let get_ana_base_invariant_enabled () =
    if conf_values.ana_base_invariant_enabled = None then
      conf_values.ana_base_invariant_enabled <- Some (get_bool "ana.base.invariant.enabled");
    conf_values.ana_base_invariant_enabled

  let get_exp_earlyglobs () =
    if conf_values.exp_earlyglobs = None then
      conf_values.exp_earlyglobs <- Some (get_bool "exp.earlyglobs");
    conf_values.exp_earlyglobs

  let get_volatiles_are_top () =
    if conf_values.volatiles_are_top = None then
      conf_values.volatiles_are_top <- Some (get_bool "exp.volatiles_are_top");
    conf_values.volatiles_are_top

  let get_globs_are_top () =
    if conf_values.globs_are_top = None then
      conf_values.globs_are_top <- Some (get_bool "exp.globs_are_top");
    conf_values.globs_are_top

  let get_partition_by_const_on_return () =
    if conf_values.partition_by_const_on_return = None then
      conf_values.partition_by_const_on_return <- Some (get_bool "ana.base.partition-arrays.partition-by-const-on-return");
    conf_values.partition_by_const_on_return

  let get_kernel () =
    if conf_values.kernel = None then
      conf_values.kernel <- Some (get_bool "kernel");
    conf_values.kernel
  
  let get_unknown_function_spawn () =
    if conf_values.unknown_function_spawn = None then
      conf_values.unknown_function_spawn <- Some (get_bool "sem.unknown_function.spawn");
    conf_values.unknown_function_spawn

  let get_sem_malloc_fail () =
    if conf_values.sem_malloc_fail = None then
      conf_values.sem_malloc_fail <- Some (get_bool "sem.malloc.fail");
    conf_values.sem_malloc_fail
  
  let get_priv_prec_dump () =
    if conf_values.priv_prec_dump = None then
      conf_values.priv_prec_dump <- Some (get_string "exp.priv-prec-dump");
    conf_values.priv_prec_dump

  let get_ana_base_privatization () =
    if conf_values.ana_base_privatization = None then
      conf_values.ana_base_privatization <- Some (get_string "ana.base.privatization");
    conf_values.ana_base_privatization

  let get_priv_not_started () =
    if conf_values.priv_not_started = None then
      conf_values.priv_not_started <- Some (get_bool "ana.base.priv.not-started");
    conf_values.priv_not_started

  let get_priv_must_joined () =
    if conf_values.priv_must_joined = None then
      conf_values.priv_must_joined <- Some (get_bool "ana.base.priv.must-joined");
    conf_values.priv_must_joined

  let get_priv_distr_init () =
    if conf_values.priv_distr_init = None then
      conf_values.priv_distr_init <- Some (get_bool "exp.priv-distr-init");
    conf_values.priv_distr_init

  let get_print_protection () =
    if conf_values.print_protection = None then
      conf_values.print_protection <- Some (get_bool "dbg.print_protection");
    conf_values.print_protection

  let get_bool_config_value (conf: string): float_precision option =
    match conf with
    | "ana.int.interval_threshold_widening" -> get_interval_threshold_widening ()
    | "ana.int.interval_narrow_by_meet" -> get_interval_narrow_by_meet ()      
    | "ana.int.def_exc_widen_by_join" -> get_def_exc_widen_by_join ()
    | "annotation.int.privglobs" -> get_privglobs ()
    | "annotation.int.enabled" -> get_annotation_int_enabled ()
    | "annotation.goblint_array_domain" -> get_goblint_array_domain ()
    | "ana.base.eval.deep-query" -> get_deep_query ()
    | "ana.base.invariant.enabled" -> get_ana_base_invariant_enabled ()
    | "exp.earlyglobs" -> get_exp_earlyglobs ()
    | "exp.volatiles_are_top" -> get_volatiles_are_top ()
    | "exp.globs_are_top" -> get_globs_are_top ()
    | "ana.base.partition-arrays.partition-by-const-on-return" -> get_partition_by_const_on_return ()
    | "kernel" -> get_kernel ()
    | "sem.unknown_function.spawn" -> get_unknown_function_spawn ()
    | "sem.malloc.fail" -> get_sem_malloc_fail ()
    | "ana.base.priv.not-started" -> get_priv_not_started ()
    | "ana.base.priv.must-joined" -> get_priv_must_joined ()
    | "exp.priv-distr-init" -> get_priv_distr_init ()
    | "dbg.print_protection" -> get_print_protection ()
    | _ -> failwith "Tried to read invalid config value"
  
  let get_string_config_value (conf: string): string option =
    match conf with
    | "sem.int.signed_overflow" -> get_signed_overflow ()
    | "ana.int.interval_threshold_widening_constants" -> get_interval_threshold_widening_constants ()
    | "ana.int.refinement" -> get_refinement ()
    | "sem.null-pointer.dereference" -> get_dereference ()
    | "exp.priv-prec-dump" -> get_priv_prec_dump ()
    | "ana.base.privatization" -> get_ana_base_privatization ()
    | _ -> failwith "Tried to read invalid config value"