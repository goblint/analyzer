(** Integer and floating-point option and attribute handling. *)

(* We define precision by the number of IntDomains activated.
 * We currently have 5 types: DefExc, Interval, Enums, Congruence, IntervalSet *)
type int_precision = (bool * bool * bool * bool * bool)
(* Same applies for FloatDomain
 * We currently have only an interval type analysis *)
type float_precision = (bool)

let def_exc: bool option ref = ref None
let interval: bool option ref = ref None
let enums: bool option ref = ref None
let congruence: bool option ref = ref None
let interval_set: bool option ref = ref None

let get_def_exc () =
  if !def_exc = None then
    def_exc := Some (GobConfig.get_bool "ana.int.def_exc");
  Option.get !def_exc

let get_interval () =
  if !interval = None then
    interval := Some (GobConfig.get_bool "ana.int.interval");
  Option.get !interval

let get_enums () =
  if !enums = None then
    enums := Some (GobConfig.get_bool "ana.int.enums");
  Option.get !enums

let get_congruence () =
  if !congruence = None then
    congruence := Some (GobConfig.get_bool "ana.int.congruence");
  Option.get !congruence

let get_interval_set () =
  if !interval_set = None then
    interval_set := Some (GobConfig.get_bool "ana.int.interval_set");
  Option.get !interval_set

let annotation_int_enabled: bool option ref = ref None

let get_annotation_int_enabled () =
  if !annotation_int_enabled = None then
    annotation_int_enabled := Some (GobConfig.get_bool "annotation.int.enabled");
  Option.get !annotation_int_enabled

let reset_lazy () =
  def_exc := None;
  interval := None;
  enums := None;
  congruence := None;
  interval_set := None;
  annotation_int_enabled := None

(* Thus for maximum precision we activate all Domains *)
let max_int_precision : int_precision = (true, true, true, true, true)
let max_float_precision : float_precision = (true)
let int_precision_from_fundec (fd: GoblintCil.fundec): int_precision =
  ((ContextUtil.should_keep_int_domain ~isAttr:GobPrecision ~keepOption:(get_def_exc ()) ~removeAttr:"no-def_exc" ~keepAttr:"def_exc" fd),
   (ContextUtil.should_keep_int_domain ~isAttr:GobPrecision ~keepOption:(get_interval ()) ~removeAttr:"no-interval" ~keepAttr:"interval" fd),
   (ContextUtil.should_keep_int_domain ~isAttr:GobPrecision ~keepOption:(get_enums ()) ~removeAttr:"no-enums" ~keepAttr:"enums" fd),
   (ContextUtil.should_keep_int_domain ~isAttr:GobPrecision ~keepOption:(get_congruence ()) ~removeAttr:"no-congruence" ~keepAttr:"congruence" fd),
   (ContextUtil.should_keep_int_domain ~isAttr:GobPrecision ~keepOption:(get_interval_set ()) ~removeAttr:"no-interval_set" ~keepAttr:"interval_set" fd))

let float_precision_from_fundec (fd: GoblintCil.fundec): float_precision =
  ((ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.float.interval" ~removeAttr:"no-float-interval" ~keepAttr:"float-interval" fd))
let int_precision_from_node (): int_precision =
  match !MyCFG.current_node with
  | Some n -> int_precision_from_fundec (Node.find_fundec n)
  | _ -> max_int_precision (* In case a Node is None we have to handle Globals, i.e. we activate all IntDomains (TODO: verify this assumption) *)

let is_congruence_active (_, _, _, c,_: int_precision): bool = c

let float_precision_from_node (): float_precision =
  match !MyCFG.current_node with
  | Some n -> float_precision_from_fundec (Node.find_fundec n)
  | _ -> max_float_precision

let int_precision_from_node_or_config (): int_precision =
  if get_annotation_int_enabled () then
    int_precision_from_node ()
  else
    (get_def_exc (), get_interval (), get_enums (), get_congruence (), get_interval_set ())

let float_precision_from_node_or_config (): float_precision =
  if GobConfig.get_bool "annotation.float.enabled" then
    float_precision_from_node ()
  else
    let f n = GobConfig.get_bool ("ana.float."^n) in
    (f "interval")
