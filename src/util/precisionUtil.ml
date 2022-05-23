(* We define precision by the number of IntDomains activated.
 * We currently have 4 types: DefExc, Interval, Enums, Congruence *)
type int_precision = (bool * bool * bool * bool)
(* Same applies for FloatDomain
 * We currently have only an interval type analysis *)
type float_precision = (bool)


(* Thus for maximum precision we activate all Domains *)
let max_int_precision : int_precision = (true, true, true, true)
let max_float_precision : float_precision = (true)
let int_precision_from_fundec (fd: Cil.fundec): int_precision =
  ((ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.def_exc" ~removeAttr:"no-def_exc" ~keepAttr:"def_exc" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.interval" ~removeAttr:"no-interval" ~keepAttr:"interval" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.enums" ~removeAttr:"no-enums" ~keepAttr:"enums" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.congruence" ~removeAttr:"no-congruence" ~keepAttr:"congruence" fd))

let float_precision_from_fundec (fd: Cil.fundec): float_precision =
  ((ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.float.interval" ~removeAttr:"no-float-interval" ~keepAttr:"float-interval" fd))
let int_precision_from_node (): int_precision =
  match !MyCFG.current_node with
  | Some n -> int_precision_from_fundec (Node.find_fundec n)
  | _ -> max_int_precision (* In case a Node is None we have to handle Globals, i.e. we activate all IntDomains (TODO: verify this assumption) *)

let float_precision_from_node (): float_precision =
  match !MyCFG.current_node with
  | Some n -> float_precision_from_fundec (Node.find_fundec n)
  | _ -> max_float_precision

let int_precision_from_node_or_config (): int_precision =
  if GobConfig.get_bool "annotation.int.enabled" then
    int_precision_from_node ()
  else
    let f n = GobConfig.get_bool ("ana.int."^n) in
    (f "def_exc", f "interval", f "enums", f "congruence")

let float_precision_from_node_or_config (): float_precision =
  if GobConfig.get_bool "annotation.float.enabled" then
    float_precision_from_node ()
  else
    let f n = GobConfig.get_bool ("ana.float."^n) in
    (f "interval")
