(* We define precision by the number of IntDomains activated.
 * We currently have 4 types: DefExc, Interval, Enums, Congruence *)
type precision = (bool * bool * bool * bool)


(* Thus for maximum precision we activate all IntDomains *)
let max_precision : precision = (true, true, true, true)
let precision_from_fundec (fd: Cil.fundec): precision =
  ((ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.def_exc" ~removeAttr:"no-def_exc" ~keepAttr:"def_exc" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.interval" ~removeAttr:"no-interval" ~keepAttr:"interval" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.enums" ~removeAttr:"no-enums" ~keepAttr:"enums" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.congruence" ~removeAttr:"no-congruence" ~keepAttr:"congruence" fd))
let precision_from_node (): precision =
  match !MyCFG.current_node with
  | Some n -> precision_from_fundec (Node.find_fundec n)
  | _ -> max_precision (* In case a Node is None we have to handle Globals, i.e. we activate all IntDomains (TODO: verify this assumption) *)

let precision_from_node_or_config (): precision =
  if GobConfig.get_bool "annotation.int.enabled" then
    precision_from_node ()
  else
    let f n = GobConfig.get_bool ("ana.int."^n) in
    (f "def_exc", f "interval", f "enums", f "congruence")
