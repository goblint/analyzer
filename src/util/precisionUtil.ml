(* We define precision by the number of IntDomains activated.
 * We currently have 4 types: DefExc, Interval, Enums, Congruence *)
type precision = (bool * bool * bool * bool)


(* Thus for maximum precision we activate all IntDomains *)
let max_enabled_precision (): precision = (true, true, true, true)
let precision_from_fundec (fd: Cil.fundec): precision =
  ((ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.def_exc" ~removeAttr:"no-def_exc" ~keepAttr:"def_exc" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.interval" ~removeAttr:"no-interval" ~keepAttr:"interval" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.enums" ~removeAttr:"no-enums" ~keepAttr:"enums" fd),
   (ContextUtil.should_keep ~isAttr:GobPrecision ~keepOption:"ana.int.congruence" ~removeAttr:"no-congruence" ~keepAttr:"congruence" fd))
let precision_from_node (): precision =
  let node = !MyCFG.current_node in
  if Option.is_some node then
    let fd = Node.find_fundec (Option.get node) in
    precision_from_fundec fd
  else
    max_enabled_precision () (* In case a Node is None we have to handle Globals, i.e. we activate all IntDomains (TODO: varify this assumption) *)
