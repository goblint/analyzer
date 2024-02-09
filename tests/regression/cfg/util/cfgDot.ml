(* Part of CilCfg *)
class allBBVisitor = object (* puts every instruction into its own basic block *)
  inherit GoblintCil.nopCilVisitor
  method! vstmt s =
    match s.skind with
    | Instr(il) ->
      let list_of_stmts =
        List.map (fun one_inst -> GoblintCil.mkStmtOneInstr one_inst) il in
      let block = GoblintCil.mkBlock list_of_stmts in
      ChangeDoChildrenPost(s, (fun _ -> s.skind <- Block(block); s))
    | _ -> DoChildren

  method! vvdec _ = SkipChildren
  method! vexpr _ = SkipChildren
  method! vlval _ = SkipChildren
  method! vtype _ = SkipChildren
end

let main () =
  Cilfacade.init ();

  let ast = Cilfacade.getAST (Fpath.v Sys.argv.(1)) in
  GoblintCil.visitCilFileSameGlobals (new allBBVisitor) ast;
  (* Part of CilCfg.createCFG *)
  GoblintCil.iterGlobals ast (function
      | GFun (fd, _) ->
        GoblintCil.prepareCFG fd;
        GoblintCil.computeCFGInfo fd true
      | _ -> ()
    );
  let (module Cfg) = CfgTools.compute_cfg ast in

  let module LocationExtraNodeStyles =
  struct
    let defaultNodeStyles = []
    let extraNodeStyles = function
      | Node.Statement _ as node ->
        let loc = Node.location node in
        [Printf.sprintf "label=\"%s\\n(synthetic: %B)\"" (CilType.Location.show loc) loc.synthetic]
      | _ -> []
  end
  in

  GoblintCil.iterGlobals ast (function
      | GFun (fd, _) ->
        let out = open_out (fd.svar.vname ^ ".dot") in
        let iter_edges = CfgTools.iter_fd_edges (module Cfg) fd in
        let ppf = Format.formatter_of_out_channel out in
        CfgTools.fprint_dot (module CfgTools.CfgPrinters (LocationExtraNodeStyles)) iter_edges ppf;
        Format.pp_print_flush ppf ();
        close_out out
      | _ -> ()
    )

let () = main ()
