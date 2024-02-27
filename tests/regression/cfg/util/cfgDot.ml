open Goblint_lib

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
  Goblint_logs.Logs.Level.current := Info;
  Cilfacade.init ();
  GobConfig.set_bool "witness.invariant.loop-head" true;
  GobConfig.set_bool "witness.invariant.after-lock" true;
  GobConfig.set_bool "witness.invariant.other" true;

  let ast = Cilfacade.getAST (Fpath.v Sys.argv.(1)) in
  GoblintCil.visitCilFileSameGlobals (new allBBVisitor) ast;
  Cilfacade.current_file := ast;
  (* Part of CilCfg.createCFG *)
  GoblintCil.iterGlobals ast (function
      | GFun (fd, _) ->
        GoblintCil.prepareCFG fd;
        GoblintCil.computeCFGInfo fd true
      | _ -> ()
    );
  let ((module Cfg), skipped) = CfgTools.compute_cfg_skips ast in
  let module FileCfg =
  struct
    let file = ast
    module Cfg = Cfg
  end
  in

  let is_loop_head n =
    let prevs = Cfg.prev n in
    List.find_map (fun (edges, prev) ->
        let stmts = CfgTools.CfgEdgeH.find skipped (prev, edges, n) in
        List.find_map (fun s ->
            match s.GoblintCil.skind with
            | Loop (_, loc, _, _, _) -> Some loc
            | _ -> None
          ) stmts
      ) prevs
  in

  let module GraphmlWitnessInvariant = WitnessUtil.Invariant (FileCfg) in
  let module YamlWitnessInvariant = WitnessUtil.YamlInvariant (FileCfg) in
  let module YamlWitnessValidateInvariant = WitnessUtil.YamlInvariantValidate (FileCfg) in

  let module LocationExtraNodeStyles =
  struct
    let defaultNodeStyles = ["align=\"left\""]

    let pp_loc ppf (loc: GoblintCil.location) =
      if loc.line < 0 then
        Format.pp_print_string ppf "unknown"
      else if loc.synthetic then
        Format.fprintf ppf "%a (synthetic)" CilType.Location.pp loc
      else
        CilType.Location.pp ppf loc

    let pp_locs ppf {CilLocation.loc; eloc} =
      Format.fprintf ppf "@[<v 0>%a@;(%a)@]" pp_loc loc pp_loc eloc

    let pp_label_locs ppf label =
      let locs = CilLocation.get_labelLoc label in
      Format.fprintf ppf "@;[%a]" pp_locs locs

    let pp_loop_loc ppf loop =
      Format.fprintf ppf "@;loop: %a" CilType.Location.pp loop

    let extraNodeStyles = function
      | Node.Statement stmt as n ->
        let locs: CilLocation.locs = CilLocation.get_stmtLoc stmt in
        let label =
          Format.asprintf "@[<v 2>%a%a@;YAML loc: %B, loop: %B@;YAMLval loc: %B, loop: %B@;GraphML: %B; server: %B%a@]"
            pp_locs locs
            (Format.pp_print_list ~pp_sep:GobFormat.pp_print_nothing pp_label_locs) stmt.labels
            (YamlWitnessInvariant.is_invariant_node n) (YamlWitnessInvariant.is_loop_head_node n)
            (YamlWitnessValidateInvariant.is_invariant_node n) (YamlWitnessValidateInvariant.is_loop_head_node n)
            (GraphmlWitnessInvariant.is_invariant_node n) (Server.is_server_node n)
            (Format.pp_print_option pp_loop_loc) (is_loop_head n)
        in
        [Printf.sprintf "label=\"%s\"" (Str.global_replace (Str.regexp "\n") "\\n" label)]
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
