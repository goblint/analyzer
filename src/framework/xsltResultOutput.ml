(** XSLT analysis result output. *)

open GoblintCil
open GobConfig
open AnalysisResult

module Make (Result: Result) =
struct
  open Result

  let printXml_node f n v =
    (* Not using Node.location here to have updated locations in incremental analysis.
        See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
    let loc = UpdateCil.getLoc n in
    BatPrintf.fprintf f {xml|<call id="%s" file="%s" line="%d" order="%d" column="%d" endLine="%d" endColumn="%d" synthetic="%B">
%a</call>
|xml}
      (Node.show_id n) loc.file loc.line loc.byte loc.column loc.endLine loc.endColumn loc.synthetic
      Range.printXml v

  let printXml f xs =
    iter (printXml_node f) xs

  let printXmlWarning_one_text f Messages.Piece.{loc; text = m; _} =
    match loc with
    | Some loc ->
      let l = Messages.Location.to_cil loc in
      BatPrintf.fprintf f {xml|
<text file="%s" line="%d" column="%d">%s</text>|xml}
        l.file l.line l.column (XmlUtil.escape m)
    | None ->
      () (* TODO: not outputting warning without location *)

  let printXml_warn f (m: Messages.Message.t) = match m.multipiece with
    | Single piece  -> printXmlWarning_one_text f piece
    | Group {group_text = n; pieces = e; group_loc} ->
      let group_loc_text = match group_loc with
        | None -> ""
        | Some group_loc -> GobPretty.sprintf " (%a)" CilType.Location.pretty (Messages.Location.to_cil group_loc)
      in
      BatPrintf.fprintf f {xml|<group name="%s%s">%a</group>
|xml}
        n group_loc_text (BatList.print ~first:"" ~last:"" ~sep:"" printXmlWarning_one_text) e

  let printXml_warn f x =
    BatPrintf.fprintf f {xml|
<warning>%a</warning>|xml}
      printXml_warn x

  let printXmlWarning f () =
    List.iter (printXml_warn f) !Messages.Table.messages_list

  let do_html_output () =
    let g2html_path = get_string "exp.g2html_path" in
    let g2html_path =
      if g2html_path = "" then
        GobSys.exe_dir
      else
        Fpath.v g2html_path
    in
    let jar = Fpath.(g2html_path / "g2html.jar") in
    if Sys.file_exists (Fpath.to_string jar) then (
      let command = Filename.quote_command "java" [
          "-jar"; Fpath.to_string jar;
          "--num-threads"; string_of_int (jobs ());
          "--dot-timeout"; "0";
          "--result-dir"; "result";
          !Messages.xml_file_name
        ]
      in
      match Timing.wrap "g2html" Unix.system command with
      | Unix.WEXITED 0 -> ()
      | _ -> Logs.error "HTML generation failed! Command: %s" command
      | exception Unix.Unix_error (e, f, a) ->
        Logs.error "%s at syscall %s with argument \"%s\"." (Unix.error_message e) f a
    ) else
      Logs.Format.error "Warning: jar file %a not found." Fpath.pp jar

  let output table live gtable gtfxml (module FileCfg: MyCFG.FileCfg) =
    let file = FileCfg.file in
    let out = Messages.get_out result_name !Messages.out in
    let module SH = BatHashtbl.Make (Basetype.RawStrings) in
    let file2funs = SH.create 100 in
    let funs2node = SH.create 100 in
    iter (fun n _ -> SH.add funs2node (Node.find_fundec n).svar.vname n) (Lazy.force table);
    iterGlobals file (function
        | GFun (fd,loc) -> SH.add file2funs loc.file fd.svar.vname
        | _ -> ()
      );
    let p_node f n = BatPrintf.fprintf f "%s" (Node.show_id n) in
    let p_nodes f xs =
      List.iter (BatPrintf.fprintf f "<node name=\"%a\"/>\n" p_node) xs
    in
    let p_funs f xs =
      let one_fun n =
        BatPrintf.fprintf f "<function name=\"%s\">\n%a</function>\n" n p_nodes (SH.find_all funs2node n)
      in
      List.iter one_fun xs
    in
    let write_file f fn =
      Messages.xml_file_name := fn;
      Logs.info "Writing xml to temp. file: %s" fn;
      BatPrintf.fprintf f "<run>";
      BatPrintf.fprintf f "<parameters>%s</parameters>" GobSys.command_line;
      BatPrintf.fprintf f "<statistics>";
      let timing_ppf = BatFormat.formatter_of_out_channel f in
      Timing.Default.print timing_ppf;
      Format.pp_print_flush timing_ppf ();
      BatPrintf.fprintf f "</statistics>";
      BatPrintf.fprintf f "<result>\n";
      BatEnum.iter (fun b -> BatPrintf.fprintf f "<file name=\"%s\" path=\"%s\">\n%a</file>\n" (Filename.basename b) b p_funs (SH.find_all file2funs b)) (BatEnum.uniq @@ SH.keys file2funs); (* nosemgrep: batenum-module *)
      BatPrintf.fprintf f "%a" printXml (Lazy.force table);
      gtfxml f gtable;
      printXmlWarning f ();
      BatPrintf.fprintf f "</result></run>\n";
      BatPrintf.fprintf f "%!"
    in
    if get_string "result" = "g2html" then (
      BatFile.with_temporary_out ~mode:[`create;`text;`delete_on_exit] write_file;
      CfgTools.dead_code_cfg ~path:(Fpath.v "cfgs") (module FileCfg) live;
      do_html_output ()
    )
    else
      let f = BatIO.output_channel out in
      write_file f (get_string "outfile")
end

module Make2 (Result: Result) =
struct
  open Result
  open Make (Result)

  module SH = GobHashtbl.Make (Basetype.RawStrings)
  module FundecSet = BatSet.Make (CilType.Fundec)
  module IH = BatHashtbl.Make (struct type t = int [@@deriving hash, eq] end)

  let write_index ~result_dir ~file2funs =
    let printXml_fun f fd = BatPrintf.fprintf f {xml|<function name="%s"/>|xml} fd.svar.vname in
    let printXml_file f file funs =
      BatPrintf.fprintf f {xml|<file name="%s">
%a</file>
|xml}
        file (FundecSet.print ~first:"" ~sep:"\n" ~last:"\n" printXml_fun) funs
    in

    let index_file = Fpath.(result_dir / "index.xml") in
    BatFile.with_file_out (Fpath.to_string index_file) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="report.xsl"?>
<report>|xml};
        (* TODO: exclude dead files/functions? *)
        (* g2html has full path in name field *)
        (* for g2html we have path field for <file>, but this doesn't seem necessary *)
        (* for g2html we add <node>s into each <function>, but these aren't necessary because they aren't shown *)
        SH.iter (printXml_file f) file2funs;
        BatPrintf.fprintf f "</report>";
      )

  let write_globals ~result_dir ~gtfxml ~gtable =
    let nodes_dir = Fpath.(result_dir / "nodes") in
    GobSys.mkdir_or_exists nodes_dir;
    let globals_file = Fpath.(nodes_dir / "globals.xml") in
    BatFile.with_file_out (Fpath.to_string globals_file) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../globals.xsl"?>
<globs>%a</globs>|xml} gtfxml gtable
      )

  let write_node ~nodes_dir n v =
    let node_file = Fpath.(nodes_dir / Node.show_id n + "xml") in
    BatFile.with_file_out (Fpath.to_string node_file) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../node.xsl"?>
<loc>%a</loc>|xml} (Fun.flip printXml_node n) v
        (* g2html adds fun attribute to <call> (inside printXml_node), but this doesn't seem to be necessary *)
      )

  let write_nodes ~result_dir ~table =
    let nodes_dir = Fpath.(result_dir / "nodes") in
    GobSys.mkdir_or_exists nodes_dir;
    let file2line2nodes: Node.t IH.t SH.t = SH.create 10 in
    iter (fun n v ->
        write_node ~nodes_dir n v;
        let loc = UpdateCil.getLoc n in (* from printXml_node *)
        let line2nodes = SH.find_or_add_default_delayed file2line2nodes loc.file ~default:(fun () -> IH.create 100) in
        IH.add line2nodes loc.line n
      ) (Lazy.force table);
    file2line2nodes

  let write_warn ~warn_dir i w =
    let warn_file = Fpath.(warn_dir / Printf.sprintf "warn%d.xml" i) in
    BatFile.with_file_out (Fpath.to_string warn_file) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../warn.xsl"?>%a|xml} printXml_warn w
      )

  let write_warns ~result_dir =
    let warn_dir = Fpath.(result_dir / "warn") in
    GobSys.mkdir_or_exists warn_dir;
    let file2line2warns: int IH.t SH.t = SH.create 10 in
    List.iteri (fun i w ->
        write_warn ~warn_dir (i + 1) w;
        let locs =
          match w.multipiece with
          | Single piece -> [piece.loc]
          | Group {pieces; _} -> List.map (fun p -> p.Messages.Piece.loc) pieces (* TODO: add group_loc, old doesn't *)
        in
        List.iter (fun (loc: Messages.Location.t) ->
            let loc = Messages.Location.to_cil loc in
            let line2warns = SH.find_or_add_default_delayed file2line2warns loc.file ~default:(fun () -> IH.create 100) in
            IH.add line2warns loc.line (i + 1)
          ) (List.filter_map Fun.id locs)
      ) !Messages.Table.messages_list;
    file2line2warns

  let empty_line2nodes = IH.create 0
  let empty_line2warns = IH.create 0

  let xmlify_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F")

  let write_file ~files_dir ~file2line2nodes ~file2line2warns ~live ~code_highlighter file =
    let line2nodes = SH.find_default file2line2nodes file empty_line2nodes in
    let line2warns = SH.find_default file2line2warns file empty_line2warns in

    let printXml_node f node = BatPrintf.fprintf f "&quot;%s&quot;" (Node.show_id node) in
    let printXml_warn f w = BatPrintf.fprintf f "&quot;warn%d&quot;" w in
    let printXml_line f line text =
      let nodes = IH.find_all line2nodes (line + 1) |> BatList.unique ~eq:Node.equal in
      let warns = IH.find_all line2warns (line + 1) |> BatList.unique in
      let dead = nodes <> [] && not (List.exists live nodes) in
      BatPrintf.fprintf f {xml|<ln nr="%d" ns="%a" wrn="%a" ded="%B">%s</ln>
|xml}
        (line + 1)
        (BatList.print ~first:"[" ~sep:"," ~last:"]" printXml_node) nodes
        (BatList.print ~first:"[" ~sep:"," ~last:"]" printXml_warn) warns
        dead text
    in

    let file_file = Fpath.(files_dir / xmlify_file_name file + "xml") in
    BatFile.with_file_out (Fpath.to_string file_file) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../file.xsl"?>
<file>
|xml};
        Timing.wrap "highlighter" (fun () ->
            let lines = code_highlighter (Fpath.v file) in
            BatEnum.iteri (printXml_line f) lines (* nosemgrep: batenum-module *)
          ) ();
        BatPrintf.fprintf f "</file>";
      )

  let write_files ~result_dir ~file2funs ~file2line2nodes ~file2line2warns ~live =
    let files_dir = Fpath.(result_dir / "files") in
    GobSys.mkdir_or_exists files_dir;
    let style_css_file = Fpath.(result_dir / "pyg.css") in
    let code_highlighter = CodeHighlighter.make ~style_css_file in
    Seq.iter (write_file ~files_dir ~file2line2nodes ~file2line2warns ~live ~code_highlighter) (SH.to_seq_keys file2funs)

  let write_dot ~dot_dir (module FileCfg: MyCFG.FileCfg) ~live fd file =
    let dot_file_dir = GobSys.mkdir_or_exists_absolute Fpath.(dot_dir / xmlify_file_name file) in
    let dot_file = Fpath.(dot_file_dir / fd.svar.vname + "dot") in
    BatFile.with_file_out (Fpath.to_string dot_file) (fun f ->
        let ppf = BatFormat.formatter_of_output f in
        CfgTools.fprint_fundec_html_dot (module FileCfg.Cfg) live fd ppf;
        Format.pp_print_flush ppf ();
      );
    dot_file

  let cfg_task ~cfgs_dir ~dot_file fd file: ProcessPool.task =
    let cfgs_file_dir = GobSys.mkdir_or_exists_absolute Fpath.(cfgs_dir / xmlify_file_name file) in
    let svg_file = Fpath.(cfgs_file_dir / fd.svar.vname + "svg") in
    {
      command = Filename.quote_command "dot" [Fpath.to_string dot_file; "-Tsvg"; "-o"; Fpath.to_string svg_file];
      cwd = None;
    }

  let write_dots_cfgs ~result_dir (module FileCfg: MyCFG.FileCfg) ~live ~file2funs =
    let dot_dir = GobSys.mkdir_or_exists_absolute Fpath.(result_dir / "dot") in
    let cfgs_dir = GobSys.mkdir_or_exists_absolute Fpath.(result_dir / "cfgs") in
    let tasks = SH.fold (fun file funs acc ->
        FundecSet.fold (fun fd acc ->
            let dot_file = write_dot ~dot_dir (module FileCfg) ~live fd file in
            cfg_task ~cfgs_dir ~dot_file fd file :: acc
          ) funs acc
      ) file2funs []
    in
    Timing.wrap "graphviz" (ProcessPool.run ~jobs:(GobConfig.jobs ())) tasks

  let copy_resources ~result_dir =
    let xslt_dirs = Fpath.(GobSys.exe_dir / "xslt") :: Goblint_sites.xslt in
    let xslt_dir = List.find_opt (fun dir ->
        let dir_str = Fpath.to_string dir in
        Sys.file_exists dir_str && Sys.is_directory dir_str
      ) xslt_dirs
    in
    match xslt_dir with
    | Some xslt_dir ->
      Sys.readdir (Fpath.to_string xslt_dir)
      |> Array.to_seq
      |> Seq.filter ((<>) "dune")
      |> Seq.map (Fpath.add_seg xslt_dir)
      |> Seq.map Fpath.to_string
      |> List.of_seq
      |> Fun.flip FileUtil.cp (Fpath.to_string result_dir)
    | None ->
      failwith "xslt/ not found"

  let output table live gtable gtfxml (module FileCfg: MyCFG.FileCfg) =
    let result_dir = Fpath.(v "result") in
    GobSys.rmdir_recursive_if_exists result_dir;
    GobSys.mkdir_or_exists result_dir;
    let file = FileCfg.file in
    let file2funs = SH.create 100 in
    iterGlobals file (function
        | GFun (fd, loc) -> SH.modify_def FundecSet.empty loc.file (FundecSet.add fd) file2funs
        | _ -> ()
      );
    write_index ~result_dir ~file2funs;
    write_globals ~result_dir ~gtfxml ~gtable;
    let file2line2nodes = write_nodes ~result_dir ~table in
    let file2line2warns = write_warns ~result_dir in
    write_files ~result_dir ~file2funs ~file2line2nodes ~file2line2warns ~live;
    write_dots_cfgs  ~result_dir (module FileCfg) ~live ~file2funs;
    copy_resources ~result_dir
end