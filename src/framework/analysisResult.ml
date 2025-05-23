(** Analysis result output. *)

open GoblintCil
open Pretty
open GobConfig

module ResultNode: Printable.S with type t = MyCFG.node =
struct
  include Printable.Std

  include Node

  let name () = "resultnode"

  let show a =
    (* Not using Node.location here to have updated locations in incremental analysis.
       See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
    let x = UpdateCil.getLoc a in
    let f = Node.find_fundec a in
    CilType.Location.show x ^ "(" ^ f.svar.vname ^ ")"

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module type ResultConf =
sig
  val result_name: string
end

module Result (Range: Printable.S) (C: ResultConf) =
struct
  include BatHashtbl.Make (ResultNode)
  type nonrec t = Range.t t (* specialize polymorphic type for Range values *)

  let pretty () mapping =
    let f key st dok =
      dok ++ dprintf "%a ->@?  @[%a@]\n" ResultNode.pretty key Range.pretty st
    in
    let content () = fold f mapping nil in
    let defline () = dprintf "OTHERS -> Not available\n" in
    dprintf "@[Mapping {\n  @[%t%t@]}@]" content defline

  let pretty_deterministic () mapping =
    let bindings =
      to_list mapping
      |> List.sort [%ord: ResultNode.t * Range.t]
    in
    let f dok (key, st) =
      dok ++ dprintf "%a ->@?  @[%a@]\n" ResultNode.pretty key Range.pretty st
    in
    let content () = List.fold_left f nil bindings in
    let defline () = dprintf "OTHERS -> Not available\n" in
    dprintf "@[Mapping {\n  @[%t%t@]}@]" content defline

  include C


  let printXml_print_one f n v =
    (* Not using Node.location here to have updated locations in incremental analysis.
        See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
    let loc = UpdateCil.getLoc n in
    BatPrintf.fprintf f "<call id=\"%s\" file=\"%s\" line=\"%d\" order=\"%d\" column=\"%d\" endLine=\"%d\" endColumn=\"%d\" synthetic=\"%B\">\n" (Node.show_id n) loc.file loc.line loc.byte loc.column loc.endLine loc.endColumn loc.synthetic;
    BatPrintf.fprintf f "%a</call>\n" Range.printXml v

  let printXml f xs =
    iter (printXml_print_one f) xs

  let printJson f xs =
    let print_one n v =
      (* Not using Node.location here to have updated locations in incremental analysis.
         See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let loc = UpdateCil.getLoc n in
      BatPrintf.fprintf f "{\n\"id\": \"%s\", \"file\": \"%s\", \"line\": \"%d\", \"byte\": \"%d\", \"column\": \"%d\", \"states\": %s\n},\n" (Node.show_id n) loc.file loc.line loc.byte loc.column (Yojson.Safe.to_string (Range.to_yojson v))
    in
    iter print_one xs

  let printXmlWarning_one_text f Messages.Piece.{loc; text = m; _} =
    match loc with
    | Some loc ->
      let l = Messages.Location.to_cil loc in
      BatPrintf.fprintf f "\n<text file=\"%s\" line=\"%d\" column=\"%d\">%s</text>" l.file l.line l.column (XmlUtil.escape m)
    | None ->
      () (* TODO: not outputting warning without location *)

  let printXmlWarning_one_w f (m: Messages.Message.t) = match m.multipiece with
    | Single piece  -> printXmlWarning_one_text f piece
    | Group {group_text = n; pieces = e; group_loc} ->
      let group_loc_text = match group_loc with
        | None -> ""
        | Some group_loc -> GobPretty.sprintf " (%a)" CilType.Location.pretty (Messages.Location.to_cil group_loc)
      in
      BatPrintf.fprintf f "<group name=\"%s%s\">%a</group>\n" n group_loc_text (BatList.print ~first:"" ~last:"" ~sep:"" printXmlWarning_one_text) e

  let printXmlWarning_one_w f x = BatPrintf.fprintf f "\n<warning>%a</warning>" printXmlWarning_one_w x

  let printXmlWarning f () =
    List.iter (printXmlWarning_one_w f) !Messages.Table.messages_list

  let output table live gtable gtfxml (module FileCfg: MyCFG.FileCfg) =
    let file = FileCfg.file in
    let out = Messages.get_out result_name !Messages.out in
    match get_string "result" with
    | "pretty" -> ignore (fprintf out "%a\n" pretty (Lazy.force table))
    | "pretty-deterministic" -> ignore (fprintf out "%a\n" pretty_deterministic (Lazy.force table))
    | "fast_xml" ->
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
        BatEnum.iter (fun b -> BatPrintf.fprintf f "<file name=\"%s\" path=\"%s\">\n%a</file>\n" (Filename.basename b) b p_funs (SH.find_all file2funs b)) (BatEnum.uniq @@ SH.keys file2funs);
        BatPrintf.fprintf f "%a" printXml (Lazy.force table);
        gtfxml f gtable;
        printXmlWarning f ();
        BatPrintf.fprintf f "</result></run>\n";
        BatPrintf.fprintf f "%!"
      in
      if get_bool "g2html" then
        BatFile.with_temporary_out ~mode:[`create;`text;`delete_on_exit] write_file
      else
        let f = BatIO.output_channel out in
        write_file f (get_string "outfile")
    | "g2html" ->
      (* copied from above *)
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
      GobSys.mkdir_or_exists Fpath.(v "result2");
      GobSys.mkdir_or_exists Fpath.(v "result2" / "nodes");
      GobSys.mkdir_or_exists Fpath.(v "result2" / "warn");
      GobSys.mkdir_or_exists Fpath.(v "result2" / "files");
      BatFile.with_file_out "result2/index.xml" (fun f ->
          BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="report.xsl"?>
<report>|xml};
          (* TODO: exclude <file> path? *)
          (* TODO: exclude <node>s? *)
          (* TODO: exclude dead files/functions? *)
          BatEnum.iter (fun b -> BatPrintf.fprintf f "<file name=\"%s\" path=\"%s\">\n%a</file>\n" (Filename.basename b) b p_funs (SH.find_all file2funs b)) (BatEnum.uniq @@ SH.keys file2funs);
          BatPrintf.fprintf f "</report>";
        );
      BatFile.with_file_out "result2/nodes/globals.xml" (fun f ->
          BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../globals.xsl"?>
<globs>|xml};
          gtfxml f gtable;
          BatPrintf.fprintf f "</globs>";
        );
      iter (fun n v ->
          BatFile.with_file_out (Printf.sprintf "result2/nodes/%s.xml" (Node.show_id n)) (fun f ->
              BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../node.xsl"?>
<loc>|xml};
              (* TODO: need fun in <call>? *)
              printXml_print_one f n v;
              BatPrintf.fprintf f "</loc>";
            )
        ) (Lazy.force table);
      List.iteri (fun i w ->
          BatFile.with_file_out (Printf.sprintf "result2/warn/warn%d.xml" (i + 1)) (fun f ->
              BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../warn.xsl"?>|xml};
              printXmlWarning_one_w f w;
            )
        ) !Messages.Table.messages_list;
      BatEnum.iter (fun b ->
          let c_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F") b in
          BatFile.with_file_out (Printf.sprintf "result2/files/%s.xml" c_file_name) (fun f ->
              BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../file.xsl"?>
<file>
|xml};
              let lines = BatFile.lines_of b in
              BatEnum.iteri (fun line text ->
                  BatPrintf.fprintf f {xml|<ln nr="%d" ns="[]" wrn="[]" ded="false">%s</ln>
|xml} (line + 1) (XmlUtil.escape text)
                ) lines;
              BatPrintf.fprintf f "</file>";
            )
        ) (BatEnum.uniq @@ SH.keys file2funs);
      (* CfgTools.dead_code_cfg ~path:Fpath.(v "result2" / "dot") (module FileCfg) live; *)
      (* TODO: copied and modified... *)
      let tasks = foldGlobals FileCfg.file (fun acc glob ->
          match glob with
          | GFun (fd,loc) ->
            (* ignore (Printf.printf "fun: %s\n" fd.svar.vname); *)
            let base_dir = GobSys.mkdir_or_exists_absolute Fpath.(v "result2" / "dot") in
            let base_dir2 = GobSys.mkdir_or_exists_absolute Fpath.(v "result2" / "cfgs") in
            let c_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F") loc.file in
            let dot_file_name = fd.svar.vname^".dot" in
            let svg_file_name = fd.svar.vname^".svg" in
            let file_dir = GobSys.mkdir_or_exists_absolute Fpath.(base_dir / c_file_name) in
            let file_dir2 = GobSys.mkdir_or_exists_absolute Fpath.(base_dir2 / c_file_name) in
            let fname = Fpath.(file_dir / dot_file_name) in
            let fname2 = Fpath.(file_dir2 / svg_file_name) in
            let out = open_out (Fpath.to_string fname) in
            let ppf = Format.formatter_of_out_channel out in
            CfgTools.fprint_fundec_html_dot (module FileCfg.Cfg) live fd ppf;
            Format.pp_print_flush ppf ();
            close_out out;
            let task: ProcessPool.task = {
              command = Filename.quote_command "dot" [Fpath.to_string fname; "-Tsvg"; "-o"; Fpath.to_string fname2];
              cwd = None;
            } in
            task :: acc
          | _ -> acc
        ) []
      in
      Timing.wrap "graphviz" (ProcessPool.run ~jobs:(GobConfig.jobs ())) tasks;
      assert false
    | "json" ->
      let open BatPrintf in
      let module SH = BatHashtbl.Make (Basetype.RawStrings) in
      let file2funs = SH.create 100 in
      let funs2node = SH.create 100 in
      iter (fun n _ -> SH.add funs2node (Node.find_fundec n).svar.vname n) (Lazy.force table);
      iterGlobals file (function
          | GFun (fd,loc) -> SH.add file2funs loc.file fd.svar.vname
          | _ -> ()
        );
      let p_enum p f xs = BatEnum.print ~first:"[\n  " ~last:"\n]" ~sep:",\n  " p f xs in
      let p_list p f xs = BatList.print ~first:"[\n  " ~last:"\n]" ~sep:",\n  " p f xs in
      (*let p_kv f (k,p,v) = fprintf f "\"%s\": %a" k p v in*)
      (*let p_obj f xs = BatList.print ~first:"{\n  " ~last:"\n}" ~sep:",\n  " p_kv xs in*)
      let p_node f n = BatPrintf.fprintf f "\"%s\"" (Node.show_id n) in
      let p_fun f x = fprintf f "{\n  \"name\": \"%s\",\n  \"nodes\": %a\n}" x (p_list p_node) (SH.find_all funs2node x) in
      (*let p_fun f x = p_obj f [ "name", BatString.print, x; "nodes", p_list p_node, SH.find_all funs2node x ] in*)
      let p_file f x = fprintf f "{\n  \"name\": \"%s\",\n  \"path\": \"%s\",\n  \"functions\": %a\n}" (Filename.basename x) x (p_list p_fun) (SH.find_all file2funs x) in
      let write_file f fn =
        Logs.info "Writing json to temp. file: %s" fn;
        fprintf f "{\n  \"parameters\": \"%s\",\n  " GobSys.command_line;
        fprintf f "\"files\": %a,\n  " (p_enum p_file) (SH.keys file2funs);
        fprintf f "\"results\": [\n  %a\n]\n" printJson (Lazy.force table);
        (*gtfxml f gtable;*)
        (*printXmlWarning f ();*)
        fprintf f "}\n";
      in
      if get_bool "g2html" then
        BatFile.with_temporary_out ~mode:[`create;`text;`delete_on_exit] write_file
      else
        let f = BatIO.output_channel out in
        write_file f (get_string "outfile")
    | "sarif" ->
      Logs.result "Writing Sarif to file: %s" (get_string "outfile");
      Yojson.Safe.to_channel ~std:true out (Sarif.to_yojson (List.rev !Messages.Table.messages_list));
    | "json-messages" ->
      let json = `Assoc [
          ("files", Preprocessor.dependencies_to_yojson ());
          ("messages", Messages.Table.to_yojson ());
        ]
      in
      Yojson.Safe.to_channel ~std:true out json
    | "none" -> ()
    | s -> failwith @@ "Unsupported value for option `result`: "^s
end

module ResultType2 (S: Analyses.Spec) =
struct
  open S
  include Printable.Prod3 (C) (D) (CilType.Fundec)
  let show (es,x,f:t) = D.show x
  let pretty () (_,x,_) = D.pretty () x
  let printXml f (c,d,fd) =
    BatPrintf.fprintf f "<context>\n%a</context>\n%a" C.printXml c D.printXml d
end
