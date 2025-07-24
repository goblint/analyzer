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

module XsltResult (Range: Printable.S) (C: ResultConf) =
struct
  include BatHashtbl.Make (ResultNode)
  type nonrec t = Range.t t (* specialize polymorphic type for Range values *)

  include C

  let printXml_print_one f n v =
    (* Not using Node.location here to have updated locations in incremental analysis.
        See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
    let loc = UpdateCil.getLoc n in
    BatPrintf.fprintf f "<call id=\"%s\" file=\"%s\" line=\"%d\" order=\"%d\" column=\"%d\" endLine=\"%d\" endColumn=\"%d\" synthetic=\"%B\">\n" (Node.show_id n) loc.file loc.line loc.byte loc.column loc.endLine loc.endColumn loc.synthetic;
    BatPrintf.fprintf f "%a</call>\n" Range.printXml v

  let printXml f xs =
    iter (printXml_print_one f) xs

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
    | s -> failwith @@ "Unsupported value for option `result`: "^s
end

module XsltResult2 (Range: Printable.S) (C: ResultConf) =
struct
  include XsltResult (Range) (C)

  module SH = BatHashtbl.Make (Basetype.RawStrings)
  module FundecSet = Set.Make (CilType.Fundec)
  module IH = BatHashtbl.Make (struct type t = int [@@deriving hash, eq] end)

  let write_index ~file2funs =
    let p_funs f xs =
      let one_fun n =
        BatPrintf.fprintf f "<function name=\"%s\"/>\n" n.svar.vname
      in
      FundecSet.iter one_fun xs
    in
    BatFile.with_file_out "result2/index.xml" (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="report.xsl"?>
<report>|xml};
        (* TODO: exclude <file> path? *)
        (* TODO: exclude <node>s? *)
        (* TODO: exclude dead files/functions? *)
        (* BatEnum.iter (fun b -> BatPrintf.fprintf f "<file name=\"%s\" path=\"%s\">\n%a</file>\n" (Filename.basename b) b p_funs (SH.find_all file2funs b)) (BatEnum.uniq @@ SH.keys file2funs); *)
        (* g2html has full path in name field *)
        SH.iter (fun b funs -> BatPrintf.fprintf f "<file name=\"%s\">\n%a</file>\n" b p_funs funs) file2funs;
        BatPrintf.fprintf f "</report>";
      )

  let write_globals ~gtfxml ~gtable =
    BatFile.with_file_out "result2/nodes/globals.xml" (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../globals.xsl"?>
<globs>|xml};
        gtfxml f gtable;
        BatPrintf.fprintf f "</globs>";
      )

  let write_node n v =
    BatFile.with_file_out (Printf.sprintf "result2/nodes/%s.xml" (Node.show_id n)) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../node.xsl"?>
<loc>|xml};
        (* TODO: need fun in <call>? *)
        printXml_print_one f n v;
        BatPrintf.fprintf f "</loc>";
      )

  let write_nodes ~table =
    let file2line2nodes: Node.t IH.t SH.t = SH.create 10 in
    iter (fun n v ->
        write_node n v;
        let loc = UpdateCil.getLoc n in (* from printXml_print_one *)
        let line2nodes: Node.t IH.t =
          match SH.find_option file2line2nodes loc.file with
          | Some line2nodes -> line2nodes
          | None ->
            let line2nodes = IH.create 100 in
            SH.replace file2line2nodes loc.file line2nodes;
            line2nodes
        in
        IH.add line2nodes loc.line n
      ) (Lazy.force table);
    file2line2nodes

  let write_warn i w =
    BatFile.with_file_out (Printf.sprintf "result2/warn/warn%d.xml" (i + 1)) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../warn.xsl"?>|xml};
        printXmlWarning_one_w f w;
      )

  let write_file ~file2line2nodes ~file2line2warns ~live b =
    let c_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F") b in
    BatFile.with_file_out (Printf.sprintf "result2/files/%s.xml" c_file_name) (fun f ->
        BatPrintf.fprintf f {xml|<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="../file.xsl"?>
<file>
|xml};
        let ic = BatUnix.open_process_args_in "pygmentize" [|"pygmentize"; "-f"; "html"; "-O"; "nowrap,classprefix=pyg-"; b|] in (* TODO: close *)
        let ic' = BatIO.input_channel ic in
        let lines = BatIO.lines_of ic' in
        BatEnum.iteri (fun line text ->
            let nodes =
              match SH.find_option file2line2nodes b with
              | Some line2nodes -> IH.find_all line2nodes (line + 1) |> BatList.unique ~eq:Node.equal
              | None -> []
            in
            let print_node f w =
              BatPrintf.fprintf f "&quot;%s&quot;" (Node.show_id w)
            in
            let warns =
              match SH.find_option file2line2warns b with
              | Some line2warns -> IH.find_all line2warns (line + 1) |> BatList.unique
              | None -> []
            in
            let print_warn f w =
              BatPrintf.fprintf f "&quot;warn%d&quot;" w
            in
            let dead = nodes <> [] && not (List.exists live nodes) in
            BatPrintf.fprintf f {xml|<ln nr="%d" ns="[%a]" wrn="[%a]" ded="%B">%s</ln>
|xml} (line + 1) (BatList.print ~first:"" ~sep:"," ~last:"" print_node) nodes (BatList.print ~first:"" ~sep:"," ~last:"" print_warn) warns dead text
          ) lines;
        BatPrintf.fprintf f "</file>";
      )

  let write_dot (module FileCfg: MyCFG.FileCfg) ~live fd file =
    let base_dir = GobSys.mkdir_or_exists_absolute Fpath.(v "result2" / "dot") in (* TODO: move out *)
    let c_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F") file in
    let dot_file_name = fd.svar.vname^".dot" in
    let file_dir = GobSys.mkdir_or_exists_absolute Fpath.(base_dir / c_file_name) in
    let fname = Fpath.(file_dir / dot_file_name) in
    let out = open_out (Fpath.to_string fname) in
    let ppf = Format.formatter_of_out_channel out in
    CfgTools.fprint_fundec_html_dot (module FileCfg.Cfg) live fd ppf;
    Format.pp_print_flush ppf ();
    close_out out;
    fname

  let cfg_task fname fd file: ProcessPool.task =
    let base_dir2 = GobSys.mkdir_or_exists_absolute Fpath.(v "result2" / "cfgs") in
    let c_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F") file in
    let svg_file_name = fd.svar.vname^".svg" in
    let file_dir2 = GobSys.mkdir_or_exists_absolute Fpath.(base_dir2 / c_file_name) in
    let fname2 = Fpath.(file_dir2 / svg_file_name) in
    {
      command = Filename.quote_command "dot" [Fpath.to_string fname; "-Tsvg"; "-o"; Fpath.to_string fname2];
      cwd = None;
    }

  let copy_resources () =
    (* TODO: vendor resources *)
    Sys.readdir "g2html/resources"
    |> Array.to_list
    |> List.map (fun f -> "g2html/resources/" ^ f)
    |> (fun fs -> FileUtil.cp fs "result2")

  let output table live gtable gtfxml (module FileCfg: MyCFG.FileCfg) =
    let file = FileCfg.file in
    match get_string "result" with
    | "fast_xml" ->
      output table live gtable gtfxml (module FileCfg)
    | "g2html" ->
      let file2funs = SH.create 100 in
      iterGlobals file (function
          | GFun (fd, loc) -> SH.modify_def FundecSet.empty loc.file (FundecSet.add fd) file2funs
          | _ -> ()
        );
      GobSys.mkdir_or_exists Fpath.(v "result2");
      GobSys.mkdir_or_exists Fpath.(v "result2" / "nodes");
      GobSys.mkdir_or_exists Fpath.(v "result2" / "warn");
      GobSys.mkdir_or_exists Fpath.(v "result2" / "files");
      write_index ~file2funs;
      write_globals ~gtfxml ~gtable;
      let file2line2nodes: Node.t IH.t SH.t = write_nodes ~table in
      let file2line2warns: int IH.t SH.t = SH.create 10 in
      List.iteri (fun i w ->
          write_warn i w;
          let locs =
            match w.multipiece with
            | Single piece -> [piece.loc]
            | Group {pieces; _} -> List.map (fun p -> p.Messages.Piece.loc) pieces (* TODO: add group_loc, old doesn't *)
          in
          List.iter (fun (loc: Messages.Location.t) ->
              let loc = Messages.Location.to_cil loc in
              let line2warns: int IH.t =
                match SH.find_option file2line2warns loc.file with
                | Some line2warns -> line2warns
                | None ->
                  let line2warns = IH.create 100 in
                  SH.replace file2line2warns loc.file line2warns;
                  line2warns
              in
              IH.add line2warns loc.line (i + 1)
            ) (List.filter_map Fun.id locs)
        ) !Messages.Table.messages_list;
      let asd = BatSys.command {a|pygmentize -S default -f html -O nowrap,classprefix=pyg- > result2/pyg.css|a} in
      assert (asd = 0);
      BatEnum.iter (write_file ~file2line2nodes ~file2line2warns ~live) (SH.keys file2funs);
      let tasks = SH.fold (fun file funs acc ->
          FundecSet.fold (fun fd acc ->
              let fname = write_dot (module FileCfg) ~live fd file in
              cfg_task fname fd file :: acc
            ) funs acc
        ) file2funs []
      in
      Timing.wrap "graphviz" (ProcessPool.run ~jobs:(GobConfig.jobs ())) tasks;
      copy_resources ();
      assert false
    | s -> failwith @@ "Unsupported value for option `result`: "^s
end