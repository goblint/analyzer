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

  include C

  let printXml f xs =
    let print_one n v =
      (* Not using Node.location here to have updated locations in incremental analysis.
         See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let loc = UpdateCil.getLoc n in
      BatPrintf.fprintf f "<call id=\"%s\" file=\"%s\" line=\"%d\" order=\"%d\" column=\"%d\" endLine=\"%d\" endColumn=\"%d\" synthetic=\"%B\">\n" (Node.show_id n) loc.file loc.line loc.byte loc.column loc.endLine loc.endColumn loc.synthetic;
      BatPrintf.fprintf f "%a</call>\n" Range.printXml v
    in
    iter print_one xs

  let printJson f xs =
    let print_one n v =
      (* Not using Node.location here to have updated locations in incremental analysis.
         See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let loc = UpdateCil.getLoc n in
      BatPrintf.fprintf f "{\n\"id\": \"%s\", \"file\": \"%s\", \"line\": \"%d\", \"byte\": \"%d\", \"column\": \"%d\", \"states\": %s\n},\n" (Node.show_id n) loc.file loc.line loc.byte loc.column (Yojson.Safe.to_string (Range.to_yojson v))
    in
    iter print_one xs

  let printXmlWarning f () =
    let one_text f Messages.Piece.{loc; text = m; _} =
      match loc with
      | Some loc ->
        let l = Messages.Location.to_cil loc in
        BatPrintf.fprintf f "\n<text file=\"%s\" line=\"%d\" column=\"%d\">%s</text>" l.file l.line l.column (XmlUtil.escape m)
      | None ->
        () (* TODO: not outputting warning without location *)
    in
    let one_w f (m: Messages.Message.t) = match m.multipiece with
      | Single piece  -> one_text f piece
      | Group {group_text = n; pieces = e; group_loc} ->
        let group_loc_text = match group_loc with
          | None -> ""
          | Some group_loc -> GobPretty.sprintf " (%a)" CilType.Location.pretty (Messages.Location.to_cil group_loc)
        in
        BatPrintf.fprintf f "<group name=\"%s%s\">%a</group>\n" n group_loc_text (BatList.print ~first:"" ~last:"" ~sep:"" one_text) e
    in
    let one_w f x = BatPrintf.fprintf f "\n<warning>%a</warning>" one_w x in
    List.iter (one_w f) !Messages.Table.messages_list

  let output table gtable gtfxml (file: file) =
    let out = Messages.get_out result_name !Messages.out in
    match get_string "result" with
    | "pretty" -> ignore (fprintf out "%a\n" pretty (Lazy.force table))
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
