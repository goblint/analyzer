(** Analysis result output. *)

open GoblintCil
open Pretty
open GobConfig
open AnalysisResult

module Make (Result: Result) =
struct
  open Result

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


  let printJson f xs =
    let print_one n v =
      (* Not using Node.location here to have updated locations in incremental analysis.
         See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let loc = UpdateCil.getLoc n in
      BatPrintf.fprintf f "{\n\"id\": \"%s\", \"file\": \"%s\", \"line\": \"%d\", \"byte\": \"%d\", \"column\": \"%d\", \"states\": %s\n},\n" (Node.show_id n) loc.file loc.line loc.byte loc.column (Yojson.Safe.to_string (Range.to_yojson v))
    in
    iter print_one xs


  let output table live gtable gtfxml (module FileCfg: MyCFG.FileCfg) =
    let file = FileCfg.file in
    let out = Messages.get_out result_name !Messages.out in
    match get_string "result" with
    | "pretty" -> ignore (fprintf out "%a\n" pretty (Lazy.force table))
    | "pretty-deterministic" -> ignore (fprintf out "%a\n" pretty_deterministic (Lazy.force table))
    | "fast_xml"
    | "g2html" ->
      let module Output = XsltResultOutput.Make (Result) in
      Output.output table live gtable gtfxml (module FileCfg)
    | "xslt" ->
      let module Output = XsltResultOutput.Make2 (Result) in
      Output.output table live gtable gtfxml (module FileCfg)
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
      let p_enum p f xs = BatEnum.print ~first:"[\n  " ~last:"\n]" ~sep:",\n  " p f xs in (* nosemgrep: batenum-module *)
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
    | "dashboard" ->
      let timings = Timing.Default.root_with_current () in
      let json = `Assoc [
          ("files", Preprocessor.dependencies_to_yojson ());
          ("time", `Float (if get_bool "dbg.timing.enabled" then timings.cputime else -1.));
          ("checks", Checks.export ());
        ] in
      Yojson.Safe.to_channel ~std:true out json
    | "none" -> ()
    | s -> failwith @@ "Unsupported value for option `result`: "^s
end
