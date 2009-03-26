(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(** This is the main program! *)

module CF = Cilfacade
module GU = Goblintutil
module M = Messages

let main () =
  let usage_str = "Usage: goblint [options] source-files" in
  let fileNames : string list ref = ref [] in
  (* default settings for the command line arguments: *)
  let include_dir = ref (Filename.concat (Filename.dirname Sys.executable_name) "includes") in 
  let kernel_dir = "/lib/modules/`uname -r`/build/include" in
  let other_includes = ref "" in
  let add_include x = other_includes := "-I " ^ x ^ " " ^ !other_includes in
  let use_libc = ref false in
  let justCil = ref false in
  let dopartial = ref false in
  let keep_cpp = ref false in
  let cppflags = ref "" in
  let outFile = ref "" in 
  let cilout = ref stderr in
  (* Function for setting the style, basically Haskell's read function: *)
  let setstyle x = 
    GU.result_style := match x with
      | "none" -> GU.None
      | "state" -> GU.State
      | "indented" -> GU.Indented
      | "compact" -> GU.Compact
      | "pretty" -> GU.Pretty
      | _ -> raise (Arg.Bad "invalid result style") 
  in
  let setdump path = GU.dump_path := Some (GU.create_dir path) in
  let add_exitfun f = GU.exitfun := f :: !GU.exitfun in
  let setcil path = cilout := open_out path in
  let analyze = ref Mutex.Analysis.analyze in
  let nonstatic () = GU.allfuns := true; GU.nonstatic := true in
  let setanalysis str = 
    analyze := match str with
      | "mutex" -> Mutex.Analysis.analyze
      | "no_path" -> Mutex.SimpleAnalysis.analyze
      | "base" -> Base.Analysis.analyze
      | _ -> raise (Arg.Bad "no such analysis")
  in
  let set_trace sys = 
    if M.tracing then Trace.traceAddMulti sys
    else (prerr_endline "Goblin has been compiled without tracing, run ./trace_on to recompile."; exit 2)
  in
  let speclist = [
                 ("-o", Arg.Set_string outFile, "<file>  Prints the output to file.");
                 ("-v", Arg.Set GU.verbose, " Prints some status information.");
                 ("-I", Arg.String add_include,  " Add include directory.");
                 ("--includes", Arg.Set_string include_dir, " Uses custom include files.");
                 ("--libc", Arg.Set use_libc, " Merge with a custom implementation of standard libs.");
                 ("--justcil", Arg.Set justCil, " Just print the transformated CIL output.");
                 ("--dopartial", Arg.Set dopartial, " Apply CIL's constant folding and partial evaluation.");
                 ("--cfg", Arg.Set GU.cfg_print, " prints the cfg into cfg.dot.");
                 ("--debug", Arg.Set GU.debug, " Debug mode: for testing the anlyzer itself.");
                 ("--trace", Arg.String set_trace, "<sys>  subsystem to show debug printfs for: con, sol.");
                 ("--stats", Arg.Set Cilutil.printStats, " Outputs timing information.");
                 ("--eclipse", Arg.Set GU.eclipse, " Flag for Goblin's Eclipse Plugin.");
                 ("--allfuns", Arg.Set GU.allfuns, " Analyzes all the functions (not just beginning from main).");
                 ("--nonstatic", Arg.Unit nonstatic, " Analyzes all non-static functions.");
                 ("--mainfun", Arg.Set_string GU.mainfun, " Sets the name of the main function.");
                 ("--exitfun", Arg.String add_exitfun, " Sets the name of the main function.");
                 ("--allglobs", Arg.Set GU.allglobs, " Prints access information about all globals, not just races.");
                 ("--earlyglobs", Arg.Set GU.earlyglobs, " Side-effecting of globals right after initialization.");
                 ("--write-races", Arg.Set GU.no_read, " Ignores read accesses altogether in reporting races.");
                 ("--unmerged-fields", Arg.Set GU.unmerged_fields, " Does not merge accesses to possibly same fields, unsound.");
                 ("--die-on-collapse", Arg.Set GU.die_on_collapse, " Raise an exception as soon as an array collapses.");
                 ("--keepcpp", Arg.Set keep_cpp, " Keep the intermediate output of running the C preprocessor.");
                 ("--cppflags", Arg.Set_string cppflags, "<flags>  Pre-processing parameters.");
                 ("--kernel", Arg.Set GU.kernel, "For analyzing Linux Device Drivers.");
                 ("--regions", Arg.Set GU.regions, "Enable region-based race detection.");
                 ("--showtemps", Arg.Set CF.showtemps, " Shows CIL's temporary variables when printing the state.");
                 ("--uncalled", Arg.Set GU.print_uncalled, " Display uncalled functions.");
                 ("--result", Arg.String setstyle, "<style>  Result style: none, state, indented, compact, or pretty.");
                 ("--analysis", Arg.String setanalysis, "<name>  Picks the analysis: mutex, no_path, base.");
                 ("--dump", Arg.String setdump, "<path>  Dumps the results to the given path");
                 ("--cilout", Arg.String setcil, "<path>  Where to dump cil output");
                 ] in
  let recordFile fname = 
    fileNames := fname :: (!fileNames) in
  (* The temp directory for preprocessing the input files *)
  let dirName = GU.create_dir "goblin_temp" in
  Stats.reset Stats.HardwareIfAvail;
  CF.init();
  Arg.parse speclist recordFile usage_str;
  if !GU.allfuns then GU.multi_threaded := true;
  (* GU.regions := true; *)
  let _ = match !GU.dump_path with
    | Some path -> begin
        M.warn_out := open_out (Filename.concat path "warnings.out");
        outFile := "" (*Filename.concat path "analysis.out";*)
        (* --dump overwrites the -o flag*)
      end
    | _ -> ()
  in
  (* The include files, libc stuff  *)
  let warn_includes () = print_endline "Warning, cannot find goblin's custom include files." in
  let includes = ref (if Sys.file_exists(!include_dir) then "-I" ^ !include_dir else (warn_includes () ; "")) in
  let _ = includes := !includes ^ " " ^ !other_includes in
  let libc = Filename.concat !include_dir "lib.c" in 
  fileNames := List.rev !fileNames;
  if !use_libc then fileNames := libc :: !fileNames;
  if !GU.kernel then begin
    let preconf = Filename.concat !include_dir "linux/goblint_preconf.h" in 
    let autoconf = Filename.concat kernel_dir "linux/autoconf.h" in 
    cppflags := "-D__KERNEL__ -include " ^ preconf ^ " -include " ^ autoconf ^ " " ^ !cppflags;
    includes := !includes ^ " -I" ^ kernel_dir ^ " -I" ^ kernel_dir ^ "/asm-x86/mach-default"
  end;
  (* preprocess all the files *)
  let preproFile fname =
    (* The actual filename of the preprocessed sourcefile *)
    let nname =  Filename.concat dirName (Filename.basename fname) in 
    (* Preprocess using gcc -E *)
    let command = "gcc -E " ^ !cppflags ^ " " ^ !includes ^ " " ^ fname ^ " -o " ^ nname in
      ignore (Unix.system command);  (* MAYBE BAD IDEA to ingore! *)
      nname
  in
  let cpp_file_names = 
    if !GU.verbose then print_endline "Preprocessing files.";
    List.map preproFile !fileNames in
  (* and get their AST *)
  let files_AST = 
    if !GU.verbose then print_endline "Parsing files.";
    List.map CF.getAST cpp_file_names in
  let _ = if !keep_cpp then () else ignore (Unix.system ("rm -rf " ^ dirName)) in
  (* direct the output to file if requested  *)
  let _ = if not (!outFile = "") then GU.out :=  open_out !outFile in
  let _ = Errormsg.logChannel := M.get_out "cil" !cilout in
  (* we use CIL to merge all inputs to ONE file *)
  let merged_AST = 
    match files_AST with
      | [one] -> one
      | [] -> 
          prerr_endline "No arguments for Goblint?"; 
          prerr_endline usage_str; 
          prerr_endline "Try `goblint --help' for more information."; 
          exit 2
      | xs -> CF.getMergedAST xs 
  in
    (* using CIL's partial evaluation and constant folding! *)
    if !dopartial then CF.partial merged_AST;
    CF.rmTemps merged_AST;
    (* creat the Control Flow Graph from CIL's AST *)
    CF.createCFG merged_AST;
    CF.ugglyImperativeHack := merged_AST;
    (* we let the "--eclipse" flag override result style: *)
    if !GU.eclipse then GU.result_style := GU.Compact;
    if !justCil then 
      (* if we only want to print the output created by CIL: *)
      CF.print merged_AST
    else begin
      (* we first find the functions to analyze: *)
      if !GU.verbose then print_endline "And now...  the Goblin!";
      let funs = 
        if !GU.allfuns then CF.getFuns merged_AST
        else [CF.getMain merged_AST]
      in
        (* and here we run the analysis! *)
        Stats.time "analysis" (!analyze merged_AST) funs;
        if !Cilutil.printStats then 
          Stats.print (M.get_out "timing" stderr) "Timings:\n"
    end

let _ = 
  main ()
