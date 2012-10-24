module GU = Goblintutil
module M = Messages

open Htmlutil
open Printf
open Json

let report_dir = "result"

let prepare_html_report () =
  let dr = GU.create_dir report_dir in
  let css_ch = open_out (dr^"/style.css") in
  fprintf css_ch "%s" Css_template.css_string;
  let js_ch = open_out (dr^"/script.js") in
  fprintf js_ch "%s" Js_template.js_string

let do_stats fileNames =
  let an = array !(field GU.conf "analyses") in
  let cn = objekt !(field GU.conf "context") in
  let sn = objekt !(field GU.conf "sensitive") in
  let cont x = bool !(field cn x) in
  let path x = bool !(field sn x) in
  let sens x = if x then str "Sensitive" else str "Insensitive" in
  let yesno x = if x then str "Yes" else str "No" in
  let listp x = str (String.concat ", " x) in
  let intd = 
    let tr = bool !(field (objekt !(field GU.conf "int_domain")) "trier") in
    let inv = bool !(field (objekt !(field GU.conf "int_domain")) "interval") in
    match tr, inv with
      | true , true  -> str "Kildall domain with exclusion sets & intervals"
      | true , false -> str "Kildall domain with exclusion sets "
      | false, true  -> str "intervals"
      | false, false -> str "disabled"
  in
  let phase x =
    let rec phase' n = function
      | [] -> None
      | xs::xss when List.exists (fun y->x=string !y) (!(array !xs)) -> Some n
      | xs::xss -> phase' (n+1) xss
    in
    phase' 1 !an
  in      
  let phaseTbl = 
    let f xs x = 
        match phase x.MCP.featurename with 
            None -> xs 
            | Some n -> (x.MCP.featurename,n,cont x.MCP.featurename,path x.MCP.featurename)::xs 
    in
    List.fold_left f [] !MCP.analysesList 
  in
  match !Cilutil.printStats, !GU.result_style with
    | _ , GU.Html ->
      begin
        let filesTable = 
          tag "ol"
          begin
            let f pr fn =
              let fn = Filename.basename fn in 
              pr <:> tag "li" (tag "a" ~tp:["href",fn^".html"] (str fn))
            in 
            List.fold_left f (fun _ -> ()) fileNames 
          end
        in
        let o = open_out (Filename.concat "result" "report.html") in
        (*let o = stdout in*)
        let t = Unix.localtime (Unix.time ()) in
        let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                        "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |] in
        newfile o 
        begin
          tag "head" 
          begin
            tag "title" (str "Goblint Report") <:>
            tag "meta" ~tp:["http-equiv","Content-Type";"content","text/html; charset=utf-8"] (str "") <:>
            tag "link" ~tp:["rel","stylesheet";"href","style.css";"type","text/css"] (str "") <:>
            tag "script" ~tp:["type","text/javascript";"src","script.js"] (str "")
          end <:>
          tag "body" ~tp:["onload","init_all();"]
          begin
            tag "h2" (str "Goblint Analysis Report,") <:>
            tag "div" ~tp:["id","content"]
            begin
              table' ~tp:["width","100%";"class","mmenu"] ~rp:[["width","1%"]]
                [ [ tag "h3" ~tp:["class","toggle_ttl";"title","files"] (str "Files")
                  ; tag "div" ~tp:["id","files"] filesTable]
                ; [ tag "h3" ~tp:["class","toggle_ttl off";"title","conf"] (str "Configuration")
                  ; tag "div" ~tp:["id","conf"]
                    begin
                      table' ~tp:["class","amenu";"width","100%"] ~rp:[["width","1%"]]
                        [ [ tag "h5" ~tp:["class","toggle_ttl";"title","gprop"] (str "General&nbsp;Properties")
                          ; table ~tp:["id","gprop"] 
                              [[str "Property"; str "Value"]
                              ;[str "Solver" ; str !GU.solver]
                              ;[str "Propagation"; if !GU.forward then str "Forward" else str "Demand-driven"]
                              ;[str "Total phases" ; str (string_of_int (List.length !an))]
                              ;[str "Starting with all functions"; yesno !GU.allfuns]
                              ;[str "Starting with non-static functions"; yesno !GU.nonstatic]
                              ;[str "Main functions"; listp !GU.mainfuns]
                              ;[str "Other functions"; listp !GU.otherfuns]
                              ;[str "Exit functions"; listp !GU.exitfuns]
                              ] 
                          ]
                        ; [ tag "h5" ~tp:["class","toggle_ttl";"title","ana"] (str "Enabled&nbsp;Analyses")
                          ; table ~tp:["id","ana"] 
                            ( [str "Analysis"; str "Phase"; str "Context"; str "Path"]
                            ::List.map (fun (x,n,c,p) -> [str x; num n; sens c; sens p] ) phaseTbl) 
                          ]
                        ; [ tag "h5" ~tp:["class","toggle_ttl off";"title","detail"] (str "Details")
                          ; table ~tp:["id","detail"] 
                              [[str "Property"; str "Value"]
                              ;[str "Integer domain"; intd]
                              ;[str "Ignoring read-races"; yesno !Mutex.no_read]
                              ;[str "Assume locking succeeds"; yesno (not !LibraryFunctions.failing_locks)]
                              ;[str "Field insensitive accesses"; yesno !Mutex.field_insensitive]
                              ;[str "Globalize early"; yesno !GU.earlyglobs]
                              ;[str "Ignoring read-races"; yesno !GU.region_offsets]
                              ;[str "Analyze kernel modules"; yesno !GU.kernel]
                              ] 
                          ]
                        ]
                    end]
                ; [ tag "h3" ~tp:["class","toggle_ttl off";"title","stats"] (str "Stats") 
                  ; tag "pre" ~tp:["id","stats"] (fun c -> Stats.print c "Timings:\n\n") ]
                ; [ tag "h3" ~tp:["class","toggle_ttl off";"title","vers"] (str "Version") 
                  ; tag "div" ~tp:["id","vers"]
                    begin
                      let f b = if b then "enabled" else "disabled" in
                      table 
                      [[str "Component"   ; str "Version"]
                      ;[str "Goblint"     ; str Version.goblint]
                      ;[str "Cil"         ; str Version.cil]
                      ]<:>
                      table 
                      [[str "Goblint Build Option"; str "Status"]
                      ;[str "Tracing"     ; str (f Config.tracing)]
                      ;[str "Tracking"    ; (fun c -> fprintf c "%s (n=%d)" (f Config.tracking) Config.track_n)]
                      (*;[str "Experimental"; str (f Config.experimental)]*)
                      ]
                    end] 
                ]
            end
          end
          <:> tag "div"~tp:["id","date"]
                (fun c -> Printf.fprintf c "generated on %02d %s %04d.\n" 
                                (t.Unix.tm_mday) months.(t.Unix.tm_mon) (1900+t.Unix.tm_year))         
        end 
      end
    | true, _ ->
      ignore (Pretty.printf "vars = %d    evals = %d  \n" !EffectWCon.vars !EffectWCon.evals);
      flush_all ();
      prerr_endline "Solver stats:";
      prerr_endline ("  globals changed "^string_of_int !Goblintutil.globals_changed^" times");
      Stats.print (M.get_out "timing" stderr) "Timings:\n"
    | _ -> ()

