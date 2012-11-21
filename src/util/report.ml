module GU = Goblintutil
module M = Messages

open Batteries_uni
open GobConfig
open Htmlutil
open Printf
open Json

let report_dir = "result"

let prepare_html_report () =
  let dr = GU.create_dir report_dir in
  let css_ch = open_out (dr^"/style.css") in
  fprintf css_ch "%s" Css_template.css_string;
  close_out css_ch;
  let js_ch = open_out (dr^"/script.js") in
  fprintf js_ch "%s" Js_template.js_string;
  close_out js_ch

let do_stats fileNames =
  let an = get_list "ana.activated" in
  let cn = get_list "ana.ctx_insens" |> List.map Json.string in
  let sn = get_list "ana.path_sens" |> List.map Json.string in
  let cont x = List.mem x cn |> not in
  let path x = List.mem x sn in
  let sens x = if x then str "Sensitive" else str "Insensitive" in
  let yesno x = if x then str "Yes" else str "No" in
  let listp x = str (String.concat ", " x) in
  let intd = 
    let tr = get_bool "ana.int.trier" in
    let inv = get_bool "ana.int.interval" in
    match tr, inv with
      | true , true  -> str "Kildall domain with exclusion sets & intervals"
      | true , false -> str "Kildall domain with exclusion sets "
      | false, true  -> str "intervals"
      | false, false -> str "disabled"
  in
  let phase x =
    let rec phase' n = function
      | [] -> None
      | xs::xss when List.exists (fun y->x=string !y) (!(array xs)) -> Some n
      | xs::xss -> phase' (n+1) xss
    in
    phase' 1 an
  in      
  let phaseTbl = 
    let f xs x = 
        match phase x.MCP.featurename with 
            None -> xs 
            | Some n -> (x.MCP.featurename,n,cont x.MCP.featurename,path x.MCP.featurename)::xs 
    in
    List.fold_left f [] !MCP.analysesList 
  in
  match get_bool "printstats", get_string "result" with
    | _ , "html" ->
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
        let o = Legacy.open_out (Filename.concat "result" "report.html") in
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
                              ;[str "Solver" ; str (get_string "solver")]
                              ;[str "Propagation"; if (get_bool "exp.forward") then str "Forward" else str "Demand-driven"]
                              ;[str "Total phases" ; str (string_of_int (List.length an))]
                              ;[str "Starting with all functions"; yesno (get_bool "allfuns")]
                              ;[str "Starting with non-static functions"; yesno (get_bool "nonstatic")]
                              ;[str "Main functions"; listp (List.map string (get_list "mainfun"))]
                              ;[str "Other functions"; listp (List.map string (get_list "otherfun"))]
                              ;[str "Exit functions"; listp (List.map string (get_list "exitfun"))]
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
(*                              ;[str "Assume locking succeeds"; yesno (not !LibraryFunctions.failing_locks)]*)
                              ;[str "Field insensitive accesses"; yesno !Mutex.field_insensitive]
                              ;[str "Globalize early"; yesno (get_bool "exp.earlyglobs")]
                              ;[str "Ignoring read-races"; yesno (get_bool "exp.region-offsets")]
                              ;[str "Analyze kernel modules"; yesno (get_bool "kernel")]
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
                      ;[str "Tracking"    ; (fun c -> Legacy.Printf.fprintf c "%s (n=%d)" (f Config.tracking) Config.track_n)]
                      (*;[str "Experimental"; str (f Config.experimental)]*)
                      ]
                    end] 
                ]
            end
          end
          <:> tag "div"~tp:["id","date"]
                (fun c -> Legacy.Printf.fprintf c "generated on %02d %s %04d.\n" 
                                (t.Unix.tm_mday) months.(t.Unix.tm_mon) (1900+t.Unix.tm_year))         
        end 
      end
    | true, _ ->
      ignore (Pretty.printf "vars = %d    evals = %d  \n" !EffectWCon.vars !EffectWCon.evals);
      flush_all ();
      prerr_endline "Solver stats:";
      prerr_endline ("  globals changed "^string_of_int !Goblintutil.globals_changed^" times");
      Stats.print (M.get_out "timing" Legacy.stderr) "Timings:\n"
    | _ -> ()

