module GU = Goblintutil
module M = Messages

open Batteries
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
  (*let yesno x = if x then str "Yes" else str "No" in
  let intd = 
    let tr = get_bool "ana.int.trier" in
    let inv = get_bool "ana.int.interval" in
    match tr, inv with
      | true , true  -> str "Kildall domain with exclusion sets & intervals"
      | true , false -> str "Kildall domain with exclusion sets "
      | false, true  -> str "intervals"
      | false, false -> str "disabled"
  in*)
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
      let name = snd x in
        match phase name with 
            None -> xs 
            | Some n -> (name, cont name, path name)::xs 
    in
    List.fold_left f [] !MCP.analyses_table 
  in
  match get_bool "printstats", get_string "result" with
    | _ , "html" ->
      begin
        let filesTable = 
          tag "ol"
          begin
            let f pr fn =
              let fn = Filename.basename fn in 
              if Sys.file_exists ("./" ^ report_dir ^ "/" ^ fn ^ ".html") then
                pr <:> tag "li" (tag "a" ~tp:["href",fn^".html"] (str fn))
              else
                pr <:> tag "li" (tag "span" ((str fn) <:> str " (no analysis information)"))
            in 
            List.fold_left f (fun _ -> ()) fileNames 
          end
        in
        let o = Legacy.open_out (Filename.concat "result" "index.html") in
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
                        [ [ tag "h5" ~tp:["class","toggle_ttl";"title","ana"] (str "Enabled&nbsp;Analyses")
                          ; table ~tp:["id","ana"] 
                            ( [str "Analysis"; str "Context"; str "Path"]
                            ::List.map (fun (x,c,p) -> [str x; sens c; sens p] ) phaseTbl) 
                          ]
                        ; [ tag "h5" ~tp:["class","toggle_ttl off";"title","detail"] (str "Details")
                          ; tag "pre" ~tp:["class","prettyprint";"id","detail"] (fun c -> GobConfig.print (IO.output_channel c))
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
          <:> tag "script" ~tp:["src","https://google-code-prettify.googlecode.com/svn/loader/run_prettify.js"] (fun c -> ())
          <:> tag "div"~tp:["id","date"]
                (fun c -> Legacy.Printf.fprintf c "generated on %02d %s %04d.\n" 
                                (t.Unix.tm_mday) months.(t.Unix.tm_mon) (1900+t.Unix.tm_year))         
        end 
      end
    | true, _ ->
      ignore (Pretty.printf "vars = %d    evals = %d  \n" !Goblintutil.vars !Goblintutil.evals);
      flush_all ();
      prerr_endline "Solver stats:";
      prerr_endline ("  globals changed "^string_of_int !Goblintutil.globals_changed^" times");
      Stats.print (M.get_out "timing" Legacy.stderr) "Timings:\n"
    | _ -> ()

