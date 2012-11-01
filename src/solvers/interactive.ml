open GobConfig
open Messages
open Progress
open Pretty

let run = ref false
let break = ref []
let oldcommand = ref []
let read () = 
  flush_all ();
  let x = input_line !Goblintutil.command_in in
  flush_all ();
  if !Goblintutil.command_port <> 0 then ignore (Printf.printf "'%s'\n" x);
  x 
let event x =   
  output_string !Goblintutil.event_out x;
  output_string !Goblintutil.event_out "\n";
  flush !Goblintutil.event_out
let close_sockets () =
  Unix.close Goblintutil.command_socket;
  Unix.close Goblintutil.event_socket

module GU = Goblintutil

module Make 
  (Var: Analyses.VarType)  (* the equation variables *)
  (VDom: Lattice.S) (* the domain *)
  (G: Glob.S) =
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes

  module GCache = Cache.OneVar (G.Var)
  module WorkSet = Set.Make (Var)
  
  let cons_unique key x xs =
    let xk = key x in
    if List.exists (fun y -> xk = key y) xs
    then xs
    else x::xs
  
  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' =
    let sigma: VDom.t VMap.t = VMap.create 113 (VDom.bot ()) in
    let theta = GMap.create 113 (GDom.bot ()) in
    let vInfl = VMap.create 113 ([]: (constrain * int) list) in
    let gInfl = GMap.create 113 ([]: (constrain * int) list) in
    let todo  = VMap.create 113 ([]: (rhs * int) list) in
    let unsafe = ref ([]: (constrain * int) list) in
    let workset = ref (List.fold_right WorkSet.add initialvars WorkSet.empty) in
    
    let rec constrainOneVar (x: variable) =
      let rec debugger os ls = 
        let print_globs () =
          let print_one x d = ignore (Pretty.fprintf !Goblintutil.command_out "%s " x.Cil.vname) in
          ignore (Pretty.fprintf !Goblintutil.command_out "globals: ");          
          GMap.iter print_one theta;
          ignore (Pretty.fprintf !Goblintutil.command_out "\n");          
        in
        let print_glob x =
          let print_one y d = 
            if Some y.Cil.vname = x 
            then ignore (Pretty.fprintf !Goblintutil.command_out "%s = %a\n" y.Cil.vname GDom.pretty d)
          in
          GMap.iter print_one theta
        in
        let listing () =
          let n = Var.line_nr x in
          let f = Var.file_name x in
          ignore (Unix.system ("cat -n "^f^" | sed -n '"^string_of_int(n-1)^","^string_of_int(n+1)^ " p'"))
        in
        let split s =
          Str.split (Str.regexp "[ \t\r\n]+") s
        in
        let rec unsplit = function
          | [] -> ""
          | x::[] -> x
          | x::y::ys -> x^unsplit (y::ys)
        in
        let help = 
          [" Commands to the InTeRaCtIvE goblint solver. woot!"
          ;" "
          ;" run          runs the analysis from current position"
          ;" step         advance to the next constraint"
          ;" list         show program text context of the current constraint"
          ;" "            
          ;" context      prints the context of the current function"
          ;" state        prints the current local state"
          ;" old          prints the old local state"
          ;" "            
          ;" globs        lists the global variables"
          ;" glob x       prints the value of the global 'x'"
          ;" allglobs     prints the values of all globals"
          ;" "            
          ;" break n      breaks at the line 'n'"
          ;" unbreak n    removes the break at line 'n'"
          ;" "            
          ;" help         prints this text "
          ]
        in
        let action () =
          if !GU.command_port=(-1) then ignore (Pretty.fprintf !Goblintutil.command_out "(goblint) ");
          let command = split (read ()) in
          if command<>[] then oldcommand := command;
          match !oldcommand with
            | ["q"]    -> event "terminated"; close_sockets (); exit 0
            | ["run"]  -> event "resume run";  run := true; raise Exit
            | ["step"] -> event "resume step"; raise Exit
            | ["position"] -> ignore (Pretty.fprintf !Goblintutil.command_out "%d|%s\n" (Var.line_nr x) (Var.file_name x))
            | ["break";y] when try ignore (int_of_string y); true with Failure _ -> false -> 
                if !GU.command_port=(-1) then ignore (Pretty.fprintf !Goblintutil.command_out "break at %s:%s\n" (Var.file_name x) y);
                break := (Var.file_name x,int_of_string y) :: !break
            | ["unbreak";y] when try ignore (int_of_string y); true with Failure _ -> false ->
                break := List.filter ((<>) (Var.file_name x, int_of_string y)) !break
            | ["old"]  -> ignore (Pretty.fprintf !Goblintutil.command_out "Old local state:\n%a\n" VDom.pretty os)
            | ["state"]  -> ignore (Pretty.fprintf !Goblintutil.command_out "New local state:\n%a\n" VDom.pretty ls)
            | ["context"]  -> ignore (Pretty.fprintf !Goblintutil.command_out "Context:\n%a\n" Var.context x)
            | ["allglobs"] -> print_glob None
            | ["globs"] -> print_globs ()
            | ["glob";x] -> print_glob (Some x)
            | ["list"] -> listing ()
            | ["help"] -> List.iter print_endline help
            | x -> if !GU.command_port=(-1) then ignore (Pretty.fprintf !Goblintutil.command_out "Unrecognized command '%s', maybe you need 'help'.\n" (unsplit x))
          in
          try action (); debugger os ls with Exit -> ()
      in
      let rhsides = 
        let notnew = VMap.mem sigma in 
          if notnew x then
            let temp = VMap.find todo x in VMap.remove todo x; temp
          else begin
            if tracing && Var.category x = 3 then trace "sol" "New %a\n" Var.pretty_trace x;
            VMap.add sigma x (VDom.bot ());  (* danger! Adding default value!!! If the datastruct refuses this,  membership test will fail -> inf. loop *)
            fst (List.fold_right (fun x (xs,i) -> (x,i)::xs, i+1) (system x) ([],0))
          end
      in 
	
      begin if rhsides=[] then ()
      else begin
        let local_state = ref (VDom.bot ()) in 
        let constrainOneRHS (f, i) =
          let doOneGlobalDelta = function
            | `L (v, state) ->
              if not ( VDom.leq state (VDom.bot ()) ) then
                (* If a variable has become live we must solve it "manually" 
                   because there are no dependecies to it yet. *)
                begin if not (VMap.mem sigma v) then constrainOneVar v end;
                let oldstate = VMap.find sigma v in
                let compls = VDom.join oldstate state in
                  if not (VDom.leq compls oldstate) then begin
                    let lst = VMap.find vInfl v in
                    VMap.replace sigma v compls;
                    unsafe := lst @ !unsafe;
                    VMap.remove vInfl v
                  end
                  
            | `G (g, gstate) -> 
              if not ( GDom.leq gstate (GDom.bot ()) ) then
                let oldgstate = GMap.find theta g in
                let compgs = GDom.join oldgstate gstate in
                  if not (GDom.leq compgs oldgstate) then begin
                    let lst = GMap.find gInfl g in
                    GMap.replace theta g (GDom.widen oldgstate compgs);
                    unsafe := lst @ !unsafe;
                    GMap.remove gInfl g
                  end
          in
            let (nls,tc) = f (vEval ((x,f),i), GCache.cached (gEval ((x,f),i))) doOneGlobalDelta in
            if get_bool "exp.eclipse" then show_add_work_buf (List.length tc);
            List.iter constrainOneVar tc;
            local_state := VDom.join !local_state nls;
        in
          List.iter constrainOneRHS rhsides;
          let old_state = VMap.find sigma x in
          if (not !run) then event "suspend step";
          if (!run && List.mem (Var.file_name x,Var.line_nr x) !break) then begin
            event "suspend run";
            run := false
          end;
          if (not !run) then begin
            if !GU.command_port=(-1) then ignore (Pretty.fprintf !Goblintutil.command_out "File: %s\n%d: %s\n" (Var.file_name x) (Var.line_nr x) (Var.description x));
            debugger old_state !local_state
          end;
          let new_val = VDom.join !local_state old_state in
          if not (VDom.leq new_val old_state) then begin
            VMap.replace sigma x (VDom.widen old_state new_val);
            let influenced_vars = ref WorkSet.empty in
            let collectInfluence ((y,f),i) = 
              VMap.replace todo y (cons_unique snd (f,i) (VMap.find todo y));             
              influenced_vars := WorkSet.add y !influenced_vars
            in
              List.iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
              WorkSet.iter constrainOneVar !influenced_vars;
          end 
    end end          

    and vEval (c: constrain * int) var =
      if get_bool "exp.eclipse" then show_add_work_buf 1;
      constrainOneVar var;
      VMap.replace vInfl var (c :: VMap.find vInfl var);
      VMap.find sigma var 
    
    and gEval (c: constrain * int) glob = 
      GMap.replace gInfl glob (c :: GMap.find gInfl glob);
      GMap.find theta glob 

    in
      event "started";
      GU.may_narrow := false;
      let add_start (v,d) = 
        VMap.add sigma v d;
        let edges = fst (List.fold_right (fun x (xs,i) -> (x,i)::xs, i+1) (system v) ([],0)) in
        VMap.add todo v edges;
        workset := WorkSet.add v !workset
      in
      List.iter add_start start ;
      while not (WorkSet.is_empty !workset) do
        WorkSet.iter constrainOneVar !workset;
        workset := WorkSet.empty;
        let recallConstraint ((y,f),i) = 
          VMap.replace todo y (cons_unique snd (f,i) (VMap.find todo y));
          workset := WorkSet.add y !workset;
        in
          List.iter recallConstraint !unsafe;
          unsafe := [];
      done;
      event "terminated";
      close_sockets ();
      (sigma, theta)
end 
