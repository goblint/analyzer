open Cil
open Pretty
module Trivial =  ConcDomain.Simple

module Spec =
struct
  module Dom  = Lattice.ProdConf (struct let expand_fst = true let expand_snd = true end) (Mutex.NoBaseSpec.Dom) (IntDomain.Flattened)
  module Glob = Mutex.NoBaseSpec.Glob

  let oilFile = ref ""

  (*priority function, currently onlywokrs for tasks*)
  let pry f = 
    if Sys.file_exists(Filename.dirname(Sys.executable_name) ^ "/osek_temp/priorities.txt") then begin
      let oil = open_in (Filename.dirname(Sys.executable_name) ^ "/osek_temp/priorities.txt") in
      let rec look_up line f =
	match line with
	| "" -> if (input_line oil = "default") then int_of_string (input_line oil) else -1
	| _ -> if line = f.svar.vname then int_of_string(input_line oil) else look_up (input_line oil) f
      in
      let ret = look_up (input_line oil) f in
      close_in oil ; (`Lifted (Int64.of_int ret));
      end else begin
      prerr_endline "Priorites could not be determined." ;
      exit 2;
    end 

  (*brutal hack*)
  let is_task f = 
    (String.length f.svar.vname >= 12 && String.sub f.svar.vname 0 12 = "function_of_")
  
   (* queries *)
  let query ask _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = 
    Queries.Result.top ()
 
  (* transfer functions *)
  let assign a (lval:lval) (rval:exp) gl (st:Dom.t) : Dom.t =
    (Mutex.NoBaseSpec.assign a lval rval gl  (fst st), (snd st))
    (* add to access table (copy from mutex?); *)
     (*assignment*)
   
  let branch a (exp:exp) (tv:bool) gl (st:Dom.t) : Dom.t = 
    (Mutex.NoBaseSpec.branch a (exp:exp) (tv:bool) gl (fst st), snd st) 
(*if*)
  
  let body a (f:fundec) gl (st:Dom.t) : Dom.t = 
    let m_st = Mutex.NoBaseSpec.body a (f:fundec) gl (fst st) in
    if (is_task f) then 
	(m_st, (pry f))
    else 
	(m_st, snd st)
    (*entry*)

  let return a (exp:exp option) (f:fundec) gl (st:Dom.t) : Dom.t =
    let m_st = Mutex.NoBaseSpec.return a (exp:exp option) (f:fundec) gl (fst st) in
    if (is_task f) then 
      (m_st, `Bot)
    else 
      (m_st, snd st)
    (*return*)
  
  let eval_funvar a (fv:exp) gl (st:Dom.t) : varinfo list = 
    Mutex.NoBaseSpec.eval_funvar a (fv:exp) gl (fst st)
    (*functionnames?*)
    
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) gl (st:Dom.t) : (Dom.t * Dom.t) list =
    List.map (fun (x,y) -> ((x,(snd st)),(y,(snd st)))) (Mutex.NoBaseSpec.enter_func a (lval: lval option) (f:varinfo) (args:exp list) gl (fst st))
    (*helper, no idea*)
  
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) gl (bu:Dom.t) (au:Dom.t) : Dom.t =
   (Mutex.NoBaseSpec.leave_func a (lval:lval option) (f:varinfo) (args:exp list) gl (fst bu) (fst au), snd au)

(*map (fun x->x) Mutex.NoBaseSpec.acc *)
  
  let special_fn a (lval: lval option) (f:varinfo) (arglist:exp list) gl (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    let constant_lock c =
      if Sys.file_exists("/osek_temp/resources.txt") then begin
	let res = open_in "/osek_temp/resources.txt" in
	let rec look_up line id = 
	  if line = id then input_line res else look_up (input_line res) id in 
	let var = makeVarinfo true (look_up (input_line res) c) (TVoid []) in
	  close_in res; [AddrOf (Var var,NoOffset)];
      end else begin
	prerr_endline "Resources could not be determined." ;
	exit 2;
      end 
    in
    let arglist =
      match f.vname with
        | "GetResource" | "ReleaseResource" -> 
           (match arglist with 
             | [Lval l] -> [AddrOf l] 
	     | [Const (CInt64 (c,_,_) ) ] -> constant_lock (Int64.to_string c)
             | x -> x)
        | _ -> arglist
    in List.map (fun (x,y,z) -> ((x,snd st),y,z)) (Mutex.NoBaseSpec.special_fn a lval f arglist gl (fst st))
  
  let fork ask lv f args gs ls = 
    List.map (fun (x,y) -> (x,(y,snd ls))) (Mutex.NoBaseSpec.fork ask lv f args gs (fst ls))

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()

  let get_diff _ = []
  let reset_diff x = x
  
  let name = "Thread analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true
  let finalize () = begin
    ignore (Unix.system("rm -rf osek_temp") );
    Mutex.NoBaseSpec.finalize ()
  end
  let init () =   
    if !oilFile != "" && Sys.file_exists(!oilFile) then begin
      ignore (Unix.system("mkdir osek_temp") );
      ignore (Unix.system( "ruby " ^ Filename.dirname(Sys.executable_name) ^"/scripts/parse_oil.rb " ^ !oilFile) );
      let tramp = Filename.dirname(!oilFile) ^ "/defaultAppWorkstation/tpl_os_generated_configuration.h" in
      if Sys.file_exists(tramp) then begin
	ignore (Unix.system ("ruby " ^ Filename.dirname(Sys.executable_name) ^ "/scripts/parse_trampoline.rb " ^ tramp) )
      end else begin
	prerr_endline "Trampoline headers not found." ;
	exit 2;
      end
    end else begin
      prerr_endline "OIL-file does not exist." ;
      exit 2;
    end;   
end

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
