open Cil
open Pretty

module M = Messages
module GU = Goblintutil

(* Compile time constants: *)
let tracking = Config.tracking
let n =        Config.track_n


let call_hashtbl = Hashtbl.create 101

let track_call f h =
  if Hashtbl.mem call_hashtbl (f,h) 
  then incr (Hashtbl.find call_hashtbl (f,h))
  else Hashtbl.add call_hashtbl (f,h) (ref 0)
  
let track_call_profile () = 
  let report_fn (x,h) v =
    print_endline ("Profiler: Function "^x.vname^" with context hash "^string_of_int h^" called "^ string_of_int !v ^" times.")
  in
  Hashtbl.iter report_fn call_hashtbl

(* Auxialiary data structures: *)
let current_n = ref 64
let reached_loc_hashtbl = Hashtbl.create 1001

let track_with_profile () =
  let rec insert n (x,l) xs rdy =
    match n, xs with
      | 0, _  -> []
      | _, [] -> [x,l]
      | _, (lx,ll)::xs when rdy || x > lx -> (x,l) :: insert (n-1) (lx,ll) xs true
      | _, y::ys -> y::insert (n-1) (x,l) ys rdy
  in
  let r = Hashtbl.fold (fun k v x -> insert 11 (!v,k) x false) reached_loc_hashtbl [] in
  let print_node (n,l) =
    Messages.print_msg ("Hotspot: visited " ^ string_of_int n ^ " times") l
  in
  List.iter print_node r

let track_with (notify: int -> unit): unit = 
  let loc = !Tracing.current_loc in
  if (Hashtbl.mem reached_loc_hashtbl loc) then
    begin
      let visited_count = Hashtbl.find reached_loc_hashtbl loc in
      (* Need this because calling notify will trigger demand-driven evaluation
       * of other nodes, so we have to update the real current_n before! *)
      let the_current_n = !current_n in
	visited_count := (succ !visited_count);
	if (!visited_count > the_current_n) then
	  begin
	    current_n := the_current_n * n;
	    notify the_current_n
	  end
    end
  else
    Hashtbl.add reached_loc_hashtbl loc (ref 1)

let track () = 
  let msg n = M.warn_all ("Line visited more than " ^ string_of_int n ^ " times.") in
    track_with msg

let show_subtask (subt:string) (len:int) =
  Printf.printf "PROGRESS /-/ SUBTASK /-/ %s /-/ %d\n%!" subt len


let show_add_work (len:int) =
  Printf.printf "PROGRESS /-/ MORE WORK /-/ %d\n%!" len


let add_work_count = ref 0 
let add_work_acc = ref 0
let show_add_work_buf (len:int) =
  if !add_work_count > (2) && (abs !add_work_acc) > 2 then begin
      show_add_work (len + !add_work_acc);
      add_work_count := 0;
      add_work_acc := 0
    end 
  else begin
      add_work_count := !add_work_count + 1;
      add_work_acc   := !add_work_acc   + len
    end      


let show_worked (len:int) =
  Printf.printf "PROGRESS /-/ WORKED /-/ %d\n%!" len


let worked_count = ref 0 
let worked_acc = ref 0 
let show_worked_buf (len:int) =
  if !worked_count > (2) && (abs !worked_acc) > 2 then begin
      show_worked (len + !worked_acc);
      worked_count := 0;
      worked_acc := 0
    end 
  else begin
      worked_count := !worked_count + 1;
      worked_acc   := !worked_acc   + len
    end
      
