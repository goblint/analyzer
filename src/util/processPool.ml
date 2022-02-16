type task = {
  command: string;
  cwd: string option;
}

let run ?(terminated=fun _ _ -> ()) tasks =
  let n = GobConfig.get_int "jobs" in
  let procs = Hashtbl.create n in
  let rec run tasks =
    match tasks with
    | task :: tasks when Hashtbl.length procs < n ->
      let old_cwd = Sys.getcwd () in
      let proc =
        match task.cwd with
        | Some cwd ->
          Fun.protect ~finally:(fun () ->
              Sys.chdir old_cwd
            ) (fun () ->
              Sys.chdir cwd;
              Unix.open_process task.command
            )
        | None ->
          Unix.open_process task.command
      in
      let pid = Unix.process_pid proc in
      Hashtbl.replace procs pid (task, proc);
      run tasks
    | [] when Hashtbl.length procs = 0 ->
      ()
    | _ ->
      let (pid, status) = Unix.wait () in
      begin match Hashtbl.find_opt procs pid with
        | Some (task, (proc_in, proc_out)) ->
          (* Unix.close_process proc; *)
          (* only part of close_process, no need to wait *)
          close_in proc_in;
          close_out proc_out;
          Hashtbl.remove procs pid;
          terminated task status
        | None -> (* unrelated process *)
          ()
      end;
      run tasks
  in
  run tasks
