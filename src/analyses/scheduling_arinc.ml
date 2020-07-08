(** An analysis specification for didactic purposes. *)
open Prelude.Ana
open Analyses_arinc
open Arinc_schedulabilty_domain
open Arinc_cfg

module Spec : Analyses_arinc.ArincSpec =
struct
  include Analyses_arinc.DefaultSpec

  let suspended = ProcessState.of_int (Int64.of_int 2)

  let name () = "scheduling_arinc"
  module D = Arinc_schedulabilty_domain.D
  module G = Arinc_schedulabilty_domain.D
  module C = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    failwith "lol, wut?!!!"

  let branch ctx (exp:exp) (tv:bool) : D.t =
    failwith "lol, wut?!!!"

  let body ctx (f:fundec) : D.t =
    failwith "lol, wut?!!!"

  let return ctx (exp:exp option) (f:fundec) : D.t =
    failwith "lol, wut?!!!"

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    (* let s = match ctx.node with
      | PC [a;b] -> "["^ string_of_int a ^ ","^ string_of_int b ^ "]"
      | _ -> ""
    in
    Printf.printf "enter for %s : %s\n %s \n\n------------------------------------------\n" s (D.short 80 ctx.local) (Printexc.raw_backtrace_to_string (Printexc.get_callstack 20)); *)
    let state1 = {
      pid = Pid.of_int (Int64.of_int 1);
      priority = Priority.of_int (Int64.of_int 15);
      period = Period.of_int (Int64.of_int 600);
      capacity = Capacity.of_int (Int64.of_int 600);
      processState = ProcessState.of_int (Int64.of_int 0)} in
    let state2 = {
      pid = Pid.of_int (Int64.of_int 2);
      priority = Priority.of_int (Int64.of_int 10);
      period = Period.top ();
      capacity = Capacity.top ();
      processState = ProcessState.of_int (Int64.of_int 2)}
    in
    [ctx.local, (state1, state2)]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    failwith "lol, wut?!!!"

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    failwith "lol, wut?!!!"

  let arinc_edge ctx (t,e) =
    (* let s = match ctx.node with
      | PC [a;b] -> "["^ string_of_int a ^ ","^ string_of_int b ^ "]"
      | _ -> ""
    in
    Printf.printf "%s : %s\n" s (D.short 80 ctx.local);
    raise Deadcode *)
    let (a, b) = ctx.local in
    if t = -1 then
      ctx.local
    else if t = 1 then
      if ProcessState.to_int b.processState = Some (Int64.of_int 2) then
        (* (Printf.printf "Dead code"; *)
        raise Deadcode (* ) *)
      else
        raise Deadcode
    else
      match e with
      | SuspendTask 0 -> ctx.local
      | ResumeTask 0 -> ctx.local
      | _ -> ctx.local

  let should_join (a, b) (a', b') = a.processState = a'.processState && b.processState = b'.processState

  let val_of () = D.bot ()
  let context _ = ()

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP_arinc.register_analysis (module Spec : ArincSpec)
