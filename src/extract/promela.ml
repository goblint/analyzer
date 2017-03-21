open BatteriesExceptionless
(* https://bitbucket.org/camlspotter/ppx_monadic *)

module Dsl = struct
  (* promela types *)
  type _ t =
    | Enum : 'a * ('a -> string) -> 'a t
    | Chan : 'a chan -> 'a chan t
    | Bool : bool -> bool t
    | Byte : int -> int t | Short : int -> int t | Int : int -> int t (* maybe do something like Bigarray.kind to avoid conflating ints of different size *)
    (* the following are for arguments, and can not be declared in promela *)
    | String : string -> string t
    | Void : unit t (* this is just for skipping uninteresting arguments *)
  and 'a chan = { capacity : int (* 0 = rendez-vous *); mutable content : 'a list; content_type : 'a t }
  type 'a e = 'a * string (* expression and its promela representation *)

  let enums = Hashtbl.create 123 (* value -> type (both as strings) *)
  let of_enums = Hashtbl.create 123 (* enum type -> (enum value as int -> enum value as string option) *)
  let enum of_enum show name =
    Hashtbl.add of_enums name (fun x -> Option.map show (of_enum x));
    let values = List.of_enum @@ (0 -- 13) //@ of_enum /@ show in
    List.iter (flip (Hashtbl.add enums) name) values;
    (), "mtype = { "^ String.concat ", " values ^" }"
  let enum_type x = Hashtbl.find enums x
  let is_enum = Option.is_some % Hashtbl.find enums
  let rec is_declared : type a . a t -> bool = function
    | Enum (x,s) -> is_enum (s x)
    | Chan c -> is_declared c.content_type
    | _ -> true
  let rec unbox : type a . a t -> string * (a -> string) * a = function (* type, show, value *)
    | Enum (x,s) -> "mtype", s, x
    | Chan c ->
      let t',s,x = unbox c.content_type in
      "chan", (fun c -> "["^string_of_int c.capacity^"] of {"^t'^"}"), c
    | Bool x -> "bool", string_of_bool, x
    | Byte x -> "byte", string_of_int, x
    | Short x -> "short", string_of_int, x
    | Int x -> "int", string_of_int, x
    | String x -> "string", identity, x
    | Void -> "byte", (fun () -> assert false), ()

  type _ var = (* variable with initial value and printer. type: content * get * set * mod *)
    | Var : { name : string; init : 'a t; value : 'a ref; show : 'a -> string } ->
      ('a e * 'a e * ('a e -> unit e) * unit e) var
    | Arr : { name : string; init : 'a t; value : 'a array; show : 'a -> string; show_all : 'a array -> string; length : int } ->
      ('a e * (int e -> 'a e) * (int e * 'a e -> unit e) * (int e -> unit e)) var

  let var value name =
    assert (is_declared value);
    let t,s,v = unbox value in
    Var { name; init = value; value = ref v; show = s },
    t^" "^name^" = "^s v^";"
  let arr length value name =
    assert (is_declared value);
    let t,s,v = unbox value in
    let show_arr a = Array.to_list a |> List.map s |> String.concat ", " in
    Arr { name; init = value; value = Array.create length v; show = s; show_all = show_arr; length },
    t^" "^name^"["^string_of_int length^"] = "^s v^";"

  (* to avoid having to give the string for enum literals, we use a dummy string and replace it using the show function from the delcaration *)
  let fix_enum show (x,s) = if s = "enum" then show x else s

  let get : type c g s m. (c*g*s*m) var -> g = function
    | Var v -> !(v.value), v.name
    | Arr v -> fun i ->
      assert (fst i >= 0 && fst i < v.length);
      Array.get v.value (fst i), v.name^"["^snd i^"]"
  let set : type c g s m. (c*g*s*m) var -> s = function
    | Var v -> fun x -> (v.value := fst x), v.name^" = "^fix_enum v.show x^";"
    | Arr v -> fun (i,x) ->
      assert (fst i >= 0 && fst i < v.length);
      Array.set v.value (fst i) (fst x), v.name^"["^snd i^"] = "^fix_enum v.show x^";"

  (* Var x: !x, x := v. Arr x: !x i, x := i, v *)
  let (!) = get
  let (:=) = set
  let (@=) = set (* maybe use this since it has higher precedence than >> *)

  (* we do not define `not` since this may tempt people to use !empty instead of nempty (same with nfull) *)

  let _assert x = assert (fst x), "assert("^snd x^");"
  let println x = (* print_endline (fst x) *) (), "printf(\\\"%s\\n\\\""^snd x^");"

  let _true  = true,  "true"
  let _false = false, "false"

  let wait x = ignore (fst x = true), "("^snd x^");" (* a standalone expression in Promela is a statement that blocks until the expression is true *)
  let break = (), "break" (* jumps to the end of the innermost do loop *)
  let skip = wait _true
  let nop = (), ""

  module Chan = struct (* channels *)
    let create len content = Chan { capacity = len; content = []; content_type = content }

    let full   (c,s) = List.length c.content >= c.capacity, "full("^s^")"
    let nfull  (c,s) = List.length c.content < c.capacity, "nfull("^s^")"
    let empty  (c,s) = List.length c.content = 0, "empty("^s^")"
    let nempty (c,s) = List.length c.content > 0, "nempty("^s^")"
    let len    (c,s) = List.length c.content, "len("^s^")"

    (*  ? = first, ?? = any, no <> = consume, <> = remain, TODO eval, !! *)
    let first c e = match c.content with x::_ when x = fst e -> true | _ -> false
    let any c e = List.mem (fst e) c.content
    let poll m (c,s) e = (* checks (no side-effects) if (the first|any) element is e *)
      let b,op = match m with
        | `First -> first c e, "?"
        | `Any   -> any c e,   "??"
      in
      b, s^op^"["^snd e^"]"
    let recv m (c,s) e = (* checks if (the first|any) element is e (w/o changing e) and removes it from the channel *)
      let b,op = match m with
        | `First -> first c e, "?"
        | `Any   -> any c e,   "??"
      in
      c.content <- List.remove c.content (fst e);
      b, s^op^"eval("^snd e^")"
    let send (c,s) e = (* append|sorted *)
      fst (nfull (c,s)), s^"!"^snd e
  end

  (* OCaml's ; - sequential composition *)
  let (>>) a b = fst b, snd a^"\n"^snd b
  let bind x f = x >> f (fst x)
  let (>>=) = bind
  let return x = x (* ? *)

  let indent x = String.nsplit x "\n" |> List.map (fun x -> "  "^x) |> String.concat "\n"

  let surround a b (v,s) = v, a^"\n"^indent s^"\n"^b
  let _match xs =
    assert (List.length xs > 0);
    let s = String.concat "\n" @@ List.map (fun (c,b) -> ":: ("^snd c^") -> "^snd b) xs in
    (*List.find (fst%fst) xs, s*)
    (), s
  let _do xs = surround "do" "od;" (_match xs)
  let _if xs = surround "if" "fi;" (_match xs)
  let _ifte b x y =
    (* TODO `else` fails for channel operations, also, how to implement in OCaml? *)
    let c = true, "else" in
    _if [b, x; c, y]
  let _ift b x = _ifte b x skip
  let _for (Var i) (Arr v) b = (* for (i in a) { b } *)
    surround ("for ("^i.name^" in "^v.name^") {") "}" b
  let _foreach (Arr v as a) b = (* for (i in a) { `b i a[i]` } *)
    var (Byte 0) ("i_"^v.name) >>= (* TODO this doesn't work for nested loops on the same array! *)
    fun i -> _for i a (b !i (!a !i)) (* NOTE this already gives the values to the body instead of the variables TODO for interpreter *)


  (* extraction *)
  type ('a,'b,'c,'d,'e,'z) args = A0 of 'z e | A1 of 'a * ('a -> 'z e) | A2 of 'a*'b * ('a -> 'b -> 'z e) | A3 of 'a*'b*'c * ('a -> 'b -> 'c -> 'z e) | A4 of 'a*'b*'c*'d * ('a -> 'b -> 'c -> 'd -> 'z e) | A5 of 'a*'b*'c*'d*'e * ('a -> 'b -> 'c -> 'd -> 'e -> 'z e)

  type eval = (* how to evaluate function arguments *)
    | EvalEnum of (int -> string option) | EvalInt | EvalString (* standard types *)
    | EvalSkip (* don't evaluate *)
    | AssignIdOfString of string * int (* kind * position: evaluate argument at position as a string, use it to generate an id of kind, and assign it to argument, map id to int for promela *)
    (*| EvalSpecial of string*)

  let extract_funs = Hashtbl.create 123 (* name of function to list of how to evaluate arguments *)
  let special_fun name = Hashtbl.find extract_funs name
  let special_funs () = Hashtbl.keys extract_funs |> List.of_enum

  let extract ?id fname args = (* generate code for function and register for extraction *)
    let unpack =
      let name (Var v) = v.name in
      let eval (type a) (Var v : a var) = match v.init with
        | Enum (x,s) -> EvalEnum (Option.get @@ Hashtbl.find of_enums (Option.get (enum_type (s x))))
        | Byte _ -> EvalInt
        | Short _ -> EvalInt
        | Int _ -> EvalInt
        | String _ -> EvalString
        | Void -> EvalSkip
        | _ -> failwith ("unsupported argument type for "^v.name)
      in
      let p a = name a, eval a in
      function
      | A0 z -> [], z
      | A1 (a,f) -> [p a], f a
      | A2 (a,b,f) -> [p a; p b], f a b
      | A3 (a,b,c,f) -> [p a; p b; p c], f a b c
      | A4 (a,b,c,d,f) -> [p a; p b; p c; p d], f a b c d
      | A5 (a,b,c,d,e,f) -> [p a; p b; p c; p d; p e], f a b c d e
    in
    let aa, body = unpack args in
    let aa' = List.filter (fun (n,e) -> e <> EvalSkip && e <> EvalString) aa in (* don't want skipped arguments or strings in definition *)
    let arg_names, arg_evals = List.map fst aa', List.map snd aa in
    let arg_evals = match id with
      | Some (dst, src, res) -> List.modify_at dst (fun _ -> AssignIdOfString (res, src)) arg_evals
      | None -> arg_evals
    in
    Hashtbl.add extract_funs fname arg_evals;
    (), "\ninline "^fname^"("^String.concat ", " arg_names^") { atomic {\n" ^
      indent (snd body) ^
    "\n}}\n"


  (* overwrite remaining operators *)
  let op o s x y = o (fst x) (fst y), snd x^" "^s^" "^snd y
  let (==) x y = op (=)  "==" x y
  let (!=) x y = op (<>) "!=" x y
  let (<)  x y = op (<)  "<"  x y
  let (>)  x y = op (>)  ">"  x y
  let (<=) x y = op (<=) "<=" x y
  let (>=) x y = op (>=) ">=" x y
  let (+)  x y = op (+)  "+"  x y
  let (-)  x y = op (-)  "-"  x y
  let (/)  x y = op (/)  "/"  x y
  let ( * ) x y = op ( * ) "*" x y
  let (&&) x y = op (&&) "&&" x y
  let (||) x y = op (||) "||" x y

  let i i = i, string_of_int i (* int literal *)
  let s s = s, s (* string literal *)
  let (^) a b = fst a^fst b, snd a^snd b (* string concatenation *)
  let fail m = println (s "FAIL: "^m) >> _assert _true (* TODO _assert _false evaluates too early :( *)
  let i2s (i,s) = string_of_int i, s
  let e x = x, "enum"
  let e2s x = s (snd x) (* TODO ? *)

  let incr : type g s m. (int e*g*s*m) var -> m = function
    | Var _ as v -> set v (get v + i 1)
    | Arr _ as v -> fun j -> set v (j, (get v j + i 1))
  let decr : type g s m. (int e*g*s*m) var -> m = function
    | Var _ as v -> set v (get v - i 1)
    | Arr _ as v -> fun j -> set v (j, (get v j - i 1))
end

(* ARINC *)
(* enums *)
type return_code = SUCCESS | ERROR
and partition_mode = IDLE | COLD_START | WARM_START | NORMAL
and status = NOTCREATED | STOPPED | SUSPENDED | WAITING | READY | RUNNING | DONE
and waiting_for = NONE | BLACKBOARD | SEMA | EVENT | TIME
and queuing_discipline = FIFO | PRIO
[@@deriving show, enum]

let extract_types = ["t123"] (* TODO extract variables of a certain type *)

let os =
  let str_remove m s = String.nreplace ~str:s ~sub:m ~by:"" in
  let nomod show = str_remove "Promela." % show in (* remove the module prefix. TODO this should be an option for ppx_deriving show *)
  let ntasks = 42 in let nsemas = 42 in (* TODO analyze number of resources *)
  let preemption = true in
  let has_semas = nsemas > 0 in
  let open Dsl in let open Chan in
  Dsl.do_; (* from now on ; is bind *)
  (* type delcarations, TODO generate this? *)
  (* TODO might need adjustment if there are enums with gaps or enums not starting at 0 *)
  enum return_code_of_enum (nomod show_return_code) "return_code";
  enum partition_mode_of_enum (nomod show_partition_mode) "partition_mode";
  enum status_of_enum (nomod show_status) "status";
  enum waiting_for_of_enum (nomod show_waiting_for) "waiting_for";
  enum queuing_discipline_of_enum (nomod show_queuing_discipline) "queuing_discipline";
  (* variable declarations *)
  (* TODO inject: let%s status = arr ntask NOTCREATED in *)
  partition_mode <-- var (Enum (COLD_START, nomod show_partition_mode)) "partition_mode";
  lock_level  <-- var (Byte 0) "lock_level"; (* scheduling only takes place if this is 0 *)exclusive   <-- var (Byte 0) "exclusive"; (* id of process that has exclusive privilige toecute if lockLevel > 0 *)
  status      <-- arr ntasks (Enum (NOTCREATED, nomod show_status)) "status";
  (* TODO type for structured data types *)
  waiting_for <-- arr ntasks (Enum (NONE, nomod show_waiting_for)) "waiting_for";
  waiting_id  <-- arr ntasks (Byte 0) "waiting_id";
  semas       <-- arr nsemas (Byte 0) "semas";
  semas_max   <-- arr nsemas (Byte 0) "semas_max";
  semas_chan  <-- arr nsemas (Chan.create ntasks (Byte 0)) "semas_chan";

  (* just for asserts *)
  tasks_created <-- var (Byte 0) "tasks_created";
  semas_created <-- var (Byte 0) "semas_created";

  (* helpers *)
  let task_info id = s "status["^i2s id^s "] = "^e2s (!status id)^s ", waiting_for[] = "^e2s (!waiting_for id)^s ", waiting_id[] = "^i2s (!waiting_id id) in
  let sema_info id = s "semas["^i2s id^s "] = "^i2s (!semas id) in
  let set_waiting id wfor wid = Dsl.do_;
    println (s "set_waiting: process "^i2s id^s " will wait for "^i2s wid);
    waiting_for := id, (e wfor);
    waiting_id  := id, wid;
    status      := id, (e WAITING)
  in
  let set_ready id = Dsl.do_;
    println (s "set_ready: process "^i2s id^s " set to ready. "^task_info id);
    waiting_for := id, (e NONE);
    waiting_id  := id, i 0;
    status      := id, (e READY)
  in
  let is_waiting id wfor wid = !status id == e WAITING && !waiting_for id == e wfor && !waiting_id id == wid in
  let can_run id = (!status id == e READY || !status id == e RUNNING) && (!lock_level == i 0 || !exclusive == id) && (!partition_mode == e NORMAL || id == i 0) in
  let is_running id = !status id = e RUNNING in
  let remove_waiting id = Dsl.do_;
    if has_semas then
      _foreach semas (fun j _ ->
          _ift (poll `Any (!semas_chan j) id) (wait (recv `Any (!semas_chan j) id))
        )
    else nop;
    waiting_for := id, e NONE;
  in

  (* this is the id we give out for every new task *)
  let tid,tid_decl = var (Byte 0) "tid" in
  (* general arguments *)
  let id,_     = var (Byte 0) "id" in
  let name,_   = var (String "") "name" in
  (*let r,_    = var (Enum (SUCCESS, show_return_code)) "r" in*)

  (* preemption *)
  let mode,_ = var (Enum (COLD_START, nomod show_partition_mode)) "mode" in
  extract "LockPreemption" @@ A0 (Dsl.do_;
    incr lock_level;
    exclusive := !tid; (* TODO is this really changed if lock_level > 0? if yes, it is probably also restored... *)
  );
  extract "UnlockPreemption" @@ A0 (
    _ift (!lock_level > i 0) (decr lock_level)
  );
  extract "SetPartitionMode" @@ A1 (mode, fun mode ->
    partition_mode := !mode
  );

  (* processes *)
  extract "CreateProcess" @@ A1 (id(*; pri; per; cap]*), fun id -> Dsl.do_;
    _assert (!status !id == e NOTCREATED);
    status := !id, e STOPPED;
    waiting_for := !id, e NONE;
    incr tasks_created;
  );
  (* CreateErrorHandler *)
  extract "Start" @@ A1 (id, fun id -> Dsl.do_;
    _assert (!status !id != e NOTCREATED);
    remove_waiting !id;
    status := !id, e READY;
  );
  extract "Stop" @@ A1 (id, fun id -> Dsl.do_;
    _assert (!status !id != e NOTCREATED);
    remove_waiting !id;
    status := !id, e STOPPED;
  );
  extract "Suspend" @@ A1 (id, fun id -> Dsl.do_;
    _assert (!status !id != e NOTCREATED);
    status := !id, e SUSPENDED;
  );
  extract "Resume" @@ A1 (id, fun id -> Dsl.do_;
    _assert (!status !id != e NOTCREATED);
    _ift (!status !id == e SUSPENDED) (
      _ifte (!waiting_for !id == e NONE)
        (status := !id, e READY)
        (status := !id, e WAITING)
    );
    status := !id, e SUSPENDED;
  );

  (* semaphores *)
  let cur,_   = var (Byte 0) "cur" in
  let max,_   = var (Byte 0) "max" in
  let queuing,_ = var (Enum (FIFO, nomod show_queuing_discipline)) "queuing" in
  extract "CreateSemaphore" ~id:(4,0,"sema") @@ A5 (name,cur,max,queuing,id, fun name cur max queuing id -> Dsl.do_;
    println (s "CreateSemaphore: " ^ !name ^s ", "^ i2s !cur ^s ", "^ i2s !max ^s ", "^ e2s !queuing);
    _assert (!queuing == e FIFO);
    semas := !id, !cur;
    semas_max := !id, !max;
    incr semas_created;
  );
  extract "GetSemaphoreId" ~id:(1,0,"sema") @@ A2 (name, id, fun name id -> nop);
  extract "WaitSemaphore" @@ A1 (id, fun id ->
    let id = !id in
    let sema = !semas id in
    let chan = !semas_chan id in
    _if [
      sema == i 0,
        println (s "WaitSema will block: "^sema_info id) >>
        _if [
          full  chan, fail (s "WaitSema: queue is full: "^sema_info id);
          nfull chan, println (s "WaitSema: Process "^i2s !tid^s " put into queue for sema "^i2s id)
        ] >>
        set_waiting !tid SEMA id;
      sema > i 0,
        println (s "WaitSema will go through: "^sema_info id) >>
        incr semas id;
      sema < i 0,
        fail (s "WaitSema: count<0: "^sema_info id)
    ]
  );
  extract "SignalSemaphore" @@ A1 (id, fun id ->
    let id = !id in
    let sema = !semas id in
    let chan = !semas_chan id in
    _if [
      (* no processes waiting on this semaphore -> increase count until max *)
      empty chan,
        println (s "SignalSema: empty queue") >>
        _ift (sema < !semas_max id) (incr semas id);
      nempty chan,
        println (s "SignalSema: "^i2s (len chan)^s " processes in queue for "^sema_info id) >>
        _foreach status (fun j _ ->
          println (s "SignalSema: check if process "^i2s j^s " is waiting. "^task_info j) >>
          _ift (is_waiting j SEMA id && poll `First chan j) (* process is waiting for this semaphore and is at the front of its queue *) (
              println (s "SignalSema: process "^i2s !tid^s " is waking up process "^i2s j) >>
              wait (recv `First chan j) >> (* consume msg from queue *)
              set_ready j >>
              break
          )
        )
    ]
  );

  (* events *)
