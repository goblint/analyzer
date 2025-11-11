(** Utilities for witness generation and witness invariants. *)

open MyCFG
open GoblintCil

module NH = Hashtbl.Make (Node)
module NS = Set.Make (Node)

let find_main_entry entrystates =
  let (main_entry_nodes, other_entry_nodes) =
    entrystates
    |> List.map fst
    |> List.partition (function
        | FunctionEntry f, _ -> f.svar.vname = "main"
        | _, _ -> false
      )
  in
  match main_entry_nodes, other_entry_nodes with
  | [], _ -> failwith "no main_entry_nodes"
  | _ :: _ :: _, _ -> failwith "multiple main_entry_nodes"
  | _, _ :: _ -> failwith "some other_entry_nodes"
  | [main_entry], [] -> main_entry

let find_loop_heads (module FileCfg: FileCfg): unit NH.t =
  let loop_heads = NH.create 100 in
  let global_visited_nodes = NH.create 100 in

  (* DFS *)
  let rec iter_node path_visited_nodes node =
    if NS.mem node path_visited_nodes then
      NH.replace loop_heads node ()
    else if not (NH.mem global_visited_nodes node) then begin
      NH.replace global_visited_nodes node ();
      let new_path_visited_nodes = NS.add node path_visited_nodes in
      List.iter (fun (_, to_node) ->
          iter_node new_path_visited_nodes to_node
        ) (FileCfg.Cfg.next node)
    end
  in

  Cil.iterGlobals FileCfg.file (function
      | GFun (fd, _) ->
        let entry_node = FunctionEntry fd in
        iter_node NS.empty entry_node
      | _ -> ()
    );

  loop_heads


module Invariant (FileCfg: MyCFG.FileCfg) =
struct
  open FileCfg

  let emit_loop_head = GobConfig.get_bool "witness.invariant.loop-head"
  let emit_after_lock = GobConfig.get_bool "witness.invariant.after-lock"
  let emit_other = GobConfig.get_bool "witness.invariant.other"

  let loop_heads = find_loop_heads (module FileCfg)

  let is_after_lock to_node =
    List.exists (fun (edges, from_node) ->
        List.exists (fun (_, edge) ->
            match edge with
            | Proc (_, Lval (Var fv, NoOffset), args) when LibraryFunctions.is_special fv ->
              Goblint_backtrace.wrap_val ~mark:(Cilfacade.FunVarinfo fv) @@ fun () ->
              let desc = LibraryFunctions.find fv in
              begin match desc.special args with
                | Lock _ -> true
                | _ -> false
              end
            | _ -> false
          ) edges
      ) (Cfg.prev to_node)

  let is_invariant_node cfgnode =
    if NH.mem loop_heads cfgnode then
      emit_loop_head
    else if is_after_lock cfgnode then
      emit_after_lock
    else
      emit_other

  let find_syntactic_loop_head = function
    | Statement s ->
      let n' = Statement (LoopUnrolling.find_original s) in
      let prevs = Cfg.prev n' in
      List.find_map (fun (edges, prev) ->
          let stmts = Cfg.skippedByEdge prev edges n' in
          List.find_map (fun s' ->
              match s'.GoblintCil.skind with
              | Loop (_, loc, _, _, _) -> Some loc
              | _ -> None
            ) stmts
        ) prevs
    | FunctionEntry _ | Function _ -> None

  let find_syntactic_loop_condition_label s =
    List.find_map (function
        | Label (name, loc, false) when String.starts_with ~prefix:"__loop_condition" name -> Some loc
        | _ -> None
      ) s.labels

  let find_syntactic_loop_condition = function
    | Statement s as n ->
      (* No need to LoopUnrolling.find_original because loop unrolling duplicates __loop_condition labels (with new suffixes). *)
      begin match find_syntactic_loop_condition_label s with
        | Some _ as r -> r
        | None ->
          (* The __loop_condition label may not be on s itself, but still on the surrounding Block (skipped in CFG) due to basic blocks transformation. *)
          let prevs = Cfg.prev n in
          List.find_map (fun (edges, prev) ->
              let stmts = Cfg.skippedByEdge prev edges n in
              List.find_map find_syntactic_loop_condition_label stmts
            ) prevs
      end
    | FunctionEntry _ | Function _ -> None
end

module YamlInvariant (FileCfg: MyCFG.FileCfg) =
struct
  include Invariant (FileCfg)

  let is_stub_node n =
    let fundec = Node.find_fundec n in
    Cil.hasAttribute "goblint_stub" fundec.svar.vattr

  let location_location (n : Node.t) = (* great naming... *)
    match n with
    | Statement s ->
      let {loc; _}: CilLocation.locs = CilLocation.get_stmtLoc s in
      if not loc.synthetic && is_invariant_node n && not (is_stub_node n) then (* TODO: remove is_invariant_node? i.e. exclude witness.invariant.loop-head check *)
        Some loc
      else
        None
    | FunctionEntry _ | Function _ ->
      (* avoid FunctionEntry/Function, because their locations are not inside the function where asserts could be inserted *)
      None

  let is_invariant_node n = Option.is_some (location_location n)

  let loop_location n =
    find_syntactic_loop_condition n
    |> BatOption.filter (fun _loc -> not (is_stub_node n))

  let is_loop_head_node n = Option.is_some (loop_location n)
end

module YamlInvariantValidate (FileCfg: MyCFG.FileCfg) =
struct
  include Invariant (FileCfg)

  (* TODO: filter synthetic?

     Almost all loops are transformed by CIL, so the loop constructs all get synthetic locations. Filtering them from the locator could give some odd behavior: if the location is right before the loop and all the synthetic loop head stuff is filtered, then the first non-synthetic node is already inside the loop, not outside where the location actually was.
     Similarly, if synthetic locations are then filtered, witness.invariant.loop-head becomes essentially useless.
     I guess at some point during testing and benchmarking I achieved better results with the filtering removed. *)

  let is_loop_head_node = NH.mem loop_heads
end
[@@deprecated]


module InvariantParser =
struct
  type t = {
    genv: (string, Cabs2cil.envdata * Cil.location) Hashtbl.t;
    global_vars: Cil.varinfo list;
  }

  module VarinfoH = Cilfacade.VarinfoH

  let create (file: Cil.file): t =
    (* Reconstruct genv from CIL file instead of using genvironment,
       because genvironment contains data from all versions of the file
       and incremental update doesn't remove the excess. *)
    let genv = Hashtbl.create (Hashtbl.length Cabs2cil.genvironment) in
    let global_vars = VarinfoH.create 113 in (* Deduplicates varinfos from declarations and definitions. *)
    Cil.iterGlobals file (function
        | Cil.GType ({tname; _} as t, loc) ->
          let name = "type " ^ tname in
          Hashtbl.replace genv name (Cabs2cil.EnvTyp (TNamed (t, [])), loc)
        | Cil.GCompTag ({cstruct; cname; _} as c, loc)
        | Cil.GCompTagDecl ({cstruct; cname; _} as c, loc) ->
          let name = (if cstruct then "struct" else "union") ^ " " ^ cname in
          Hashtbl.replace genv name (Cabs2cil.EnvTyp (TComp (c, [])), loc)
        | Cil.GEnumTag ({ename; eitems; _} as e, loc)
        | Cil.GEnumTagDecl ({ename; eitems; _} as e, loc) ->
          let typ = TEnum (e, []) in
          let name = "enum " ^ ename in
          Hashtbl.replace genv name (Cabs2cil.EnvTyp typ, loc);
          List.iter (fun (name, _, exp, loc) ->
              Hashtbl.replace genv name (Cabs2cil.EnvEnum (exp, typ), loc)
            ) eitems
        | Cil.GVar (v, _, loc)
        | Cil.GVarDecl (v, loc)
        | Cil.GFun ({svar=v; _}, loc) ->
          Hashtbl.replace genv v.vname (Cabs2cil.EnvVar v, loc);
          VarinfoH.replace global_vars v ()
        | _ -> ()
      );
    let global_vars = List.of_seq (VarinfoH.to_seq_keys global_vars) in
    {genv; global_vars}

  let parse_cabs (inv: string): (Cabs.expression, string) result =
    Errormsg.hadErrors := false; (* reset because CIL doesn't *)
    match Timing.wrap "FrontC" Frontc.parse_standalone_exp inv with
    | _ when !Errormsg.hadErrors ->
      Error "hadErrors"
    | inv_cabs ->
      Ok inv_cabs
    | exception (Frontc.ParseError e) ->
      Errormsg.log "\n"; (* CIL prints garbage without \n before *)
      Error e

  let parse_cil {genv; global_vars} ?(check=true) ~(fundec: Cil.fundec) ~loc (inv_cabs: Cabs.expression): (Cil.exp, string) result =
    let env = Hashtbl.copy genv in
    List.iter (fun (v: Cil.varinfo) ->
        Hashtbl.replace env v.vname (Cabs2cil.EnvVar v, v.vdecl)
      ) (fundec.sformals @ fundec.slocals);

    Errormsg.hadErrors := false; (* reset because CIL doesn't *)
    let inv_exp_opt =
      Cil.currentLoc := loc;
      Cil.currentExpLoc := loc;
      Cabs2cil.currentFunctionFDEC := fundec;
      let old_locals = fundec.slocals in
      let old_useLogicalOperators = !Cil.useLogicalOperators in
      Fun.protect ~finally:(fun () ->
          fundec.slocals <- old_locals; (* restore locals, Cabs2cil may mangle them by inserting temporary variables *)
          Cil.useLogicalOperators := old_useLogicalOperators
        ) (fun () ->
          Cil.useLogicalOperators := true;
          Timing.wrap "Cabs2cil" (Cabs2cil.convStandaloneExp ~genv ~env) inv_cabs
        )
    in

    let vars = fundec.sformals @ fundec.slocals @ global_vars in
    match inv_exp_opt with
    | _ when !Errormsg.hadErrors ->
      Error "hadErrors"
    | Some inv_exp when not check || Check.checkStandaloneExp ~vars inv_exp ->
      Ok inv_exp
    | _ ->
      Error "parse_cil"
end

module Locator (E: Set.OrderedType) =
struct
  module FileH = BatHashtbl.Make (Basetype.RawStrings)
  module LocM = BatMap.Make (CilType.Location)
  module ES = BatSet.Make (E)

  (* for each file, locations have total order, so LocM essentially does binary search *)
  type t = ES.t LocM.t FileH.t

  let create () = FileH.create 100

  let add (file_loc_es: t) (loc: Cil.location) (e: E.t): unit =
    FileH.modify_def LocM.empty loc.file (
      LocM.modify_def ES.empty loc (ES.add e)
    ) file_loc_es

  let find_opt (file_loc_es: t) (loc: Cil.location): ES.t option =
    let open GobOption.Syntax in
    let* loc_es = FileH.find_option file_loc_es loc.file in
    let* (_, es) = LocM.find_first_opt (fun loc' ->
        CilType.Location.compare loc loc' <= 0 (* allow inexact match *)
      ) loc_es
    in
    if ES.is_empty es then
      None
    else
      Some es

  let clear (file_loc_es: t): unit =
    FileH.clear file_loc_es
end
