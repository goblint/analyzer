(** Shared utilities for interactive server modes (JSON-RPC and MCP).
    
    This module provides common functionality for server implementations,
    including ARG access, invariant parsing, and node location lookup.
*)

open Batteries
open GoblintCil

module InvariantParser = WitnessUtil.InvariantParser

(** Is node valid for lookup by location?
    
    Filters out synthetic nodes (e.g., artificially created for analysis purposes)
    that don't correspond to actual source code locations. This is used for
    abstract debugging breakpoints and interactive queries.
    
    @param cfgnode The CFG node to check
    @return true if the node is valid for location-based lookup *)
let is_server_node cfgnode =
  let loc = UpdateCil.getLoc cfgnode in
  not loc.synthetic

(** Module signature for ARG wrapper with node lookup capabilities *)
module type ArgWrapper =
sig
  module Arg: ArgTools.BiArg
  module Locator: module type of WitnessUtil.Locator (Arg.Node)
  val locator: Locator.t
  val find_node: string -> Arg.Node.t
  val find_cfg_node: string -> Arg.Node.t list
end

(** Create arg_wrapper as a resettable lazy value.
    
    Provides access to the Abstract Reachability Graph (ARG) with node lookup
    capabilities by string ID and CFG node. The ARG must be computed by an
    analysis before this can be accessed.
    
    @raise Failure if analysis hasn't been run or ARG is disabled in configuration *)
let create_arg_wrapper () : (module ArgWrapper) ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      let module Arg = (val (Option.get_exn !ArgTools.current_arg 
        (Failure "ARG not available: either analysis hasn't been run yet or ARG is disabled in configuration"))) in
      let module Locator = WitnessUtil.Locator (Arg.Node) in
      let module StringH = Hashtbl.Make (Printable.Strings) in

      let locator = Locator.create () in
      let ids = StringH.create 113 in
      let cfg_nodes = StringH.create 113 in
      Arg.iter_nodes (fun n ->
          let cfgnode = Arg.Node.cfgnode n in
          let loc = UpdateCil.getLoc cfgnode in
          if is_server_node cfgnode then
            Locator.add locator loc n;
          StringH.replace ids (Arg.Node.to_string n) n;
          StringH.add cfg_nodes (Node.show_id cfgnode) n
        );

      let module ArgWrapper =
      struct
        module Arg = Arg
        module Locator = Locator
        let locator = locator
        let find_node = StringH.find ids
        let find_cfg_node = StringH.find_all cfg_nodes
      end
      in
      (module ArgWrapper: ArgWrapper)
    )

(** Create invariant_parser as a resettable lazy value.
    Provides parsing of C expressions for invariant evaluation. *)
let create_invariant_parser () : InvariantParser.t ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      InvariantParser.create !Cilfacade.current_file
    )

(** Create node_locator as a resettable lazy value.
    Provides lookup of CFG nodes by source location. *)
let create_node_locator () : WitnessUtil.Locator(Node).t ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      let module Cfg = (val !MyCFG.current_cfg) in
      let module Locator = WitnessUtil.Locator (Node) in
      let locator = Locator.create () in

      (* DFS, copied from CfgTools.find_backwards_reachable *)
      let module NH = MyCFG.NodeH in
      let reachable = NH.create 100 in
      let rec iter_node node =
        if not (NH.mem reachable node) then begin
          NH.replace reachable node ();
          let loc = UpdateCil.getLoc node in
          if is_server_node node then
            Locator.add locator loc node;
          List.iter (fun (_, prev_node) ->
              iter_node prev_node
            ) (Cfg.prev node)
        end
      in

      Cil.iterGlobals !Cilfacade.current_file (function
          | GFun (fd, _) ->
            let return_node = Node.Function fd in
            iter_node return_node
          | _ -> ()
        );

      locator
    )
