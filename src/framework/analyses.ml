(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Cil
open Pretty

module GU = Goblintutil
module M  = Messages

(** General type of an analyzer. *)
module type S =
sig
  val name: string
  (** name of the analyzer*)
  val analyze: file -> fundec list -> unit
  (** analyze a file -- output using Messages.* *)
end

module type VarType = 
sig
  include Hashtbl.HashedType
  val pretty_trace: unit -> t -> doc
  val is_global : t -> bool
end
  

module type Spec = 
sig
  module Dom : Lattice.S   
  (** THE data structure *)
  module Dep : VarType     
  (** global data structure *)    
  
  val name: string
  (** name of the analysis *)
  val init: unit -> unit
  (** first function to be called when analyzing using this Spec *)
  val finalize: unit -> unit
  (** last function to be called when analyzing using this Spec *)
  
  val should_join         : Dom.t -> Dom.t -> bool
  (** sensitivity predicate *)
  val startstate: Dom.t
  (** state to start analyzing the main function*)
  val otherstate: Dom.t
  (** state to start analyzing other functions (usual when calling './goblint --allfuns ...') *)
  
  val es_to_string: fundec -> Dom.t -> string
  (** no-one knows .. somehow used when generating final output *)

  (** Functions dealing with global: *)
  
  val get_changed_globals : Dom.t -> Dom.t -> Dep.t list 
  (** [get_changed_globals x y] returns list of globals that are different in [x] and [y] *)
  val filter_globals      : Dom.t -> Dom.t
  (** [filter_globals x] throws out all information, that is not considered [x]-s global state *)
  val insert_globals      : Dom.t -> Dom.t -> Dom.t
  (** [insert_globals x y]  replaces global state from [x] with the global state in [y] *)

  (** Functions dealing with dependency list maintanance: *)

  val reset_global_dep    : Dom.t -> Dom.t
  (** [reset_global_dep x]  returns state [x] where the inner globals dependency list is reset*)
  val get_global_dep      : Dom.t -> Dep.t list
  (** [get_global_dep x]  returns list of recent global dependencies --- global variables that have been queried since last reset *)

  
  (** Transfer functions:  *)
  
  val assign: lval -> exp -> Dom.t -> Dom.t 
  (** handle assignments *)
  val branch: exp -> bool -> Dom.t -> Dom.t
  (** handle branches *)
  val body  : fundec      -> Dom.t -> Dom.t
  (** steping inside of a function body *)
  val return: exp option  -> fundec -> Dom.t -> Dom.t
  (** steping out from a function *)
  

  (** Transfer function for function calls: *)
  
  (* Basic scheme:
    
                 |-> enter_func -> <analyze the functions> -> leave_func -> join -> ...
    eval_funvar -|-> special_fn ----------------------------------------------^
                 |-> fork  ------------------------------------------------------->
  *)
  
  
  val eval_funvar : exp -> Dom.t -> varinfo list
  (** [eval_funvar f st] evaluates [f] to a list of possible functions (in state [st]) *)
  val fork       : lval option -> varinfo -> exp list -> Dom.t -> (varinfo * Dom.t) list  
  (** [fork] returns list of function,input-state pairs, that the callee has spawned *)
  val special_fn : lval option -> varinfo -> exp list -> Dom.t -> Dom.t
  (** [special_fn] is called, when given varinfo is not connected to a fundec -- no function definition is given*)
  val enter_func : lval option -> varinfo -> exp list -> Dom.t -> Dom.t list 
  (** [enter_func] returns input-states that must be analyzed for the given function *)
  val leave_func : lval option -> varinfo -> exp list -> Dom.t -> Dom.t -> Dom.t
  (** [leave_func lv f a x y] does postprocessing on the analyzed [enter_func lv f a x] output [y] -- usually readding some
     context from [x] *)

end


module VarF (LD: Printable.S) (GD: VarType)= 
struct
  type t = Local  of MyCFG.node * LD.t
         | Global of GD.t

  let hash x = 
    match x with
      | Local  (MyCFG.Statement s,d) -> Hashtbl.hash (d, s.sid, 0)
      | Local  (MyCFG.Function  f,d) -> Hashtbl.hash (d, f.vid, 1)
      | Global g -> GD.hash g

  let equal x1 x2 =
    match x1, x2 with
      | Global     g1, Global     g2 -> GD.equal g1 g2
      | Local (n1,d1), Local (n2,d2) -> MyCFG.Node.equal n1 n2 && LD.equal d1 d2
      | _ -> false
      
  let getLocation x =
    match x with
      | Local (n,d) -> MyCFG.getLoc n
      | Global g    -> Cil.locUnknown

  let pretty () x =
    match x with
      | Local (MyCFG.Statement s,d) -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
      | Local (MyCFG.Function  f,d) -> dprintf "call of %s" f.vname
      | Global g -> GD.pretty_trace () g
                
  let pretty_trace () x =
    match x with
      | Local  _ -> dprintf "%a on %a" pretty x Basetype.ProgLines.pretty (getLocation x)
      | Global g -> GD.pretty_trace () g
  
  let is_global x =
    match x with
      | Local  _ -> false
      | Global _ -> true
end

module VarCS =
struct
  type t = MyCFG.node * location

  let hash (n,l) = 
    match n with
      | MyCFG.Statement s -> Hashtbl.hash (l, s.sid, 0)
      | MyCFG.Function f -> Hashtbl.hash (l, f.vid, 1)

  let equal (n1,d1) (n2,d2) =
    MyCFG.Node.equal n1 n2 && compareLoc d1 d1 = 0

  let getLocation (n,d) = MyCFG.getLoc n

  let pretty () (n,d) =
    match n with
      | MyCFG.Statement s -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
      | MyCFG.Function f -> dprintf "call of %s" f.vname

  let pretty_trace () x = 
    dprintf "%a on %a" pretty x Basetype.ProgLines.pretty (getLocation x)
end

exception Deadcode


(** [Dom (D)] produces D lifted where bottpom means dead-code *)
module Dom (LD: Lattice.S) = 
struct 
  include Lattice.Lift (LD) (struct
                               let bot_name = "Dead code"
                               let top_name = "Totally unknown & messed up"
                             end)
  let lift x = `Lifted x

  let unlift x = 
    match x with
      | `Lifted x -> x
      | _ -> raise Deadcode

  let lifted f x = 
    match x with
      | `Lifted x -> `Lifted (f x)
      | tb -> tb
end


module ResultType (Spec: Spec) (LD: Printable.S with type t = Spec.Dom.t) (SD: Printable.S) = 
struct
  include Printable.Prod3 (LD) (SD) (Basetype.CilFundec)
  let isSimple _ = false
  let short w (es,x,f) = Spec.es_to_string f es
  let toXML (_,x,_ as st) = 
    let esc = Goblintutil.escape in
      match SD.toXML x with 
	| Xml.Element (tag, attr, children) ->
            Xml.Element (tag, [("text", esc (short 80 st))], children)
	| x -> x
  let pretty () (_,x,_) = SD.pretty () x
end

open Xml

module type ResultConf =
sig
  val result_name: string
end

module type RS = 
sig
  include Printable.S 
  include ResultConf
  type key = Basetype.ProgLinesFun.t
  type value
  val create: int -> t
  val clear: t -> unit
  val copy: t -> t
  val add: t -> key -> value -> unit
  val remove: t -> key -> unit
  val find: t -> key -> value
  val find_all: t -> key -> value list
  val replace : t -> key -> value -> unit
  val mem : t -> key -> bool
  val iter: (key -> value -> unit) -> t -> unit
  val fold: (key -> value -> 'b -> 'b) -> t -> 'b -> 'b
  val length: t -> int

  val resultXML: t -> Xml.xml
  val output: t -> unit
end

module Result (Range: Printable.S) (C: ResultConf) =
struct
  include Hash.Printable (Basetype.ProgLinesFun) (Range)
  include C

  let toXML x =
    let full_result = toXML x in
    let fatten_maps  (o:xml list) (x:xml) :xml list = 
      match x with 
	| Xml.Element (_,_,child) -> child @ o
	| z -> z::o in

    let group_loc_ch x = 
      match x with 
	| Xml.Element ("Loc",b,c) -> Xml.Element ("Loc",b,List.fold_left fatten_maps [] c)
	| z -> z in

    match full_result with 
      | Xml.Element (_,_,child) ->
          Xml.Element (result_name, [("name", "Who cares?")], 
		       List.map group_loc_ch child)
      | _ -> failwith "Empty analysis?"
 
  let resultXML x = toXML x

  let output table =
    let out = M.get_out result_name !GU.out in
    match !GU.result_style with
      | GU.Pretty -> ignore (fprintf out "%a\n" pretty table)
      | GU.Indented -> begin
          Xmldump.print_fmt out (resultXML table);
          output_char out '\n'
        end
      | GU.Compact -> begin
          Xmldump.print out (resultXML table);
          output_char out '\n'
        end
      | _ -> ()
end

