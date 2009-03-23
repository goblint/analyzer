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

module Make (Domain: Hashtbl.HashedType) =
struct
  module H = Hashtbl.Make(Domain)
  type key = Domain.t
  type 'a t = 'a H.t * 'a 

  let create size def = (H.create size, def)
  let find (map,def) key = try H.find map key with Not_found -> def
  let find_all (map,def) key = H.find_all map key @ [def]

  let copy (map,def) = (H.copy map, def)  (* NB! maybe default should be copied? *)

   (* and this is inheritance???   *)
  let lift f (map,_) = f map
  let clear x = lift H.clear x
  let add x = lift H.add x
  let remove x = lift H.remove x
  let replace x = lift H.replace x
  let mem x = lift H.mem x (* or const true??? *)
  let iter f = lift (H.iter f)
  let fold f = lift (H.fold f)
  let length x = lift H.length x
end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a -> 'a t
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
  end

module type SP =
sig
  include Printable.S
  type key
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
end

module Printable (Domain: Printable.S) (Range: Printable.S) = 
struct
  include Printable.Std
  module M = Hashtbl.Make (Domain)
  (* How can I just include this one and set the type 'a t = Range.t M.t???
   * I will just include them manually for now! *)
  type t = Range.t M.t
  type key = Domain.t
  type value = Range.t
  let create = M.create
  let clear = M.clear
  let find = M.find
  let find_all = M.find_all
  let copy = M.copy
  let add = M.add 
  let remove = M.remove
  let replace = M.replace
  let mem = M.mem
  let iter = M.iter
  let fold = M.fold
  let length = M.length


  let short _ x = "mapping"
  let isSimple _ = false

  let toXML_f sf mapping =
    let f (key,st) = 
      match Domain.toXML key with
        | Xml.Element ("Loc",attr,[]) ->
            Xml.Element ("Loc", attr, [Range.toXML st])
        | Xml.Element ("Leaf",attr,[]) ->
	    let summary = 
	      let w = Goblintutil.summary_length - 4 in
	      let key_str = ref "" in
	      let st_str  = ref "" in
		key_str := Domain.short w key;
		st_str  := Range.short (w - String.length !key_str) st;
		!key_str ^ " -> " ^ !st_str in

            let attr = [("text", summary)] in begin
              match Range.toXML st with
                | Xml.Element (_, chattr, children) -> 
                    if Range.isSimple st then Xml.Element ("Leaf", attr, [])
                    else Xml.Element ("Node", attr, children)
                | x -> x
            end
        | _ -> Xml.Element ("Node", [("text","map")], [Domain.toXML key; Range.toXML st])
    in
    let assoclist = fold (fun x y rest -> (x,y)::rest) mapping [] in
      (* let default = Xml.Element ("Default", [], [Range.toXML defval]) in *)
    let children = List.rev_map f assoclist in
      Xml.Element ("Node", [("text", sf Goblintutil.summary_length mapping)], children)

  open Pretty
  let pretty_f _ () mapping = 
    let f key st dok = 
      dok ++ (if Range.isSimple st then dprintf "%a -> @[%a@]\n" else 
        dprintf "%a -> \n  @[%a@]\n") Domain.pretty key Range.pretty st 
    in
    let content () = fold f mapping nil in
    let defline () = dprintf "OTHERS -> Not available\n" in
      dprintf "@[Mapping {\n  @[%t%t@]}@]" content defline

  let pretty () x = pretty_f short () x
  let toXML m = toXML_f short m
end

