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
 
include Cil
include Pretty

module GU = Goblintutil
module JB = Json_type.Browse

(* Configurable domain with a flag.*)
module PreFlagDomain (Flag: ConcDomain.S) =
struct
  exception PartitionFlagDomainBroken
  
  (* This type should contain all analyses that do not depend on base.*)
  type e = Partition of Partition.Spec.Dom.t
         | Bad
  
  (* We pair list of configurable analyses with multithreadidness flag domain. *)
  type t = e list * Flag.t
  
  (* Constructor scheme stuff: we take a list of values and then filter out
     ones that are disabled. *)
  let int_ds = JB.make_table (JB.objekt (JB.field GU.conf "pre_cpa_analysis")) 
  let constr_scheme xs =
    let f (s,g) y : e list = 
      if JB.bool (JB.field int_ds s) 
      then (g ()) :: y
      else y
    in
    List.fold_right f xs []
    
  (* constructors *)
  let top' () = constr_scheme
    [("partition"   ,fun () -> Partition    (Partition.Spec.Dom.top ()))]
      
  let bot' () = constr_scheme
    [("partition"   ,fun () -> Partition    (Partition.Spec.Dom.bot ()))]
  
  (* element lattice functions *)
  
  let narrow' x y =
    match x, y with
      | Partition x, Partition y -> Partition (Partition.Spec.Dom.narrow x y)
      | _ -> raise PartitionFlagDomainBroken

  let widen' x y =
    match x, y with
      | Partition x, Partition y -> Partition (Partition.Spec.Dom.widen x y)
      | _ -> raise PartitionFlagDomainBroken

  let is_top' x =
    match x with
      | Partition x -> Partition.Spec.Dom.is_top x
      | _ -> raise PartitionFlagDomainBroken
  
  let is_bot' x =
    match x with
      | Partition x -> Partition.Spec.Dom.is_bot x
      | _ -> raise PartitionFlagDomainBroken

  let meet' x y =
    match x, y with
      | Partition x, Partition y -> Partition (Partition.Spec.Dom.meet x y)
      | _ -> raise PartitionFlagDomainBroken

  let join' x y =
    match x, y with
      | Partition x, Partition y -> Partition (Partition.Spec.Dom.join x y)
      | _ -> raise PartitionFlagDomainBroken

  let leq' x y =
    match x, y with
      | Partition x, Partition y -> Partition.Spec.Dom.leq x y
      | _ -> raise PartitionFlagDomainBroken
      
  let short' w x =
    match x with
      | Partition x -> Partition.Spec.Dom.short w x
      | _ -> raise PartitionFlagDomainBroken
      
  let toXML_f' sf x =
    match x with
      | Partition x -> Partition.Spec.Dom.toXML_f (fun w x -> sf w (Partition x)) x
      | _ -> raise PartitionFlagDomainBroken
      
  let pretty_f' sf () x =
    match x with
      | Partition x -> Partition.Spec.Dom.pretty_f (fun w x -> sf w (Partition x)) () x
      | _ -> raise PartitionFlagDomainBroken
      
  let toXML' x = toXML_f' short' x
      
  let pretty' x = pretty_f' short' x
      
  let isSimple' x =
    match x with
      | Partition x -> Partition.Spec.Dom.isSimple x
      | _ -> raise PartitionFlagDomainBroken

  let compare' x y =
    match x, y with
      | Partition x, Partition y -> Partition.Spec.Dom.compare x y
      | _ -> raise PartitionFlagDomainBroken

  let equal' x y =
    match x, y with
      | Partition x, Partition y -> Partition.Spec.Dom.equal x y
      | _ -> raise PartitionFlagDomainBroken

  let hash' x =
    match x with
      | Partition x-> Partition.Spec.Dom.hash x
      | _ -> raise PartitionFlagDomainBroken

  (* combining element functions to list (pair) functions *)
  
  let name () = "PreBase Domain"
  let narrow (x1,x2) (y1,y2) = List.map2 narrow' x1 y1, Flag.narrow x2 y2
  let widen  (x1,x2) (y1,y2) = List.map2 widen'  x1 y1, Flag.widen x2 y2
  let meet   (x1,x2) (y1,y2) = List.map2 meet'   x1 y1, Flag.meet x2 y2
  let join   (x1,x2) (y1,y2) = List.map2 join'   x1 y1, Flag.join x2 y2

  let top () = top' (), Flag.top () 
  let bot () = bot' (), Flag.bot ()
  let is_top (x,y) = List.for_all is_top' x && Flag.is_top y
  let is_bot (x,y) = List.for_all is_bot' x && Flag.is_top y
  let leq    (x1,x2) (y1,y2) = List.for_all2 leq' x1 y1 && Flag.leq x2 y2
    
  let short _ (x,y) = List.fold_left (fun p n -> p ^ short' 30 n ^ "; " ) (Flag.short 20 y ^ "; ") x
  
  let pretty_f _ () (x,z) = 
    match x with
      | [] -> text "()"
      | x :: [] -> pretty' () x
      | x :: y ->
        let first = pretty' () x in
        let rest  = List.fold_left (fun p n->p ++ text "," ++ pretty' () n) (text "") y in
        text "(" ++ Flag.pretty () z ++
        text "," ++ first ++ rest ++ text ")"

  let pretty () x = pretty_f short () x

  let toXML_f sf (x,y) =
    let esc = Goblintutil.escape in
    let nodes = Flag.toXML y :: List.map toXML' x in
    let node_leaf = if nodes = [] then "Leaf" else "Node" in
      Xml.Element (node_leaf, [("text", esc (sf Goblintutil.summary_length (x,y)))], nodes)

  let toXML = toXML_f short
  
  let compare (x1,x2) (y1,y2) =
    let f a x y =
      if a == 0 
      then compare' x y
      else 0
    in
    match List.fold_left2 f 0 x1 y1 with
      | 0 -> Flag.compare x2 y2
      | x -> x
    
  let isSimple (x,y) = List.for_all isSimple' x && Flag.isSimple y
  let equal (x1,x2) (y1,y2) = List.for_all2 equal' x1 y1 && Flag.equal x2 y2
  let hash     (x,y) = 
    List.fold_left (fun x y -> x lxor (hash' y)) 0 x 
    lxor
    Flag.hash y
end

(* Analysis that has PreFlagDomain as an domain. Currently we do not need 
   global state. leave_func, fork and eval_funvar are also currently not used.*)
module MakeSpec (Flag: ConcDomain.S) = 
struct
  module Dom  = PreFlagDomain (Flag)
  module Glob = 
  struct
    module Var = Basetype.Variables
    module Val = IntDomain.None
  end
  
  (* elementwise operations *)
  let exp_equal' e1 e2 g x =
    match x with
      | Dom.Partition x -> Partition.Spec.exp_equal e1 e2 g x
      | _ -> raise Dom.PartitionFlagDomainBroken
  
  let assign' lv exp g x =
    match x with
      | Dom.Partition x -> Dom.Partition (Partition.Spec.assign lv exp g x)
      | _ -> raise Dom.PartitionFlagDomainBroken

  (* analysis spec stuff *)
  let name = "PreBase analyses"
  let finalize () = ()
  let init     () = ()

  let otherstate = Dom.top ()
  let startstate = Dom.bot ()
  let should_join _ _ = true
  
  let es_to_string _ _ = "wat?"
  let get_diff   _ = []
  let reset_diff x = x  

  let get_fl (_,fl) = fl
  let set_fl (x,_) fl = (x,fl)

  (* probably not needed *) 
  let fork r v args g st = []
  let eval_funvar exp g st = []

  (* transfer functions *)
  let return r fn g   st = st
  let body fn g       st = st
  let branch exp tv g st = st
  let assign lv exp g (x,y) = List.map (assign' lv exp g) x, y

  let leave_func r v args g st1 st2  = st2
  let enter_func r v args g st = [st]
  
  let special_fn r v args g (s,f) =
    match v.vname with
      | "pthread_create" -> [(s,Flag.join f (Flag.get_main ()))]
      | _ -> [(s,f)]
      
  let exp_equal e1 e2 g (s,f) =
    let f s e =
      match s with
        | None -> exp_equal' e1 e2 g e
        | x -> x
    in
    List.fold_left f None s

end