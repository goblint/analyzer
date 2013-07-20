open Cil

module GU = Goblintutil

module type S =
sig
  include Lattice.S
  type elem
    
  val add        : elem -> t -> t
  val add_tail   : elem -> t -> t
  
  val del        : elem -> t -> t 
  
  val move       : t -> t -> t * t 
  val move_tail  : t -> t -> t * t
  
  val list_empty : t -> bool option
  
  val splice     : t -> t -> t
  
  val entry      : t -> elem
  val entry_tail : t -> elem
  val entry_rand : t -> elem
end

let list_poison = makeGlobalVar "LIST_POISON" voidType 

module SimpleList (Base: SetDomain.S)
  : S with type elem = Base.t =
struct
  include Base
  type elem = Base.t    
  
  let add      = join
  let add_tail = join
  
  let del      v x = x (*Base.diff x v*)
  
  let move      a b = let x = join a b in x, x
  let move_tail a b = let x = join a b in x, x

  let list_empty x = if Base.is_bot x then Some true else None
  
  let splice = join
  
  let entry      x = x
  let entry_tail x = x
  let entry_rand x = x
  
  let short w x =
    if is_bot x
    then "Empty List"
    else "List: " ^ short w x
  
  let pretty () x = pretty_f short () x
end
