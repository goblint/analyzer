module     Eq = IntDomain.MakeBooleans (struct let truename="==" let falsename="!=" end)
module Method = IntDomain.MakeBooleans (struct let truename="guard" let falsename="assign" end)

module P = 
struct 
  include Lattice.Fake (Printable.Prod3 (Method) (Eq) (Basetype.CilExp))
  let short w (m,b,e) = Method.short 1 m ^"ed "^ Eq.short 1 b ^ " " ^ Basetype.CilExp.short w e
  let toXML = toXML_f short
end

module Dom = 
struct
  include MapDomain.MapTop_LiftBot (Basetype.Variables) (P)
  
  let find k x = if mem k x then find k x else raise Not_found
  
  let join x y = 
    let f _ x y =
      match x, y with
        | Some x, Some y when P.equal x y -> Some x
        | _ -> None 
    in
      if is_bot x then y else if is_bot y then x else merge f x y
  
  exception Bot
  let meet x y = 
    let f _ x y =
      match x, y with
        | Some x, Some y when not (P.equal x y) -> raise Bot
        | Some x, _ | _, Some x -> Some x
        | _ -> None 
    in
      if is_bot x || is_bot y then x else try merge f x y with Bot -> bot ()
      
  let leq x y = 
    if is_top y || is_bot x then true else if is_bot y || is_top x then false else
      for_all (fun k v -> try P.equal v (find k y) with Not_found -> false) x

  let toXML_f s x = 
    match toXML_f s x with
      | Xml.Element ("Node",_,[]) ->  Xml.Element ("Leaf",["text","Flag Modes"],[])
      | Xml.Element ("Node",_,xs) ->  Xml.Element ("Node",["text","Flag Modes";"id","map"],xs)
      | x -> x 
  let toXML = toXML_f short 
end