module     Eq = IntDomain.MakeBooleans (struct let truename="==" let falsename="!=" end)
module Method = IntDomain.MakeBooleans (struct let truename="guard" let falsename="assign" end)

module L_names =
struct
  let bot_name = "unreachable"
  let top_name = "unknown"
end

module P =
struct
  include Lattice.Flat (Printable.Prod3 (Method) (Eq) (IntDomain.FlatPureIntegers)) (L_names)
  let short w x = match x with
    | `Lifted (m,b,e) -> Method.short 1 m ^"ed "^ Eq.short 1 b ^ " " ^ IntDomain.FlatPureIntegers.short w e
    | `Top -> top_name
    | `Bot -> bot_name

  let join x y = match x,y with
    | `Bot , z | z , `Bot -> z
    | `Lifted (false,_,c1),`Lifted (false,_,c2) when c1=c2 -> y
    | `Lifted (true,false,c1),`Lifted (true,false,c2) when c1=c2 -> y
    | `Lifted (true,true,c1),`Lifted (true, true, c2) when c1=c2 -> y
    | `Lifted (true,true,c1),`Lifted (true, false, c2) when not(c1=c2) -> y
    | `Lifted (true,false,c1),`Lifted (true, true, c2) when not(c1=c2) -> x
    | _ -> `Top


  let leq (x:t) (y:t) = match x,y with
    | `Bot , _  -> true
    | _ , `Top -> true
    | _, `Bot -> false
    | `Top ,_ -> false
    | `Lifted (false,_,c1), `Lifted (false,_,c2) -> c1=c2
    | _, `Lifted (false,_,_) ->  false
    | `Lifted (false,_,_), _ ->  true
    | `Lifted (true,true,c1),`Lifted (true, true, c2)  -> c1=c2
    | _, `Lifted (true,true,_) -> false
    | `Lifted (true, true, _), _ -> true
    | `Lifted (true,false,c1),`Lifted (true,false,c2) -> c1=c2
    (*    | _, `Lifted (true,false,c1) -> false
          | `Lifted (true,false,_), _ -> true    *)
    (*     | _ -> false *)
end

module Dom =
struct
  include MapDomain.MapTop_LiftBot (Basetype.Variables) (P)

  (*   let find k x = if mem k x then find k x else P.top() *)
end
