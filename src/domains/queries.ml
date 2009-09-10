include Cil
include Pretty 

module ID = IntDomain.Integers
module LS = SetDomain.ToppedSet (Lval.CilLval) (struct let topname = "All" end)

type t = ExpEq of exp * exp
       | MayPointTo of exp
       | ReachableFrom of exp
       | TheAnswerToLifeUniverseAndEverything

      
module Result: Lattice.S with type t = [
    | `Top
    | `Int of ID.t
    | `LvalSet of LS.t
    | `Bot
    ] = 
struct 
  type t = [
    | `Top
    | `Int of ID.t
    | `LvalSet of LS.t
    | `Bot
    ]

  let name () = "query result domain"

  let bot () = `Bot
  let is_bot x = x = `Bot
  let bot_name = "Bottom"
  let top () = `Top
  let is_top x = x = `Top
  let top_name = "Unknown"

  let equal x y = 
    match (x, y) with
      | (`Top, `Top) -> true
      | (`Bot, `Bot) -> true
      | (`Int x, `Int y) -> ID.equal x y
      | (`LvalSet x, `LvalSet y) -> LS.equal x y
      | _ -> false

  let hash (x:t) =
    match x with
      | `Int n -> ID.hash n
      | `LvalSet n -> LS.hash n
      | _ -> Hashtbl.hash x

  let compare x y = 
    let constr_to_int x = match x with
        | `Bot -> 0
        | `Int _ -> 1
        | `LvalSet _ -> 3
        | `Top -> 100
    in match x,y with
      | `Int x, `Int y -> ID.compare x y
      | `LvalSet x, `LvalSet y -> LS.compare x y
      | _ -> Pervasives.compare (constr_to_int x) (constr_to_int y)

  let pretty_f _ () state = 
    match state with
      | `Int n ->  ID.pretty () n
      | `LvalSet n ->  LS.pretty () n
      | `Bot -> text bot_name
      | `Top -> text top_name

  let short w state = 
    match state with
      | `Int n ->  ID.short w n
      | `LvalSet n ->  LS.short w n
      | `Bot -> bot_name
      | `Top -> top_name

  let rec isSimple x = 
    match x with
      | `Int n ->  ID.isSimple n
      | `LvalSet n ->  LS.isSimple n
      | _ -> true

  let toXML_f _ state =
    match state with
      | `Int n -> ID.toXML n
      | `LvalSet n -> LS.toXML n
      | `Bot -> Xml.Element ("Leaf", ["text",bot_name], [])
      | `Top -> Xml.Element ("Leaf", ["text",top_name], [])

  let pretty () x = pretty_f short () x
  let toXML s = toXML_f short s

  let leq x y =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Int x, `Int y) -> ID.leq x y
      | (`LvalSet x, `LvalSet y) -> LS.leq x y
      | _ -> false

  let join x y = 
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.join x y)
      | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.join x y)
      | _ -> `Top

  let meet x y = 
    match (x,y) with 
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Int x, `Int y) -> `Int (ID.meet x y)
      | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.meet x y)
      | _ -> `Bot

  let widen x y =
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.widen x y)
      | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.widen x y)
      | _ -> `Top    

  let narrow x y =
    match (x,y) with 
      | (`Int x, `Int y) -> `Int (ID.narrow x y)
      | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.narrow x y)
      | (x,_) -> x
end