type t' = Val of int | Bot
type t = t' * t' * t'* t'

include Printable.Blank
include Lattice.StdCousot

(* lowest priority obtained over:
1st component = critical region (between first and last variable access)
2nd component = first access till end of function
3rd component = beginning of function till last variable access
4th component = over whole function
*)


let name () = "Transactionality tupels"

let is_bot_c x = (x = Bot)

let hash (a,b,c,d) = 
  let a' = match a with Bot -> -1 | Val a'' -> a'' in
  let b' = match b with Bot -> -1 | Val b'' -> b'' in
  let c' = match c with Bot -> -1 | Val c'' -> c'' in
  let d' = match d with Bot -> -1 | Val d'' -> d'' in
    a' lxor b' lxor c' lxor d'

let equal (a1,a2,a3,a4) (b1,b2,b3,b4) = a1=b1&&a2=b2&&a3=b3&&a4=b4

let copy x = x
let top () = (Val 0, Val 0, Val 0, Val 0)
let is_top x = (x = top())
let bot () = (Bot, Bot, Bot, Bot)
let is_bot x = (x = bot())
let isSimple _  = true

let short _ (a,b,c,d) = 
  let a' = match a with Bot -> "bot" | Val a'' -> string_of_int a'' in
  let b' = match b with Bot -> "bot" | Val b'' -> string_of_int b'' in
  let c' = match c with Bot -> "bot" | Val c'' -> string_of_int c'' in
  let d' = match d with Bot -> "bot" | Val d'' -> string_of_int d'' in
    "("^a'^", "^b'^", "^c'^", "^d'^")"

let pretty_f _ _ x = Pretty.text (short 0 x)
let toXML_f _ x = Xml.Element ("Leaf", [("text", short 0 x)],[])
let toXML m = toXML_f short m
let pretty () x = pretty_f short () x

let pretty_diff () (x,y) = Pretty.dprintf "%a instead of %a" pretty x pretty y

(* include Printable.PrintSimple (struct type t' = t let short = short let name = name end) *)
(* let pretty () x = Pretty.nil *)

let min_t' a b = match (a,b) with
  | (Bot,x) | (x, Bot) -> x
  | (Val a',Val b')  -> Val (min a' b')

let max_t' a b = match (a,b) with
  | (Bot,x) | (x, Bot) -> Bot
  | (Val a',Val b')  -> Val (max a' b')

let leq_t' a b = match (a,b) with
  | (Bot,_)  -> true
  | (_, Bot) -> false
  | (Val a',Val b')  -> a' >= b'


let leq (a1,a2,a3,a4) (b1,b2,b3,b4) = leq_t' a1 b1 && leq_t' a2 b2 && leq_t' a3 b3 && leq_t' a4 b4

let join (a1,a2,a3,a4) (b1,b2,b3,b4) = (min_t' a1 b1 ,min_t' a2 b2 ,min_t' a3 b3 ,min_t' a4 b4 )
let meet (a1,a2,a3,a4) (b1,b2,b3,b4) = (max_t' a1 b1 ,max_t' a2 b2 ,max_t' a3 b3 ,max_t' a4 b4 )

(* composition operator  (b \fcon a) *)
let fcon (a1,a2,a3,a4) (b1,b2,b3,b4) =  match (a2,b2) with
    | (Bot,Bot) -> (a1,           a2,           a3,           min_t' a4 b4 )
    | (Bot,_)  	-> (b1,           b2,           min_t' a4 b3 ,min_t' a4 b4 )
    | (_,Bot)  	-> (a1,        	  min_t' a2 b4 ,a3,           min_t' a4 b4 )
    | _       	-> (min_t' a2 b3 ,min_t' a2 b4 ,min_t' a4 b3 ,min_t' a4 b4 )
