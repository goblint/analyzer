open Cil
open Lval


type pth = Base  of (varinfo * (fieldinfo, int64) offs) option
         | Deref of pth * (fieldinfo, int64) offs
         | Star  of pth


module Path = 
struct
  type t = pth option

  include Lattice.StdCousot
  
  let name () = "Path"
  
  let rec equal' x y = 
    match x, y with
      | Base     b1, Base  b2    -> b1 = b2
      | Deref (x,o), Deref (y,i) -> o = i && equal' x y
      | Star x     , Star y      ->          equal' x y
      | _ -> false
 
  let rec equal x y =
    match x, y with
      | None  , None   -> true
      | Some x, Some y -> equal' x y
      | _ -> false
    
  let hash (x:t) = Hashtbl.hash x
  let compare (x:t) (y:t) = Pervasives.compare x y
  
  let isSimple _ = true
  let short n x =
    let rec offs_short x = 
      match x with 
        | `NoOffset -> ""
        | `Index (x,o) -> "[" ^ (Int64.to_string x) ^ "]" ^ (offs_short o) 
        | `Field (x,o) -> (x.fname) ^ (offs_short o) 
    in
    let rec to_str x =
      match x with 
        | Base (Some (x, o)) -> x.vname  ^ offs_short o
        | Base None          -> "*" 
        | Deref (x,o)        -> to_str x ^ "->" ^ offs_short o
        | Star x             -> to_str x ^ "->*"
    in
    match x with
      | None -> ".L"
      | Some x -> to_str x
   
  let pretty_f sf () x = Pretty.text (sf 80 x)
  let pretty = pretty_f short
  
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf 80 x)],[]) 
  let toXML = toXML_f short
  
  let why_not_leq () (x,y) = 
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
    
  let top ()   = Some (Base None)
  let is_top x = Some (Base None) = x
  
  let bot ()   = None 
  let is_bot x = None = x
  
  
  let rec leq' x y =
    match x, y with
      | Base (Some (x,o)), Base (Some (y,i)) -> x.vid=y.vid&&o=i  
      | _        , Base None -> true   
      | Base None, _         -> false
      | _        , Base _    -> false
      | Base (Some _), Deref (x,i) -> false
      | Base (Some x), Star y    -> leq' (Base (Some x)) y      
      | Deref (x,o)  , Deref (y,i) -> i=o && leq' x y
      | Star x       , Star y      -> List.exists (fun (x,y) -> leq' x y) [x,y;x,Star y]   
      | Star x       , Deref (y,i) -> false
      | Deref (x,o)  , Star y      -> List.exists (fun (x,y) -> leq' x y) [x,y;x,Star y]

  let leq x y =
    match x, y with
      | Some x, Some y -> leq' x y
      | None  , _      -> true
      | _     , None   -> false 
       
  let rec min x =
    match x with
      | [] -> failwith "incorrect use of PathDomain.min --- argument must be longer than one" 
      | [x] -> x
      | x::y::ys -> if leq' x y then min (x::ys) else min  (y::ys) 
  
  let join x y =
    let add_star x =
      match x with
        | Base None -> Base None
        | Star x -> Star x
        | x      -> Star x
    in
    let rec join' x y =
      match x, y with
        | Base (Some (x,o)), Base (Some (y,i)) 
          when o=i&&x.vid=y.vid -> Base (Some (x,o))
        | Base x, Deref (y,_) -> add_star (join' (Base x) y)
        | Deref (x,_), Base y -> add_star (join' x (Base y))
        | Base x, Star y -> add_star (join' (Base x) y)
        | Star x, Base y -> add_star (join' x (Base y))
        | Deref (x,o) , Deref (y,i) when i=o -> add_star (join' x y)
        | Deref (x,_), Star y -> add_star (min [join' x y; join' x (Star y)])
        | Star  x, Deref (y,_)-> add_star (min [join' x y; join' x (Star y)])
        | Star  x, Star y -> min [join' x y; join' x (Star y); join' (Star x) y]
        | _ -> Base None
    in
    let r = 
    match x, y with
      | Some x, Some y -> Some (join' x y)
      | None  , y      -> y
      | x     , None   -> x
    in
    (*Messages.report (short 80 x ^ " `join` " ^ short 80 y ^ " = " ^ short 80 r ); *)r

  
  let meet' x y = failwith "PathDomain not implemented"

  let meet x y =
    match x, y with
      | Some x, Some y -> Some (meet' x y)
      | _ -> None
      
  let from_lval (lv:lval) : t =
    let rec offs_from_cil o =
      match o with
        | NoOffset    -> `NoOffset
        | Field (f,o) -> `Field (f,offs_from_cil o)
        | Index (i,o) -> `Index (0L, offs_from_cil o)
    in 
    match lv with
      | Var x, o -> Some (Base (Some (x,offs_from_cil o)))
      | Mem e, o -> bot ()
  
  (*todo: deal with offsets*)
  let rec subst_base_var x (v,o) y =
    let rec subst_base_var' x y =
      match x with
        | Base (Some (v',o')) when v.vid = v'.vid && o = o' -> y  
        | Base x -> Base x
        | Deref (x,o) -> Deref (subst_base_var' x y, o)
        | Star x -> Star (subst_base_var' x y)      
    in
    match x, y with
      | Some x, Some y -> Some (subst_base_var' x y) 
      | _ -> None 

  
  let rec from_exp (e:exp) : t =
    let rec offs_from_cil o =
      match o with
        | NoOffset    -> `NoOffset
        | Field (f,o) -> `Field (f,offs_from_cil o)
        | Index (i,o) -> `Index (0L, offs_from_cil o)
    in 
    match e with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.AlignOfE _
      | Cil.UnOp  _    
      | Cil.BinOp _ 
      | Cil.Const _ -> top ()
      | Cil.AddrOf  (Cil.Var v,o) 
      | Cil.StartOf (Cil.Var v,o) 
      | Cil.Lval    (Cil.Var v,o) -> Some (Base (Some (v,offs_from_cil o)))
      | Cil.AddrOf  (Cil.Mem e,o) 
      | Cil.StartOf (Cil.Mem e,o) 
      | Cil.Lval    (Cil.Mem e,o) ->
          begin match from_exp e with
            | None -> Some (Deref (Base None,offs_from_cil o))
            | Some x -> Some (Deref (x,offs_from_cil o))
          end
      | Cil.CastE (_,e)           -> from_exp e 
      end


module Access =
struct 
  module Lvals = Lval.Normal (IntDomain.Integers)
  include MapDomain.MapTop (Lval.Normal (IntDomain.Integers)) (Path)

  let toXML_f sf x = 
    match toXML x with
      | Xml.Element (_, _, [Xml.Element (node,_,elems)]) -> Xml.Element (node, ["text", "Paths"], elems)
      | Xml.Element (node, _, []) -> Xml.Element (node, ["text", "Paths (empty)"], [])
      | x -> x
      
  let toXML s = toXML_f short s 
  
  (*todo: kill when prefix matches*)
  let kill v mp =
    remove v mp
  
  let kill_tops mp =
    let rem_top k v m =
      if Path.is_top v
      then m
      else add k v m
    in
    fold rem_top mp (top ())
  
  let join x y = kill_tops (join x y)
  
  let assign (lhs:lval) (rhs:exp) (mp:t) =
    let rec offs_from_cil o =
      match o with
        | NoOffset    -> `NoOffset
        | Field (f,o) -> `Field (f,offs_from_cil o)
        | Index (i,o) -> `Index (0L, offs_from_cil o)
    in 
    let get_val lv mp : pth option =
      try find lv mp
      with Not_found -> Path.top ()
    in
    match lhs with
      | (Var v,o) when v.vglob -> 
        let rlv = (v, offs_from_cil o) in
        let lv = Lvals.from_var_offset rlv in 
        kill lv mp
      | (Var v,o) -> 
        begin
          let rlv = (v, offs_from_cil o) in
          let lv = Lvals.from_var_offset rlv in 
          let new_val = Path.subst_base_var (Path.from_exp rhs) rlv (get_val lv mp) in
          if Path.is_top new_val
          then kill lv mp
          else add lv new_val mp          
        end
      | _ -> 
        (*todo: take more care in this case so it would not go to top all the time  *)
        top ()
    
end
