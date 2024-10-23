(** Simplest possible analysis with unit domain ([unit]). *)

open GoblintCil
open Analyses


module Bitfield= struct
 module I = IntDomain.Flattened  

  type t = I.t * I.t

(* abstract operators from the paper *)

  let of_int (z:Z.t) : t = (I.lognot @@ I.of_int (Z.to_int64 z), I.of_int (Z.to_int64 z))

  let logneg (p:t) :t = let (z,o) = p in (o,z)

  let logand (p1:t) (p2:t) :t = let (z1,o1) = p1 in let (z2,o2) = p2 in (I.logor z1 z2, I.logand o1 o2)

    let logor (p1:t) (p2:t) :t = let (z1,o1) = p1 in let (z2,o2) = p2 in (I.logand z1 z2, I.logor o1 o2)

    let logxor (p1:t) (p2:t) :t = let (z1,o1) = p1 in let (z2,o2) = p2 in (I.logor (I.logand z1 (I.lognot o2)) (I.logand (I.lognot o1) o2), I.logor (I.logand o1 (I.lognot o2)) (I.logand (I.lognot o1) o2))

    let logshiftleft (p1:t) (p2:t) :t =  failwith "Not implemented"

    let logshiftright (p1:t) (p2:t) :t =  failwith "Not implemented"


    let join (z1,o1) (z2,o2) =
      (I.logor z1 z2, I.logor o1 o2)
  
    let meet (z1,o1) (z2,o2) = let nabla x y= (if x = I.logor x y then y else  (I.of_int (Z.to_int64 (Z.minus_one) ))) in 
      (nabla z1 z2, nabla o1 o2)
  
      (* todo wrap *)


  let equal (z1,o1) (z2,o2) = z1 = z2 && o1 = o2
  let hash (z,o) = I.hash z + 31 * I.hash o
  let compare (z1,o1) (z2,o2) =
    match compare z1 z2 with
    | 0 -> compare o1 o2
    | c -> c

  let show (z,o) = Printf.sprintf "Bitfield{z:%s,o:%s}" (I.show z) (I.show o)

  let pretty () (z,o) = Pretty.dprintf "Bitfield{z:%s,o:%s}" (I.show z) (I.show o)
  let printXml out(z,o) = BatPrintf.fprintf out "<Bitfield><z>%a</z><o>%a</o></Bitfield>" I.printXml z I.printXml o

  let name () = "Bitfield"

  let to_yojson (z,o) = I.to_yojson z (*TODO*)


  let tag (z,o) = Hashtbl.hash (z,o)
  let arbitrary () = QCheck.pair (I.arbitrary ()) (I.arbitrary ())
  let relift x = x

  let leq (z1,o1) (z2,o2) = I.leq z1 z2  && I.leq o1 o2


  let widen (z1,o1) (z2,o2) = if I.leq z1 z2 && I.leq o1 o2 then (z2, o2) else (I.top (), I.top ())

  let narrow = meet

  let pretty_diff () ((z1,o1),(z2,o2)) =
    Pretty.dprintf "Bitfield: (%s,%s) not leq (%s,%s)" (I.show z1) (I.show o1) (I.show z2) (I.show o2)



  let top () : t = (I.of_int (Z.to_int64 (Z.minus_one)), I.of_int (Z.to_int64 (Z.minus_one)))
  let bot () : t =  (I.of_int (Z.to_int64 Z.zero), I.of_int (Z.to_int64 Z.zero))
  let is_top (e:t) = e = top ()
  let is_bot (e:t) = e = bot ()
end



(* module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Printable.Unit and type marshal = unit = *)
(* No signature so others can override module G *)
module Spec =
struct
  include Analyses.DefaultSpec

  module B = Bitfield

  let name () = "bitfield"
  module D = MapDomain.MapBot (Basetype.Variables) (B)
  include Analyses.ValueContexts(D)



  let is_integer_var (v: varinfo) =
    match v.vtype with
      | TInt _ -> true
      | _ -> false


  let get_local = function
  | Var v, NoOffset when is_integer_var v && not (v.vglob || v.vaddrof) -> Some v (* local integer variable whose address is never taken *)
  | _, _ -> None

  let rec eval (state : D.t) (e: exp) =
    match e with
    | Const c -> (match c with
        | CInt (i,_,_) ->
          (try B.of_int i with Z.Overflow -> B.top ())
        (* Our underlying int domain here can not deal with values that do not fit into int64 *)
        (* Use Z.to_int64 instead of Cilint.int64_of_cilint to get exception instead of silent wrap-around *)
        | _ -> B.top ()
        

      
    )
    | Lval (Var x, NoOffset) when is_integer_var x && not (x.vglob || x.vaddrof) -> 
        (try D.find x state with Not_found -> B.top ())
    | _ -> B.top ()


    (* Map of integers variables to our signs lattice. *)
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    print_endline "assign";

      let d = ctx.local in
      match lval with
      | (Var x, NoOffset) -> 
          (* Convert the raw tuple to a proper Bitfield.t value *)
          let v = eval d rval in
          D.add x v d
      | _ -> d

      let branch ctx (exp:exp) (tv:bool) : D.t =
        print_endline "branch";
        let d = ctx.local in
        match exp with
        | BinOp (Eq, e1, e2, _) -> 
        (match e1, e2 with
          | Lval (Var x, NoOffset), Const (CInt (i,_,_)) when is_integer_var x && not (x.vglob || x.vaddrof) ->
            let v = eval d e2 in
            if tv then 
             D.add x v d  else 
              D.add x (B.logneg v) d
          | _ -> d
        ) 

        | _ -> d
    

  let body ctx (f:fundec) : D.t =
    print_endline "body";
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    print_endline "return";
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    print_endline "enter";
    [ctx.local, ctx.local]

  
  let assert_holds (d: D.t) (e:exp) = 
      print_endline "assert_holds";
    match e with
  | BinOp (Eq, e1, e2, _) ->
    (match e1, e2 with
      | BinOp (BAnd, a,b,_), Const (CInt (i,_,_))  ->
        let pl=eval d a in
        let pr=eval d b in
        let and_result=B.logand pl pr in
        B.equal and_result (B.of_int i)
      | _ -> false
    )
| _ -> false


let query ctx (type a) (q: a Queries.t): a Queries.result =
  print_endline "query";
  let open Queries in
  match q with
  | EvalInt e when assert_holds ctx.local e ->
    let ik = Cilfacade.get_ikind_exp e in
    ID.of_bool ik true
  | _ -> Result.top q


  let combine_env ctx lval fexp f args fc au f_ask =
    print_endline "combine_env";
    au

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    print_endline "combine_assign";
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let d = ctx.local in
    match lval with
    | Some (Var x, NoOffset) -> D.add x( B.top ()) d
    | _ -> d



  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.top ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
