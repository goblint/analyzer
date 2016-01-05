open Batteries
open Cil
open Pretty

module Ident : Printable.S with type t = string = 
struct
  open Pretty
  type t = string
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let compare (x:t) (y:t) = compare x y
  let isSimple _ = true
  let short _ x = x
  let toXML_f sf x =
    let esc = Goblintutil.escape in
    Xml.Element ("Leaf", ["text", esc (sf 80 x)], [])
  let pretty_f sf () x = text (sf 80 x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "strings"
  let pretty_diff () (x,y) = 
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = 
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" 
      (Goblintutil.escape (short 80 x))
end

module LabeledString = 
struct 
  include Printable.Prod (Ident) (Ident)
  let pretty_f sf () (x,y) =
    Pretty.text (sf Goblintutil.summary_length (x,y))
  let short _ (x,y) = x^":"^y
  let pretty () x = pretty_f short () x
  let toXML m = toXML_f short m
end
module LSSet = SetDomain.Make (LabeledString)
module LSSSet = SetDomain.Make (LSSet)

let typeIncl = Hashtbl.create 101
let init (f:file) = 
  let visit_field fi = 
    Hashtbl.add typeIncl fi.ftype fi
  in
  let visit_glob = function
    | GCompTag (c,_) -> 
      List.iter visit_field c.cfields
    | _ -> ()
  in
  List.iter visit_glob f.globals

(* from cil *)
let rec compareOffset (off1: offset) (off2: offset) : bool =
  match off1, off2 with
  | Field (fld1, off1'), Field (fld2, off2') ->
    fld1 == fld2 && compareOffset off1' off2'
  | Index (e1, off1'), Index (e2, off2') ->
    Expcompare.compareExp e1 e2 && compareOffset off1' off2'
  | NoOffset, NoOffset -> true
  | _ -> false

type acc_typ = [ `Type of typ | `Struct of compinfo * offset ]

let d_acct () = function
  | `Type t -> dprintf "(%a)" d_type t
  | `Struct (s,o) -> d_offset (dprintf "(struct %s)" s.cname) () o

let file_re = Str.regexp "\\(.*/\\|\\)\\([^/]*\\)"
let d_loc () loc = 
  if Str.string_match file_re loc.file 0 then
    dprintf "%s:%d" (Str.matched_group 2 loc.file) loc.line
  else
    dprintf "%s:%d" loc.file loc.line

let d_memo () (t, lv) =
  match lv with 
  | Some (v,o) -> 
    let var = dprintf "%s" v.vname in
    dprintf "%a" (d_offset var) o
  | None -> dprintf "%a" d_acct t

let rec get_type (fb: typ) : exp -> acc_typ = function
  | AddrOf (h,o) | StartOf (h,o) -> 
    let rec f htyp = 
      match htyp with
      | TComp (ci,_) -> `Struct (ci,o)
      | TNamed (ti,_) -> f ti.ttype
      | _ -> `Type fb
    in
    begin match o with
      | Field (f, on) -> `Struct (f.fcomp, o)
      | NoOffset | Index _ ->
        begin match h with
          | Var v -> f (v.vtype)
          | Mem e -> f fb
        end
    end
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ | AddrOfLabel _  -> 
    `Type (uintType)
  | UnOp (_,_,t) -> `Type t
  | BinOp (_,_,_,t) -> `Type t
  | CastE (t,e) ->
    begin match get_type fb e with
      | `Struct s -> `Struct s
      | _         -> `Type t
    end
  | Question (_,b,c,t) -> 
    begin match get_type fb b, get_type fb c with
      | `Struct (s1,o1), `Struct (s2,o2) 
        when s1.ckey = s2.ckey && compareOffset o1 o2 ->
        `Struct (s1, o1)
      | _ -> `Type t
    end
  | _ -> `Type fb

let get_type fb e = 
  (* printf "e = %a\n" d_plainexp e; *)
  let r = get_type fb e in
  (* printf "result = %a\n" d_acct r; *)
  match r with
  | `Type (TPtr (t,a)) -> `Type t
  | x -> x

module Ht = 
struct
  include Hashtbl

  let find_def (ht:('a,'b) Hashtbl.t) (k:'a) (z:'b Lazy.t ) : 'b = 
    try 
      find ht k 
    with Not_found ->
      let v = Lazy.force z in
      add ht k v;
      v

  let modify_def (ht:('a,'b) Hashtbl.t) (k:'a) (z:'b Lazy.t) (f: 'b -> 'b): unit =
    let g = function
      | None -> Some (f (Lazy.force z))
      | Some b -> Some (f b)
    in
    modify_opt k g ht

end

(* type -> lval option -> part -> (2^(write, loc, e, locks), locks_union) *)
let accs = Hashtbl.create 100

type var_o = varinfo option
type off_o = offset  option
type part  = LSSSet.t * LSSet.t 

let get_val_type e (vo: var_o) (oo: off_o) : acc_typ =
  try (* FIXME: Cil's typeOf fails on our fake variables: (struct s).data *)
    match vo, oo with
    | Some v, Some o -> get_type (typeOf e) (AddrOf (Var v, o))
    | Some v, None -> get_type (typeOf e) (AddrOf (Var v, NoOffset))
    | _ -> get_type (typeOf e) e
  with _ -> get_type voidType e

let add_one (e:exp) (w:bool) (ty:acc_typ) lv ((pp,lp):part): unit =
  if (LSSSet.is_empty pp) then () else begin
    let tyh = Ht.find_def accs  ty (lazy (Ht.create 10)) in
    let lvh = Ht.find_def tyh lv (lazy (Ht.create 10)) in
    let add_part ls =
      let loc = !Tracing.current_loc in
      Ht.modify_def lvh ls (lazy (Set.empty,lp)) (fun (s,o_lp) ->
          (Set.add (w,loc,e,lp) s, LSSet.inter lp o_lp)
        )
    in
    LSSSet.iter add_part pp
  end

let type_from_type_offset : acc_typ -> typ = function
  | `Type t -> t
  | `Struct (s,o) ->
    let rec deref t = 
      match unrollType t with
      | TPtr (t,_) -> t  (*?*)
      | TArray (t,_,_) -> t
      | _ -> failwith "type_from_type_offset: indexing non-pointer type"
    in
    let rec type_from_offs (t,o) = 
      match o with
      | NoOffset -> t
      | Index (i,os) -> type_from_offs (deref t, os)
      | Field (f,os) -> type_from_offs (f.ftype, os)
    in
    unrollType (type_from_offs (TComp (s, []), o))

let rec add_propagate e w ty ls p =
  add_one e w ty None p;
  let t = type_from_type_offset ty in
  let incl = Ht.find_all typeIncl t in
  let add_field fi = add_one e w (`Struct (fi.fcomp, Field (fi,NoOffset))) None p in
  List.iter add_field incl

let add e w vo oo p =
  let ty = get_val_type e vo oo in
  match vo, oo with
  | Some v, Some o -> add_one       e w ty (Some (v, o)) p
  | _              -> add_propagate e w ty None          p

let partition_race ps (accs,ls) =
  let write (w,loc,e,lp) = w in
  LSSet.is_empty ls && Set.exists write accs

let only_read ps (accs,ls) =
  let read (w,loc,e,lp) = not w in
  Set.for_all read accs

let common_resource ps (accs,ls) = not (LSSet.is_empty ls)

(* let is_data_race k =
   let ht_exists f ht = Hashtbl.fold (fun k v z -> z || f k v) ht false in
   if not (Hashtbl.mem accs k) then
    false
   else begin
    let ht = Hashtbl.find accs k in
    ht_exists partition_race ht
   end *)

let print_races () =
  let only_rd = ref 0 in
  let common_res = ref 0 in
  let race = ref 0 in
  let g ls (accs,lp) =
    let reason = 
      if only_read ls (accs,lp) then begin
        incr only_rd;
        "only read" 
      end else if common_resource ls (accs,lp) then begin
        incr common_res;
        "common resource" 
      end else begin
        incr race;
        "race" 
      end
    in
    ignore (Pretty.printf "  %a -> %a (%s)\n" LSSet.pretty ls LSSet.pretty lp reason)
  in
  let h ty lv =
    ignore(printf "Memory location %a\n" d_memo (ty,lv));
    Hashtbl.iter g 
  in
  let f ty = Hashtbl.iter (h ty) in
  Hashtbl.iter f accs;
  ignore (Pretty.printf "\tonly read:       %d\n" !only_rd);
  ignore (Pretty.printf "\tcommon resource: %d\n" !common_res);
  ignore (Pretty.printf "\tno protection:   %d\n" !race);
  ignore (Pretty.printf "\t-----------------------\n");
  ignore (Pretty.printf "\ttotal:           %d\n" ((!race) + (!common_res) + (!only_rd)))

let print_accesses () =
  let g ls (acs,_) =
    let h (w,loc,e,lp) =
      let atyp = if w then "write" else "read" in
      ignore (printf "  %s@@%a %a -> %a\n" atyp d_loc loc 
                LSSet.pretty ls LSSet.pretty lp)
    in
    Set.iter h acs
  in
  let h ty lv ht = 
    ignore(printf "Memory location %a\n" d_memo (ty,lv));
    Hashtbl.iter g ht
  in
  let f ty ht = 
    Hashtbl.iter (h ty) ht
  in
  Hashtbl.iter f accs

let print_result () = 
  print_races ();
  ignore (printf "--------------------\n");
  print_accesses ()
