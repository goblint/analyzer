open Batteries
open Cil
open Pretty
open GobConfig

(* Some helper functions to avoid flagging race warnings on atomic types, and
 * other irrelevant stuff, such as mutexes and functions. *)

let is_ignorable_type (t: typ): bool =
  match t with
  | TNamed (info, attr) -> info.tname = "atomic_t" || info.tname = "pthread_mutex_t" || info.tname = "spinlock_t"
  | TComp (info, attr) -> info.cname = "lock_class_key"
  | TInt (IInt, attr) -> hasAttribute "mutex" attr
  | _ -> false

let is_ignorable = function
  | None -> false
  | Some (v,os) ->
      try isFunctionType v.vtype || is_ignorable_type v.vtype
      with Not_found -> false

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

let typeVar  = Hashtbl.create 101
let typeIncl = Hashtbl.create 101
let unsound = ref false 

let init (f:file) = 
  unsound := get_bool "exp.unsoundbasic";
  let visited_vars = Hashtbl.create 100 in
  let visit_field fi = 
    Hashtbl.add typeIncl (typeSig fi.ftype) fi
  in
  let visit_glob = function
    | GCompTag (c,_) -> 
      List.iter visit_field c.cfields
    | GVarDecl (v,_) | GVar (v,_,_) ->
      if not (Hashtbl.mem visited_vars v.vid) then begin
        Hashtbl.add typeVar (typeSig v.vtype) v;
        (* ignore (printf "init adding %s : %a" v.vname d_typsig ((typeSig v.vtype))); *)
        Hashtbl.replace visited_vars v.vid true
      end
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

type offs = [`NoOffset | `Index of 't | `Field of fieldinfo * 't] as 't

let rec remove_idx : offset -> offs  = function
  | NoOffset    -> `NoOffset
  | Index (_,o) -> `Index (remove_idx o)
  | Field (f,o) -> `Field (f, remove_idx o)

let rec d_offs () : offs -> doc = function
  | `NoOffset -> nil
  | `Index o -> dprintf "[?]%a" d_offs o
  | `Field (f,o) -> dprintf ".%s%a" f.fname d_offs o

let d_memo () (t, lv) =
  match lv with 
  | Some (v,o) -> dprintf "%s%a" v.vname d_offs o
  | None       -> dprintf "%a" d_acct t

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

(* type -> lval option -> partition option -> (2^(confidence, write, loc, e, locks), locks_union) *)
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

let some_accesses = ref false
let add_one (e:exp) (w:bool) (conf:int) (ty:acc_typ) lv ((pp,lp):part): unit =
  let lv = 
    match lv with
    | None -> None 
    | Some (v,os) -> Some (v, remove_idx os)
  in
  if is_ignorable lv then () else begin
    some_accesses := true;
    let tyh = Ht.find_def accs  ty (lazy (Ht.create 10)) in
    let lvh = Ht.find_def tyh lv (lazy (Ht.create 10)) in
    let loc = !Tracing.current_loc in
    let add_part ls =
      Ht.modify_def lvh (Some(ls)) (lazy (Set.empty,lp)) (fun (s,o_lp) ->
          (Set.add (conf, w,loc,e,lp) s, LSSet.inter lp o_lp)
        )
    in
    if LSSSet.is_empty pp then
      Ht.modify_def lvh None (lazy (Set.empty,lp)) (fun (s,o_lp) ->
          (Set.add (conf, w,loc,e,lp) s, LSSet.inter lp o_lp)
        )
    else
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

let add_struct (e:exp) (w:bool) (conf:int) (ty:acc_typ) lv (p:part): unit =
  let rec dist_fields ty =
    match unrollType ty with
    | TComp (ci,_)   ->
      let one_field fld =
        List.map (fun x -> Field (fld,x)) (dist_fields fld.ftype)
      in
      List.concat (List.map one_field ci.cfields)
    | TArray (t,_,_) -> 
      List.map (fun x -> Index(mone,x)) (dist_fields t)
    | _ -> [NoOffset]
  in
  match ty with
  | `Struct (s,os2) ->
    let add_lv os = match lv with
      | Some (v, os1) -> Some (v, addOffset os1 os)
      | None -> None
    in
    begin try 
      let oss = dist_fields (type_from_type_offset ty) in
      List.iter (fun os -> add_one e w conf (`Struct (s,addOffset os2 os)) (add_lv os) p) oss
    with Failure _ ->
      add_one e w conf ty lv p
    end
  | _ when lv = None && !unsound -> 
    (* don't recognize accesses to locations such as (long ) and (int ). *)
    ()
  | _ -> 
    add_one e w conf ty lv p

let rec add_propagate e w conf ty ls p =
  (* ignore (printf "%a:\n" d_exp e); *)
  let rec only_fields = function
    | NoOffset -> true
    | Field (_,os) -> only_fields os
    | Index _ -> false
  in
  let struct_inv f = 
    let fi = 
      match f with
      | Field (fi,_) -> fi
      | _ -> Messages.bailwith "add_propagate: no field found"
    in
    let ts = typeSig (TComp (fi.fcomp,[])) in
    let vars = Ht.find_all typeVar ts in
    (* List.iter (fun v -> ignore (printf " * %s : %a" v.vname d_typsig ts)) vars; *)
    let add_vars v = add_struct e w conf (`Struct (fi.fcomp, f)) (Some (v, f)) p in
    List.iter add_vars vars;
    add_struct e w conf (`Struct (fi.fcomp, f)) None p;
  in
  let just_vars t v = 
    add_struct e w conf (`Type t) (Some (v, NoOffset)) p;    
  in
  add_struct e w conf ty None p;
  match ty with
  | `Struct (c,os) when only_fields os && os <> NoOffset ->
    (* ignore (printf "  * type is a struct\n"); *)
    struct_inv  os
  | _ ->
    (* ignore (printf "  * type is NOT a struct\n"); *)
    let t = type_from_type_offset ty in
    let incl = Ht.find_all typeIncl (typeSig t) in
    List.iter (fun fi -> struct_inv (Field (fi,NoOffset))) incl;
    let vars = Ht.find_all typeVar (typeSig t) in
    List.iter (just_vars t) vars

let rec distribute_access_lval f w r c lv =
  f w r c (mkAddrOf lv);
  distribute_access_lval_addr f w r c lv

and distribute_access_lval_addr f w r c lv =
  match lv with 
  | (Var v, os) -> 
    distribute_access_offset f c os
  | (Mem e, os) ->
    distribute_access_offset f c os;
    distribute_access_exp f false false c e

and distribute_access_offset f c = function
  | NoOffset -> ()
  | Field (_,os) -> 
    distribute_access_offset f c os
  | Index (e,os) -> 
    distribute_access_exp f false false c e;
    distribute_access_offset f c os

and distribute_access_exp f w r c = function
  (* Variables and address expressions *)
  | Lval lval ->
    distribute_access_lval f w r c lval;

  (* Binary operators *)
  | BinOp (op,arg1,arg2,typ) ->
    distribute_access_exp f w r c arg1;
    distribute_access_exp f w r c arg2 

  (* Unary operators *)
  | UnOp (op,arg1,typ) -> distribute_access_exp f w r c arg1
  
  (* The address operators, we just check the accesses under them *)
  | AddrOf lval | StartOf lval -> 
    if r then
      distribute_access_lval f w r c lval
    else
      distribute_access_lval_addr f false r c lval
    
  (* Most casts are currently just ignored, that's probably not a good idea! *)
  | CastE  (t, exp) -> 
    distribute_access_exp f w r c exp
  | Question (b,t,e,_) ->
    distribute_access_exp f false r c b;
    distribute_access_exp f w     r c t;
    distribute_access_exp f w     r c e
  | _ -> ()

let add e w conf vo oo p =
  if not !Goblintutil.may_narrow then begin
    let ty = get_val_type e vo oo in
    (* let loc = !Tracing.current_loc in *)
    (* ignore (printf "add %a %b -- %a\n" d_exp e w d_loc loc); *)
    match vo, oo with
    | Some v, Some o -> add_struct e w conf ty (Some (v, o)) p
    | _ -> 
      if !unsound && isArithmeticType (type_from_type_offset ty) then
        add_struct e w conf ty None p
      else
        add_propagate e w conf ty None p
  end

let partition_race ps (accs,ls) =
  let write (conf,w,loc,e,lp) = w in
  ps <> None && LSSet.is_empty ls && Set.exists write accs

let only_read ps (accs,ls) =
  let read (conf,w,loc,e,lp) = not w in
  Set.for_all read accs

let common_resource ps (accs,ls) = 
  not (LSSet.is_empty ls)

let bot_partition ps _ = 
  ps = None

let check_accs (prev_r,prev_lp,prev_w) (conf,w,loc,e,lp) = 
  match prev_r with
  | None ->
    let new_w  = prev_w || w in
    let new_lp = LSSet.inter lp prev_lp in
    let union_empty = LSSet.is_empty new_lp in
    let new_r  = if union_empty && new_w then Some conf else None in
    (new_r, new_lp, new_w)
  | _ -> (prev_r,prev_lp,prev_w)

let check_safe ls (accs,lp) prev_safe =
  if ls = None then
    prev_safe
  else
    let lp_start = (fun (_,_,_,_,lp) -> lp) (Set.choose accs) in
    match BatEnum.fold check_accs (None, lp_start, false) (Set.backwards accs), prev_safe with
    | (None, _,_), _ -> prev_safe
    | (Some n,_,_), Some m -> Some (max n m)
    | (Some n,_,_), None -> Some n


let print_races_oldscool () =
  let allglobs = get_bool "allglobs" in
  let k ls (conf,w,loc,e,lp) = 
    let wt = if w then "write" else "read" in
    match ls with
    | Some ls ->
      sprint 80 (dprintf "%s by ??? %a and lockset: %a" wt LSSet.pretty ls LSSet.pretty lp), loc
    | None ->
      sprint 80 (dprintf "%s by ??? _L and lockset: %a" wt LSSet.pretty lp), loc
  in
  let g ty lv ls (accs,lp) (s,xs) =
    let nxs  = Set.fold (fun e xs -> (k ls e) :: xs) accs xs in
    let safe = s && not (partition_race ls (accs,lp)) in
    (safe, nxs)
  in
  let h ty lv ht =
    let safe, xs = Hashtbl.fold (g ty lv) ht (true, []) in
    let groupname = 
      if safe then
        sprint 80 (dprintf "Safely accessed %a (reasons ...)" d_memo (ty,lv)) 
      else
        sprint 80 (dprintf "Datarace at %a" d_memo (ty,lv)) 
    in
    if not safe || allglobs then
      Messages.print_group groupname xs
  in
  let f ty = Hashtbl.iter (h ty) in
  ignore (Pretty.printf "vvvv This output is here because our regression test scripts parse this format. \n");
  Hashtbl.iter f accs;
  ignore (Pretty.printf "^^^^ This output is here because our regression test scripts parse this format. \n")

  (* Commenting your code is for the WEAK! *)
let print_races () =
  let allglobs = get_bool "allglobs" in
  let safe       = ref 0 in
  let vulnerable = ref 0 in
  let unsafe     = ref 0 in
  let g ls (accs,lp) =
    let reason = 
      if bot_partition ls (accs,lp) then
        "non-shared"
      else if only_read ls (accs,lp) then
        "only read"
      else if common_resource ls (accs,lp) then
        "common resource"
      else
        "race"
    in
    match ls with
    | Some ls ->
      ignore (Pretty.printf "  %a -> %a (%s)\n" LSSet.pretty ls LSSet.pretty lp reason)
    | None ->
      ignore (Pretty.printf "  _L -> %a (%s)\n" LSSet.pretty lp reason)
  in
  let h ty lv ht =
    let safety = Hashtbl.fold check_safe ht None in
    let print_location safetext = 
      ignore(printf "Memory location %a (%s)\n" d_memo (ty,lv) safetext);
      Hashtbl.iter g ht
    in
    match safety with
    | None -> 
        incr safe;
        if allglobs then begin
          print_location "safe"
        end
    | Some n when n >= 100 -> 
        incr unsafe;
        print_location "unsafe"
    | Some n ->
        incr vulnerable;
        print_location "vulnerable"
  in
  let f ty = Hashtbl.iter (h ty) in
  Hashtbl.iter f accs;
  ignore (Pretty.printf "\nSummary:\n");
  ignore (Pretty.printf "\tsafe:        %5d\n" !safe);
  ignore (Pretty.printf "\tvulnerable:  %5d\n" !vulnerable);
  ignore (Pretty.printf "\tunsafe:      %5d\n" !unsafe);
  ignore (Pretty.printf "\t-------------------\n");
  ignore (Pretty.printf "\ttotal:       %5d\n" ((!safe) + (!unsafe) + (!vulnerable)))

let print_accesses () =
  let debug = get_bool "dbg.debug" in
  let g ls (acs,_) =
    let d_ls () = match ls with None -> text "_L" | Some ls -> LSSet.pretty () ls in
    let h (conf,w,loc,e,lp) =
      let atyp = if w then "write" else "read" in
      ignore (printf "  %s@@%a %t -> %a (conf. %d)" atyp d_loc loc
                d_ls LSSet.pretty lp conf);
      if debug then
        ignore (printf "  (exp: %a)\n" d_exp e)
      else
        ignore (printf "\n")
    in
    Set.iter h acs
  in
  let h ty lv ht =
    match Hashtbl.fold check_safe ht None with 
    | None ->
      ignore(printf "Memory location %a (safe)\n" d_memo (ty,lv));
      Hashtbl.iter g ht
    | Some n -> 
      ignore(printf "Memory location %a (race with conf. %d)\n" d_memo (ty,lv) n);
      Hashtbl.iter g ht
  in
  let f ty ht = 
    Hashtbl.iter (h ty) ht
  in
  Hashtbl.iter f accs

let print_result () = 
  if !some_accesses then begin
    print_races_oldscool ();
    ignore (printf "--------------------\nListing of accesses:\n");
    print_accesses ();
    ignore (printf "\nListing of results:\n");
    print_races ()
  end
    
