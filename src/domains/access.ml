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

(* type -> lval option -> partition option -> (2^(write, loc, e, locks), locks_union) *)
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
let add_one (e:exp) (w:bool) (ty:acc_typ) lv ((pp,lp):part): unit =
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
          (Set.add (w,loc,e,lp) s, LSSet.inter lp o_lp)
        )
    in
    if LSSSet.is_empty pp then
      Ht.modify_def lvh None (lazy (Set.empty,lp)) (fun (s,o_lp) ->
          (Set.add (w,loc,e,lp) s, LSSet.inter lp o_lp)
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

let add_struct (e:exp) (w:bool) (ty:acc_typ) lv (p:part): unit =
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
      List.iter (fun os -> add_one e w (`Struct (s,addOffset os2 os)) (add_lv os) p) oss
    with Failure _ ->
      add_one e w ty lv p
    end
  | _ -> add_one e w ty lv p

let rec add_propagate e w ty ls p =
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
    let add_vars v = add_struct e w (`Struct (fi.fcomp, f)) (Some (v, f)) p in
    List.iter add_vars vars;
    add_struct e w (`Struct (fi.fcomp, f)) None p;
  in
  let just_vars t v = 
    add_struct e w (`Type t) (Some (v, NoOffset)) p;    
  in
  add_struct e w ty None p;
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

let rec distribute_access_lval f w r lv =
  f w r (mkAddrOf lv);
  distribute_access_lval_addr f w r lv

and distribute_access_lval_addr f w r lv =
  match lv with 
  | (Var v, os) -> 
    distribute_access_offset f os
  | (Mem e, os) ->
    distribute_access_offset f os;
    distribute_access_exp f false false e

and distribute_access_offset f = function
  | NoOffset -> ()
  | Field (_,os) -> 
    distribute_access_offset f os
  | Index (e,os) -> 
    distribute_access_exp f false false e;
    distribute_access_offset f os

and distribute_access_exp f w r = function
  (* Variables and address expressions *)
  | Lval lval ->
    distribute_access_lval f w r lval;

  (* Binary operators *)
  | BinOp (op,arg1,arg2,typ) ->
    distribute_access_exp f w r arg1;
    distribute_access_exp f w r arg2 

  (* Unary operators *)
  | UnOp (op,arg1,typ) -> distribute_access_exp f w r arg1
  
  (* The address operators, we just check the accesses under them *)
  | AddrOf lval | StartOf lval -> 
    if r then
      distribute_access_lval f w r lval
    else
      distribute_access_lval_addr f false r lval
    
  (* Most casts are currently just ignored, that's probably not a good idea! *)
  | CastE  (t, exp) -> 
    distribute_access_exp f w r exp
  | Question (b,t,e,_) ->
    distribute_access_exp f false r b;
    distribute_access_exp f w     r t;
    distribute_access_exp f w     r e
  | _ -> ()

let add e w vo oo p =
  if not !Goblintutil.may_narrow then begin
    let ty = get_val_type e vo oo in
    (* let loc = !Tracing.current_loc in *)
    (* ignore (printf "add %a %b -- %a\n" d_exp e w d_loc loc); *)
    match vo, oo with
    | Some v, Some o -> add_struct e w ty (Some (v, o)) p
    | _ -> 
      if !unsound && isArithmeticType (type_from_type_offset ty) then
        add_struct e w ty None p
      else
        add_propagate e w ty None p
  end

let partition_race ps (accs,ls) =
  let write (w,loc,e,lp) = w in
  ps <> None && LSSet.is_empty ls && Set.exists write accs

let only_read ps (accs,ls) =
  let read (w,loc,e,lp) = not w in
  Set.for_all read accs

let common_resource ps (accs,ls) = 
  not (LSSet.is_empty ls)

let bot_partition ps _ = 
  ps = None

(* let is_data_race k =
   let ht_exists f ht = Hashtbl.fold (fun k v z -> z || f k v) ht false in
   if not (Hashtbl.mem accs k) then
    false
   else begin
    let ht = Hashtbl.find accs k in
    ht_exists partition_race ht
   end *)

let print_races_oldscool () =
  let allglobs = get_bool "allglobs" in
  let k ls (w,loc,e,lp) = 
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


let print_races () =
  let allglobs = get_bool "allglobs" in
  let safe   = ref 0 in
  let unsafe = ref 0 in
  let check_safe ls (accs,lp) prev_safe =
    prev_safe && (ls = None || only_read ls (accs,lp) || common_resource ls (accs,lp)) 
  in
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
    let safety = Hashtbl.fold check_safe ht true in
    incr (if safety then safe else unsafe);
    if not safety || allglobs then begin
      let safetext = if safety then "safe"  else "unsafe" in
      ignore(printf "Memory location %a (%s)\n" d_memo (ty,lv) safetext);
      Hashtbl.iter g ht
    end
  in
  let f ty = Hashtbl.iter (h ty) in
  Hashtbl.iter f accs;
  ignore (Pretty.printf "\nSummary:\n");
  ignore (Pretty.printf "\tsafe:    %d\n" !safe);
  ignore (Pretty.printf "\tunsafe:  %d\n" !unsafe);
  ignore (Pretty.printf "\t-------------------\n");
  ignore (Pretty.printf "\ttotal:   %d\n" ((!safe) + (!unsafe)))

let print_accesses () =
  let debug = get_bool "dbg.debug" in
  let g ls (acs,_) =
    let d_ls () = match ls with None -> text "_L" | Some ls -> LSSet.pretty () ls in
    let h (w,loc,e,lp) =
      let atyp = if w then "write" else "read" in
      ignore (printf "  %s@@%a %t -> %a" atyp d_loc loc
                d_ls LSSet.pretty lp);
      if debug then
        ignore (printf "  (exp: %a)\n" d_exp e)
      else
        ignore (printf "\n")
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
  if !some_accesses then begin
    print_races_oldscool ();
    ignore (printf "--------------------\nListing of accesses:\n");
    print_accesses ();
    ignore (printf "\nListing of results:\n");
    print_races ()
  end
    
