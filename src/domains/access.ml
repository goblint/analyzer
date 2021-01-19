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
  include Printable.Std (* for default invariant, tag, ... *)

  open Pretty
  type t = string [@@deriving to_yojson]
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let compare (x:t) (y:t) = compare x y
  let isSimple _ = true
  let short _ x = x
  let pretty_f sf () x = text (sf 80 x)
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
end
module LSSet =
struct
  module S = SetDomain.Make (LabeledString)
  include S
  include Lattice.Reverse (S)
end
module LSSSet =
struct
  module S = SetDomain.Make (LSSet)
  include S
  (* TODO: other ops *)
  let meet po pd =
    let mult_po s = S.union (S.map (LSSet.union s) po) in
    S.fold mult_po pd (S.empty ())
end

module PartAccessResult = Lattice.Prod (LSSSet) (LSSet)

let typeVar  = Hashtbl.create 101
let typeIncl = Hashtbl.create 101
let unsound = ref false

let init (f:file) =
  unsound := get_bool "ana.mutex.disjoint_types";
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

type offs = [`NoOffset | `Index of 't | `Field of fieldinfo * 't] as 't

let rec compareOffs (off1: offs) (off2: offs) : bool =
  match off1, off2 with
  | `Field (fld1, off1'), `Field (fld2, off2') ->
    fld1 == fld2 && compareOffs off1' off2'
  | `Index off1', `Index off2' ->
    compareOffs off1' off2'
  | `NoOffset, `NoOffset -> true
  | _ -> false

let rec offs_eq x y =
  match x, y with
  | `NoOffset, `NoOffset -> true
  | `Index x, `Index y -> offs_eq x y
  | `Field (f,x), `Field (g,y) -> f.fcomp.ckey = g.fcomp.ckey && f.fname = g.fname && offs_eq x y
  | _ -> false

let rec remove_idx : offset -> offs  = function
  | NoOffset    -> `NoOffset
  | Index (_,o) -> `Index (remove_idx o)
  | Field (f,o) -> `Field (f, remove_idx o)

let rec addOffs os1 os2 : offs =
  match os1 with
  | `NoOffset -> os2
  | `Index os -> `Index (addOffs os os2)
  | `Field (f,os) -> `Field (f, addOffs os os2)

let rec d_offs () : offs -> doc = function
  | `NoOffset -> nil
  | `Index o -> dprintf "[?]%a" d_offs o
  | `Field (f,o) -> dprintf ".%s%a" f.fname d_offs o

type acc_typ = [ `Type of typ | `Struct of compinfo * offs ]

let d_acct () = function
  | `Type t -> dprintf "(%a)" d_type t
  | `Struct (s,o) -> dprintf "(struct %s)%a" s.cname d_offs o

let file_re = Str.regexp "\\(.*/\\|\\)\\([^/]*\\)"
let d_loc () loc =
  if Str.string_match file_re loc.file 0 then
    dprintf "%s:%d" (Str.matched_group 2 loc.file) loc.line
  else
    dprintf "%s:%d" loc.file loc.line

let d_memo () (t, lv) =
  match lv with
  | Some (v,o) -> dprintf "%s%a@@%a" v.vname d_offs o d_loc v.vdecl
  | None       -> dprintf "%a" d_acct t

let rec get_type (fb: typ) : exp -> acc_typ = function
  | AddrOf (h,o) | StartOf (h,o) ->
    let rec f htyp =
      match htyp with
      | TComp (ci,_) -> `Struct (ci,remove_idx o)
      | TNamed (ti,_) -> f ti.ttype
      | _ -> `Type fb
    in
    begin match o with
      | Field (f, on) -> `Struct (f.fcomp, remove_idx o)
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
        when s1.ckey = s2.ckey && compareOffs o1 o2 ->
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

module HtF (T:Hashtbl.HashedType) =
struct
  include Hashtbl.Make (T)

  let find_def ht k z : 'b =
    try
      find ht k
    with Not_found ->
      let v = Lazy.force z in
      add ht k v;
      v

  let modify_def ht k z f: unit =
    let g = function
      | None -> Some (f (Lazy.force z))
      | Some b -> Some (f b)
    in
    modify_opt k g ht

end

module Ht =
struct
  include Hashtbl

  let find_def ht k z : 'b =
    try
      find ht k
    with Not_found ->
      let v = Lazy.force z in
      add ht k v;
      v

  let modify_def ht k z f: unit =
    let g = function
      | None -> Some (f (Lazy.force z))
      | Some b -> Some (f b)
    in
    modify_opt k g ht

end

(* type -> lval option -> partition option -> (2^(confidence, write, loc, e, locks), locks_union) *)
module Acc_typHashable
  : Hashtbl.HashedType with type t = acc_typ =
struct
  type t = acc_typ
  let equal (x:t) y =
    match x, y with
    | `Type t, `Type v -> Basetype.CilType.equal t v
    | `Struct (c1,o1), `Struct (c2,o2) -> c1.ckey = c2.ckey && compareOffs o1 o2
    | _ -> false
  let hash = function
    | `Type t -> Basetype.CilType.hash t
    | `Struct (c,o) -> Hashtbl.hash (c.ckey, o)
end
module TypeHash = HtF (Acc_typHashable)

module LvalOptHashable
  : Hashtbl.HashedType with type t = (varinfo * offs) option =
struct
  type t = (varinfo * offs) option
  let equal (x:t) (y:t) =
    match x, y with
    | Some (v1,o1), Some (v2,o2) -> v1.vid = v2.vid && offs_eq o1 o2
    | None, None -> true
    | _ -> false
  let hash = function
    | None -> 435
    | Some (x,y) -> Hashtbl.hash (x.vid, Hashtbl.hash y)
end
module LvalOptHash = HtF (LvalOptHashable)

module PartOptHashable
  : Hashtbl.HashedType with type t = LSSet.t option =
struct
  type t = LSSet.t option
  let equal x y =
    match x, y with
    | Some x, Some y -> LSSet.equal x y
    | None  , None   -> true
    | _ -> false

  let hash = function
    | Some x -> LSSet.hash x
    | None -> 101
end
module PartOptHash = HtF (PartOptHashable)

let accs = TypeHash.create 100

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
let add_one (e:exp) (w:bool) (conf:int) (ty:acc_typ) (lv:(varinfo*offs) option) ((pp,lp):part): unit =
  if is_ignorable lv then () else begin
    some_accesses := true;
    let tyh = TypeHash.find_def accs  ty (lazy (LvalOptHash.create 10)) in
    let lvh = LvalOptHash.find_def tyh lv (lazy (PartOptHash.create 10)) in
    let loc = !Tracing.current_loc in
    let add_part ls =
      PartOptHash.modify_def lvh (Some(ls)) (lazy (Set.empty,lp)) (fun (s,o_lp) ->
          (Set.add (conf, w,loc,e,lp) s, LSSet.inter lp o_lp)
        )
    in
    if LSSSet.is_empty pp then
      PartOptHash.modify_def lvh None (lazy (Set.empty,lp)) (fun (s,o_lp) ->
          (Set.add (conf, w,loc,e,lp) s, LSSet.inter lp o_lp)
        )
    else
      LSSSet.iter add_part pp
  end

let type_from_type_offset : acc_typ -> typ = function
  | `Type t -> t
  | `Struct (s,o) ->
    let deref t =
      match unrollType t with
      | TPtr (t,_) -> t  (*?*)
      | TArray (t,_,_) -> t
      | _ -> failwith "type_from_type_offset: indexing non-pointer type"
    in
    let rec type_from_offs (t,o) =
      match o with
      | `NoOffset -> t
      | `Index os -> type_from_offs (deref t, os)
      | `Field (f,os) -> type_from_offs (f.ftype, os)
    in
    unrollType (type_from_offs (TComp (s, []), o))

let add_struct (e:exp) (w:bool) (conf:int) (ty:acc_typ) (lv: (varinfo * offs) option) (p:part): unit =
  let rec dist_fields ty =
    match unrollType ty with
    | TComp (ci,_)   ->
      let one_field fld =
        List.map (fun x -> `Field (fld,x)) (dist_fields fld.ftype)
      in
      List.concat (List.map one_field ci.cfields)
    | TArray (t,_,_) ->
      List.map (fun x -> `Index x) (dist_fields t)
    | _ -> [`NoOffset]
  in
  match ty with
  | `Struct (s,os2) ->
    let add_lv os = match lv with
      | Some (v, os1) -> Some (v, addOffs os1 os)
      | None -> None
    in
    begin try
        let oss = dist_fields (type_from_type_offset ty) in
        List.iter (fun os -> add_one e w conf (`Struct (s,addOffs os2 os)) (add_lv os) p) oss
      with Failure _ ->
        add_one e w conf ty lv p
    end
  | _ when lv = None && !unsound ->
    (* don't recognize accesses to locations such as (long ) and (int ). *)
    ()
  | _ ->
    add_one e w conf ty lv p

let add_propagate e w conf ty ls p =
  (* ignore (printf "%a:\n" d_exp e); *)
  let rec only_fields = function
    | `NoOffset -> true
    | `Field (_,os) -> only_fields os
    | `Index _ -> false
  in
  let struct_inv (f:offs) =
    let fi =
      match f with
      | `Field (fi,_) -> fi
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
    add_struct e w conf (`Type t) (Some (v, `NoOffset)) p;
  in
  add_struct e w conf ty None p;
  match ty with
  | `Struct (c,os) when only_fields os && os <> `NoOffset ->
    (* ignore (printf "  * type is a struct\n"); *)
    struct_inv  os
  | _ ->
    (* ignore (printf "  * type is NOT a struct\n"); *)
    let t = type_from_type_offset ty in
    let incl = Ht.find_all typeIncl (typeSig t) in
    List.iter (fun fi -> struct_inv (`Field (fi,`NoOffset))) incl;
    let vars = Ht.find_all typeVar (typeSig t) in
    List.iter (just_vars t) vars

let rec distribute_access_lval f w r c lv =
  (* Use unoptimized AddrOf so RegionDomain.Reg.eval_exp knows about dereference *)
  (* f w r c (mkAddrOf lv); *)
  f w r c (AddrOf lv);
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
  if !Goblintutil.should_warn then begin
    let ty = get_val_type e vo oo in
    (* let loc = !Tracing.current_loc in *)
    (* ignore (printf "add %a %b -- %a\n" d_exp e w d_loc loc); *)
    match vo, oo with
    | Some v, Some o -> add_struct e w conf ty (Some (v, remove_idx o)) p
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
    (* ignore(printf "intersection with %a = %a\n" LSSet.pretty lp LSSet.pretty new_lp); *)
    let new_r  = if union_empty && new_w then Some conf else None in
    (new_r, new_lp, new_w)
  | _ -> (prev_r,prev_lp,prev_w)

let check_safe ls (accs,lp) prev_safe =
  if ls = None then
    prev_safe
  else
    let ord_enum = Set.backwards accs in (* hope that it is not nil *)
    let lp_start = (fun (_,_,_,_,lp) -> lp) (BatOption.get (BatEnum.peek ord_enum)) in
    (* ignore(printf "starting with lockset %a\n" LSSet.pretty lp_start); *)
    match BatEnum.fold check_accs (None, lp_start, false) (Set.backwards accs), prev_safe with
    | (None, _,_), _ ->
      (* ignore(printf "this batch is safe\n"); *)
      prev_safe
    | (Some n,_,_), Some m ->
      (* ignore(printf "race with %d and %d \n" n m); *)
      Some (max n m)
    | (Some n,_,_), None ->
      (* ignore(printf "race with %d\n" n); *)
      Some n

let is_all_safe () =
  let safe = ref true in
  let h ty lv ht =
    let safety = PartOptHash.fold check_safe ht None in
    match safety with
    | None -> ()
    | Some n -> safe := false
  in
  let f ty = LvalOptHash.iter (h ty) in
  TypeHash.iter f accs;
  !safe


let print_races_oldscool () =
  let allglobs = get_bool "allglobs" in
  let k ls (conf,w,loc,e,lp) =
    let wt = if w then "write" else "read" in
    match ls with
    | Some ls ->
      sprint 80 (dprintf "%s by ??? %a and lockset: %a" wt LSSet.pretty ls LSSet.pretty lp), loc
    | None ->
      sprint 80 (dprintf "%s by ??? ⊥ and lockset: %a" wt LSSet.pretty lp), loc
  in
  let g ty lv ls (accs,lp) (s,xs) =
    let nxs  = Set.fold (fun e xs -> (k ls e) :: xs) accs xs in
    let safe = s && not (partition_race ls (accs,lp)) in
    (safe, nxs)
  in
  let h ty lv ht =
    let safe, xs = PartOptHash.fold (g ty lv) ht (true, []) in
    let groupname =
      if safe then
        sprint 80 (dprintf "Safely accessed %a (reasons ...)" d_memo (ty,lv))
      else
        sprint 80 (dprintf "Datarace at %a" d_memo (ty,lv))
    in
    if not safe || allglobs then
      Messages.print_group groupname xs
  in
  let f ty = LvalOptHash.iter (h ty) in
  TypeHash.iter f accs

(* Commenting your code is for the WEAK! *)
let print_summary () =
  let safe       = ref 0 in
  let vulnerable = ref 0 in
  let unsafe     = ref 0 in
  let h ty lv ht =
    (* ignore(printf "Checking safety of %a:\n" d_memo (ty,lv)); *)
    let safety = PartOptHash.fold check_safe ht None in
    match safety with
    | None -> incr safe
    | Some n when n >= 100 -> incr unsafe
    | Some n -> incr vulnerable
  in
  let f ty = LvalOptHash.iter (h ty) in
  TypeHash.iter f accs;
  ignore (Pretty.printf "\nSummary for all memory locations:\n");
  ignore (Pretty.printf "\tsafe:        %5d\n" !safe);
  ignore (Pretty.printf "\tvulnerable:  %5d\n" !vulnerable);
  ignore (Pretty.printf "\tunsafe:      %5d\n" !unsafe);
  ignore (Pretty.printf "\t-------------------\n");
  ignore (Pretty.printf "\ttotal:       %5d\n" ((!safe) + (!unsafe) + (!vulnerable)))

let print_accesses () =
  let allglobs = get_bool "allglobs" in
  let debug = get_bool "dbg.debug" in
  let g ls (acs,_) =
    let h (conf,w,loc,e,lp) =
      let d_ls () = match ls with
        | None -> Pretty.text " is ok"
        | Some ls when LSSet.is_empty ls -> nil
        | Some ls -> text " in " ++ LSSet.pretty () ls
      in
      let atyp = if w then "write" else "read" in
      ignore (printf "  %s@@%a%t with %a (conf. %d)" atyp d_loc loc
                d_ls LSSet.pretty lp conf);
      if debug then
        ignore (printf "  (exp: %a)\n" d_exp e)
      else
        ignore (printf "\n")
    in
    Set.iter h acs
  in
  let h ty lv ht =
    match PartOptHash.fold check_safe ht None with
    | None ->
      if allglobs then begin
        ignore(printf "Memory location %a (safe)\n" d_memo (ty,lv));
        PartOptHash.iter g ht
      end
    | Some n ->
      ignore(printf "Memory location %a (race with conf. %d)\n" d_memo (ty,lv) n);
      PartOptHash.iter g ht
  in
  let f ty ht =
    LvalOptHash.iter (h ty) ht
  in
  TypeHash.iter f accs

let print_accesses_xml () =
  let allglobs = get_bool "allglobs" in
  let g ls (acs,_) =
    let h (conf,w,loc,e,lp) =
      let atyp = if w then "write" else "read" in
      BatPrintf.printf "  <access type=\"%s\" loc=\"%s\" conf=\"%d\">\n"
        atyp (Basetype.ProgLines.short 0 loc) conf;

      let d_lp f (t,id) = BatPrintf.fprintf f "type=\"%s\" id=\"%s\"" t id in

      let _ = match ls with
        | None -> print_endline "    <partitions status=\"safe\" />"
        (*| Some ls when LSSet.is_empty ls ->  print_endline "    <partitions status=\"none\" />" *)
        | Some ls ->
          print_endline "    <partitions>";
          LSSet.iter (BatPrintf.printf "      <part %a />\n" d_lp) ls;
          print_endline "    </partitions>";
      in

      print_endline "    <protectors>";
      LSSet.iter (BatPrintf.printf "      <prot %a />\n" d_lp) lp;
      print_endline "    </protectors>";
      print_endline "  </access>"
    in
    Set.iter h acs
  in
  let h ty lv ht =
    let memo = sprint ~width:0 (d_memo () (ty,lv)) in
    match PartOptHash.fold check_safe ht None with
    | None ->
      if allglobs then begin
        BatPrintf.printf "<mem id=\"%s\" status=\"safe\">\n" memo;
        PartOptHash.iter g ht;
        print_endline "</mem>"
      end
    | Some n ->
      BatPrintf.printf "<mem id=\"%s\" status=\"race\" conf=\"%d\">\n" memo n;
      PartOptHash.iter g ht;
      print_endline "</mem>"
  in
  let f ty ht =
    LvalOptHash.iter (h ty) ht
  in
  print_endline "<warnings>";
  TypeHash.iter f accs;
  print_endline "</warnings>"

let print_result () =
  if !some_accesses then
    match get_string "warnstyle" with
    | "legacy" -> print_races_oldscool ()
    | "xml" -> print_accesses_xml ()
    | _ ->
      print_accesses ();
      print_summary ()
