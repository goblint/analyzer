(** Memory accesses and their manipulation. *)

open Batteries
open GoblintCil
open Pretty
open GobConfig

module M = Messages

(* Some helper functions to avoid flagging race warnings on atomic types, and
 * other irrelevant stuff, such as mutexes and functions. *)

let is_ignorable_type (t: typ): bool =
  match t with
  | TNamed ({ tname = "atomic_t" | "pthread_mutex_t" | "pthread_rwlock_t" | "pthread_spinlock_t" | "spinlock_t" | "pthread_cond_t"; _ }, _) -> true
  | TComp ({ cname = "lock_class_key"; _ }, _) -> true
  | TInt (IInt, attr) when hasAttribute "mutex" attr -> true
  | t when hasAttribute "atomic" (typeAttrs t) -> true (* C11 _Atomic *)
  | _ -> false

let is_ignorable = function
  | None -> false
  | Some (v,os) when hasAttribute "thread" v.vattr && not (v.vaddrof) -> true (* Thread-Local Storage *)
  | Some (v,os) ->
    try isFunctionType v.vtype || is_ignorable_type v.vtype
    with Not_found -> false

module TH = Hashtbl.Make (CilType.Typ)

let typeVar  = TH.create 101
let typeIncl = TH.create 101
let unsound = ref false

let init (f:file) =
  unsound := get_bool "ana.mutex.disjoint_types";
  let visited_vars = Hashtbl.create 100 in
  let visit_field fi =
    TH.add typeIncl fi.ftype fi
  in
  let visit_glob = function
    | GCompTag (c,_) ->
      List.iter visit_field c.cfields
    | GVarDecl (v,_) | GVar (v,_,_) ->
      if not (Hashtbl.mem visited_vars v.vid) then begin
        TH.add typeVar v.vtype v;
        (* ignore (printf "init adding %s : %a" v.vname d_typsig ((typeSig v.vtype))); *)
        Hashtbl.replace visited_vars v.vid true
      end
    | _ -> ()
  in
  List.iter visit_glob f.globals

let reset () =
  TH.clear typeVar;
  TH.clear typeIncl


type offs = [`NoOffset | `Index of offs | `Field of CilType.Fieldinfo.t * offs] [@@deriving eq, ord, hash]

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

type acc_typ = [ `Type of CilType.Typ.t | `Struct of CilType.Compinfo.t * offs ] [@@deriving eq, ord, hash]

let d_acct () = function
  | `Type t -> dprintf "(%a)" d_type t
  | `Struct (s,o) -> dprintf "(struct %s)%a" s.cname d_offs o

let d_memo () (t, lv) =
  match lv with
  | Some (v,o) -> dprintf "%a%a@@%a" Basetype.Variables.pretty v d_offs o CilType.Location.pretty v.vdecl
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
        when CilType.Compinfo.equal s1 s2 && equal_offs o1 o2 ->
        `Struct (s1, o1)
      | _ -> `Type t
    end
  | Const _
  | Lval _
  | Real _
  | Imag _ ->
    `Type fb (* TODO: is this right? *)

let get_type fb e =
  (* printf "e = %a\n" d_plainexp e; *)
  let r = get_type fb e in
  (* printf "result = %a\n" d_acct r; *)
  match r with
  | `Type (TPtr (t,a)) -> `Type t
  | x -> x




let get_val_type e (vo: varinfo option) (oo: offset option) : acc_typ =
  match Cilfacade.typeOf e with
  | t ->
    begin match vo, oo with
      | Some v, Some o -> get_type t (AddrOf (Var v, o))
      | Some v, None -> get_type t (AddrOf (Var v, NoOffset))
      | _ -> get_type t e
    end
  | exception (Cilfacade.TypeOfError _) -> get_type voidType e

let add_one side (ty:acc_typ) (lv:(varinfo*offs) option): unit =
  if is_ignorable lv then () else begin
    side ty lv
  end

exception Type_offset_error

let type_from_type_offset : acc_typ -> typ = function
  | `Type t -> t
  | `Struct (s,o) ->
    let deref t =
      match unrollType t with
      | TPtr (t,_) -> t  (*?*)
      | TArray (t,_,_) -> t
      | _ -> raise Type_offset_error (* indexing non-pointer type *) 
    in
    let rec type_from_offs (t,o) =
      match o with
      | `NoOffset -> t
      | `Index os -> type_from_offs (deref t, os)
      | `Field (f,os) -> type_from_offs (f.ftype, os)
    in
    unrollType (type_from_offs (TComp (s, []), o))

let add_struct side (ty:acc_typ) (lv: (varinfo * offs) option): unit =
  let rec dist_fields ty : offs list =
    match unrollType ty with
    | TComp (ci,_)   ->
      let one_field fld =
        if is_ignorable_type fld.ftype then
          []
        else
          List.map (fun x -> `Field (fld,x)) (dist_fields fld.ftype)
      in
      List.concat_map one_field ci.cfields
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
    begin match type_from_type_offset ty with
      | t -> 
        let oss = dist_fields t in
        (* 32 test(s) failed: ["02/26 malloc_struct", "04/49 type-invariants", "04/65 free_indirect_rc", "05/07 glob_fld_rc", "05/08 glob_fld_2_rc", "05/11 fldsense_rc", "05/15 fldunknown_access", "06/10 equ_rc", "06/16 type_rc", "06/21 mult_accs_rc", "06/28 symb_lockset_unsound", "06/29 symb_lockfun_unsound", "09/01 list_rc", "09/03 list2_rc", "09/05 ptra_rc", "09/07 kernel_list_rc", "09/10 arraylist_rc", "09/12 arraycollapse_rc", "09/14 kernel_foreach_rc", "09/16 arrayloop_rc", "09/18 nested_rc", "09/20 arrayloop2_rc", "09/23 evilcollapse_rc", "09/26 alloc_region_rc", "09/28 list2alloc", "09/30 list2alloc-offsets", "09/31 equ_rc", "09/35 list2_rc-offsets-thread", "09/36 global_init_rc", "29/01 race-2_3b-container_of", "29/02 race-2_4b-container_of", "29/03 race-2_5b-container_of"] *)
        List.iter (fun os -> add_one side (`Struct (s,addOffs os2 os)) (add_lv os)) oss
      | exception Type_offset_error ->
        add_one side ty lv
    end
  | _ when lv = None && !unsound ->
    (* don't recognize accesses to locations such as (long ) and (int ). *)
    ()
  | _ ->
    add_one side ty lv

let add_propagate side ty =
  (* ignore (printf "%a:\n" d_exp e); *)
  let rec only_fields = function
    | `NoOffset -> true
    | `Field (_, os) -> only_fields os
    | `Index _ -> false
  in
  let struct_inv (f:offs) (fi:fieldinfo) =
    let vars = TH.find_all typeVar (TComp (fi.fcomp,[])) in
    (* List.iter (fun v -> ignore (printf " * %s : %a" v.vname d_typsig ts)) vars; *)
    (* 1 test(s) failed: ["04/49 type-invariants"] *)
    let add_vars v = add_struct side (`Struct (fi.fcomp, f)) (Some (v, f)) in
    List.iter add_vars vars;
    (* 2 test(s) failed: ["06/16 type_rc", "06/21 mult_accs_rc"] *)
    add_struct side (`Struct (fi.fcomp, f)) None;
  in
  let just_vars t v =
    add_struct side (`Type t) (Some (v, `NoOffset));
  in
  match ty with
  | `Struct (c, (`Field (fi, _) as os)) when only_fields os ->
    (* ignore (printf "  * type is a struct\n"); *)
    (* 1 test(s) failed: ["04/49 type-invariants"] *)
    struct_inv os fi 
  | _ ->
    (* ignore (printf "  * type is NOT a struct\n"); *)
    let t = type_from_type_offset ty in
    let incl = TH.find_all typeIncl t in
    (* 2 test(s) failed: ["06/16 type_rc", "06/21 mult_accs_rc"] *)
    List.iter (fun fi -> struct_inv (`Field (fi,`NoOffset)) fi) incl;
    let vars = TH.find_all typeVar t in
    (* TODO: not tested *)
    List.iter (just_vars t) vars

let rec distribute_access_lval f lv =
  (* Use unoptimized AddrOf so RegionDomain.Reg.eval_exp knows about dereference *)
  (* f (mkAddrOf lv); *)
  f (AddrOf lv);
  distribute_access_lval_addr f lv

and distribute_access_lval_addr f lv =
  match lv with
  | (Var v, os) ->
    distribute_access_offset f os
  | (Mem e, os) ->
    distribute_access_offset f os;
    distribute_access_exp f e

and distribute_access_offset f = function
  | NoOffset -> ()
  | Field (_,os) ->
    distribute_access_offset f os
  | Index (e,os) ->
    distribute_access_exp f e;
    distribute_access_offset f os

and distribute_access_exp f = function
  (* Variables and address expressions *)
  | Lval lval ->
    distribute_access_lval f lval;

    (* Binary operators *)
  | BinOp (op,arg1,arg2,typ) ->
    distribute_access_exp f arg1;
    distribute_access_exp f arg2

  | UnOp (_,e,_)
  | Real e
  | Imag e
  | SizeOfE e
  | AlignOfE e ->
    distribute_access_exp f e

  (* The address operators, we just check the accesses under them *)
  | AddrOf lval | StartOf lval ->
    distribute_access_lval_addr f lval

  (* Most casts are currently just ignored, that's probably not a good idea! *)
  | CastE  (t, exp) ->
    distribute_access_exp f exp
  | Question (b,t,e,_) ->
    distribute_access_exp f b;
    distribute_access_exp f t;
    distribute_access_exp f e

  | SizeOf t ->
    distribute_access_type f t

  | Const _
  | SizeOfStr _
  | AlignOf _
  | AddrOfLabel _ ->
    ()

and distribute_access_type f = function
  | TArray (et, len, _) ->
    Option.may (distribute_access_exp f) len;
    distribute_access_type f et

  | TVoid _
  | TInt _
  | TFloat _
  | TPtr _
  | TFun _
  | TNamed _
  | TComp _
  | TEnum _
  | TBuiltin_va_list _ ->
    ()

let add side e vo oo =
  let ty = get_val_type e vo oo in
  (* let loc = !Tracing.current_loc in *)
  (* ignore (printf "add %a %b -- %a\n" d_exp e w d_loc loc); *)
  match vo, oo with
  | Some v, Some o -> add_struct side ty (Some (v, remove_idx o))
  | _ ->
    (* 8 test(s) failed: ["02/69 ipmi-struct-blob-fixpoint", "04/33 kernel_rc", "04/34 kernel_nr", "04/39 rw_lock_nr", "04/40 rw_lock_rc", "04/44 malloc_sound", "04/45 escape_rc", "04/46 escape_nr"] *)
    add_struct side ty None;
    if not (!unsound && isArithmeticType (type_from_type_offset ty)) then
      add_propagate side ty


(* Access table as Lattice. *)
(* (varinfo ->) offset -> type -> 2^(confidence, write, loc, e, acc) *)
module A =
struct
  include Printable.Std
  type t = int * AccessKind.t * Node.t * CilType.Exp.t * MCPAccess.A.t [@@deriving eq, ord, hash]

  let name () = "access"

  let pretty () (conf, kind, node, e, lp) =
    Pretty.dprintf "%d, %a, %a, %a, %a" conf AccessKind.pretty kind CilType.Location.pretty (Node.location node) CilType.Exp.pretty e MCPAccess.A.pretty lp

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let conf (conf, _, _, _, _) = conf

  let relift (conf, kind, node, e, a) =
    (conf, kind, node, e, MCPAccess.A.relift a)
end
module AS =
struct
  include SetDomain.Make (A)

  let max_conf accs =
    accs |> elements |> List.map A.conf |> (List.max ~cmp:Int.compare)
end
module T =
struct
  include Printable.StdLeaf
  type t = acc_typ [@@deriving eq, ord, hash]

  let name () = "acc_typ"

  let pretty = d_acct
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end
module O =
struct
  include Printable.StdLeaf
  type t = offs [@@deriving eq, ord, hash]

  let name () = "offs"

  let pretty = d_offs
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end
module LV = Printable.Prod (CilType.Varinfo) (O)
module LVOpt = Printable.Option (LV) (struct let name = "NONE" end)


(* Check if two accesses may race and if yes with which confidence *)
let may_race (conf,(kind: AccessKind.t),loc,e,a) (conf2,(kind2: AccessKind.t),loc2,e2,a2) =
  if kind = Read && kind2 = Read then
    false (* two read/read accesses do not race *)
  else if not (get_bool "ana.race.free") && (kind = Free || kind2 = Free) then
    false
  else if not (MCPAccess.A.may_race a a2) then
    false (* analysis-specific information excludes race *)
  else
    true

let group_may_race accs =
  (* BFS to traverse one component with may_race edges *)
  let rec bfs' accs visited todo =
    let accs' = AS.diff accs todo in
    let todo' = AS.fold (fun acc todo' ->
        AS.fold (fun acc' todo' ->
            if may_race acc acc' then
              AS.add acc' todo'
            else
              todo'
          ) accs' todo'
      ) todo (AS.empty ())
    in
    let visited' = AS.union visited todo in
    if AS.is_empty todo' then
      (accs', visited')
    else
      (bfs' [@tailcall]) accs' visited' todo'
  in
  let bfs accs acc = bfs' accs (AS.empty ()) (AS.singleton acc) in
  (* repeat BFS to find all components *)
  let rec components comps accs =
    if AS.is_empty accs then
      comps
    else (
      let acc = AS.choose accs in
      let (accs', comp) = bfs accs acc in
      let comps' = comp :: comps in
      components comps' accs'
    )
  in
  components [] accs

let race_conf accs =
  assert (not (AS.is_empty accs)); (* group_may_race should only construct non-empty components *)
  if AS.cardinal accs = 1 then ( (* singleton component *)
    let acc = AS.choose accs in
    if may_race acc acc then (* self-race *)
      Some (A.conf acc)
    else
      None
  )
  else
    Some (AS.max_conf accs)

let is_all_safe = ref true

(* Commenting your code is for the WEAK! *)
let incr_summary safe vulnerable unsafe (lv, ty) grouped_accs =
  (* ignore(printf "Checking safety of %a:\n" d_memo (ty,lv)); *)
  let safety =
    grouped_accs
    |> List.filter_map race_conf
    |> (function
        | [] -> None
        | confs -> Some (List.max confs)
      )
  in
  match safety with
  | None -> incr safe
  | Some n when n >= 100 -> is_all_safe := false; incr unsafe
  | Some n -> is_all_safe := false; incr vulnerable

let print_accesses (lv, ty) grouped_accs =
  let allglobs = get_bool "allglobs" in
  let race_threshold = get_int "warn.race-threshold" in
  let msgs race_accs =
    let h (conf,kind,node,e,a) =
      let d_msg () = dprintf "%a with %a (conf. %d)" AccessKind.pretty kind MCPAccess.A.pretty a conf in
      let doc = dprintf "%t  (exp: %a)" d_msg d_exp e in
      (doc, Some (Messages.Location.Node node))
    in
    AS.elements race_accs
    |> List.map h
  in
  grouped_accs
  |> List.fold_left (fun safe_accs accs ->
      match race_conf accs with
      | None ->
        AS.union safe_accs accs (* group all safe accs together for allglobs *)
      | Some conf ->
        let severity: Messages.Severity.t =
          if conf >= race_threshold then
            Warning
          else
            Info
        in
        M.msg_group severity ~category:Race "Memory location %a (race with conf. %d)" d_memo (ty,lv) conf (msgs accs);
        safe_accs
    ) (AS.empty ())
  |> (fun safe_accs ->
      if allglobs && not (AS.is_empty safe_accs) then
        M.msg_group Success ~category:Race "Memory location %a (safe)" d_memo (ty,lv) (msgs safe_accs)
    )

let warn_global safe vulnerable unsafe g accs =
  let grouped_accs = group_may_race accs in (* do expensive component finding only once *)
  incr_summary safe vulnerable unsafe g grouped_accs;
  print_accesses g grouped_accs
