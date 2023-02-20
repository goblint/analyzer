open Batteries
open GoblintCil
open Pretty
open GobConfig

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
  | Some (v,os) ->
    try isFunctionType v.vtype || is_ignorable_type v.vtype
    with Not_found -> false

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

let reset () =
  Hashtbl.clear typeVar;
  Hashtbl.clear typeIncl


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



type var_o = varinfo option
type off_o = offset  option

let get_val_type e (vo: var_o) (oo: off_o) : acc_typ =
  match Cilfacade.typeOf e with
  | t ->
    begin match vo, oo with
      | Some v, Some o -> get_type t (AddrOf (Var v, o))
      | Some v, None -> get_type t (AddrOf (Var v, NoOffset))
      | _ -> get_type t e
    end
  | exception (Cilfacade.TypeOfError _) -> get_type voidType e

let add_one side (e:exp) (kind:AccessKind.t) (conf:int) (ty:acc_typ) (lv:(varinfo*offs) option) a: unit =
  if is_ignorable lv then () else begin
    let loc = Option.get !Node.current_node in
    side ty lv (conf, kind, loc, e, a)
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

let add_struct side (e:exp) (kind:AccessKind.t) (conf:int) (ty:acc_typ) (lv: (varinfo * offs) option) a: unit =
  let rec dist_fields ty =
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
    begin try
        let oss = dist_fields (type_from_type_offset ty) in
        List.iter (fun os -> add_one side e kind conf (`Struct (s,addOffs os2 os)) (add_lv os) a) oss
      with Failure _ ->
        add_one side e kind conf ty lv a
    end
  | _ when lv = None && !unsound ->
    (* don't recognize accesses to locations such as (long ) and (int ). *)
    ()
  | _ ->
    add_one side e kind conf ty lv a

let add_propagate side e kind conf ty ls a =
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
      | _ -> failwith "add_propagate: no field found"
    in
    let ts = typeSig (TComp (fi.fcomp,[])) in
    let vars = Hashtbl.find_all typeVar ts in
    (* List.iter (fun v -> ignore (printf " * %s : %a" v.vname d_typsig ts)) vars; *)
    let add_vars v = add_struct side e kind conf (`Struct (fi.fcomp, f)) (Some (v, f)) a in
    List.iter add_vars vars;
    add_struct side e kind conf (`Struct (fi.fcomp, f)) None a;
  in
  let just_vars t v =
    add_struct side e kind conf (`Type t) (Some (v, `NoOffset)) a;
  in
  add_struct side e kind conf ty None a;
  match ty with
  | `Struct (c,os) when only_fields os && os <> `NoOffset ->
    (* ignore (printf "  * type is a struct\n"); *)
    struct_inv  os
  | _ ->
    (* ignore (printf "  * type is NOT a struct\n"); *)
    let t = type_from_type_offset ty in
    let incl = Hashtbl.find_all typeIncl (typeSig t) in
    List.iter (fun fi -> struct_inv (`Field (fi,`NoOffset))) incl;
    let vars = Hashtbl.find_all typeVar (typeSig t) in
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
  | Const _
  | SizeOf _
  | SizeOfStr _
  | AlignOf _
  | AddrOfLabel _ ->
    ()

let add side e kind conf vo oo a =
  let ty = get_val_type e vo oo in
  (* let loc = !Tracing.current_loc in *)
  (* ignore (printf "add %a %b -- %a\n" d_exp e w d_loc loc); *)
  match vo, oo with
  | Some v, Some o -> add_struct side e kind conf ty (Some (v, remove_idx o)) a
  | _ ->
    if !unsound && isArithmeticType (type_from_type_offset ty) then
      add_struct side e kind conf ty None a
    else
      add_propagate side e kind conf ty None a


(* Access table as Lattice. *)
(* (varinfo ->) offset -> type -> 2^(confidence, write, loc, e, acc) *)
module A =
struct
  include Printable.Std
  type t = int * AccessKind.t * Node.t * CilType.Exp.t * MCPAccess.A.t [@@deriving eq, ord, hash]

  let pretty () (conf, kind, node, e, lp) =
    Pretty.dprintf "%d, %a, %a, %a, %a" conf AccessKind.pretty kind CilType.Location.pretty (Node.location node) CilType.Exp.pretty e MCPAccess.A.pretty lp

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let conf (conf, _, _, _, _) = conf
end
module AS =
struct
  include SetDomain.Make (A)

  let max_conf accs =
    accs |> elements |> List.map A.conf |> (List.max ~cmp:Int.compare)
end
module T =
struct
  include Printable.Std
  type t = acc_typ [@@deriving eq, ord, hash]

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
  include Printable.Std
  type t = offs [@@deriving eq, ord, hash]

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
