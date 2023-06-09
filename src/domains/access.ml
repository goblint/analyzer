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

module TSH = Hashtbl.Make (CilType.Typsig)

let typeVar  = TSH.create 101
let typeIncl = TSH.create 101
let unsound = ref false

let init (f:file) =
  unsound := get_bool "ana.mutex.disjoint_types";
  let visited_vars = Hashtbl.create 100 in
  let add tsh t v =
    let rec add' ts =
      TSH.add tsh ts v;
      (* Account for aliasing to any level of array.
         See 06-symbeq/50-type_array_via_ptr_rc.c. *)
      match ts with
      | TSArray (ts', _, _) -> add' ts'
      | _ -> ()
    in
    add' (typeSig t)
  in
  let visit_field fi =
    (* TODO: is_ignorable_type? *)
    (* TODO: Direct ignoring doesn't really work since it doesn't account for pthread inner structs/unions being only reachable via ignorable types. *)
    add typeIncl fi.ftype fi
  in
  let visit_glob = function
    | GCompTag (c,_) ->
      List.iter visit_field c.cfields
    | GVarDecl (v,_) | GVar (v,_,_) ->
      if not (Hashtbl.mem visited_vars v.vid) then begin
        (* TODO: is_ignorable? *)
        add typeVar v.vtype v;
        (* ignore (printf "init adding %s : %a" v.vname d_typsig ((typeSig v.vtype))); *)
        Hashtbl.replace visited_vars v.vid true
      end
    | _ -> ()
  in
  List.iter visit_glob f.globals

let reset () =
  TSH.clear typeVar;
  TSH.clear typeIncl


type acc_typ = [ `Type of CilType.Typ.t | `Struct of CilType.Compinfo.t * Offset.Unit.t ] [@@deriving eq, ord, hash]

exception Type_offset_error

module Memo =
struct
  include Printable.StdLeaf
  type t = [`Var of CilType.Varinfo.t | `Type of CilType.Typ.t] * Offset.Unit.t [@@deriving eq, ord, hash]
  (* TODO: use typsig for `Type? *)

  let name () = "memo"

  let pretty () (vt, o) =
    (* Imitate old printing for now *)
    match vt with
    | `Var v -> Pretty.dprintf "%a%a@@%a" CilType.Varinfo.pretty v Offset.Unit.pretty o CilType.Location.pretty v.vdecl
    | `Type (TComp (c, _)) -> Pretty.dprintf "(struct %s)%a" c.cname Offset.Unit.pretty o
    | `Type t -> Pretty.dprintf "(%a)%a" CilType.Typ.pretty t Offset.Unit.pretty o

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let of_ty (ty: acc_typ): t =
    match ty with
    | `Struct (c, o) -> (`Type (TComp (c, [])), o)
    | `Type t -> (`Type t, `NoOffset)

  let to_mval: t -> Mval.Unit.t option = function
    | (`Var v, o) -> Some (v, o)
    | (`Type _, _) -> None

  let add_offset ((vt, o): t) o2: t = (vt, Offset.Unit.add_offset o o2)

  let type_of_base ((vt, _): t): typ =
    match vt with
    | `Var v -> v.vtype
    | `Type t -> t

  let type_of ((vt, o) as memo: t): typ =
    try Offset.Unit.type_of ~base:(type_of_base memo) o
    with Offset.Type_of_error _ -> raise Type_offset_error
end

(* TODO: What is the logic for get_type? *)
let rec get_type (fb: typ Lazy.t) : exp -> acc_typ = function
  | AddrOf (h,o) | StartOf (h,o) ->
    let rec f htyp =
      match htyp with
      | TComp (ci,_) -> `Struct (ci, Offset.Unit.of_cil o)
      | TNamed (ti,_) -> f ti.ttype
      | _ -> `Type (Lazy.force fb) (* TODO: Why fb not htyp? *)
    in
    begin match o with
      | Field (f, on) -> `Struct (f.fcomp, Offset.Unit.of_cil o)
      | NoOffset | Index _ ->
        begin match h with
          | Var v -> f (v.vtype)
          | Mem e -> f (Lazy.force fb) (* TODO: type of Mem doesn't have to be the fallback type if offsets present? *)
        end
    end
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ | AddrOfLabel _  ->
    `Type (uintType) (* TODO: Correct types from typeOf? *)
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
        when CilType.Compinfo.equal s1 s2 && Offset.Unit.equal o1 o2 ->
        `Struct (s1, o1)
      | _ -> `Type t
    end
  | Const _
  | Lval _
  | Real _
  | Imag _ ->
    `Type (Lazy.force fb) (* TODO: is this right? *)

let get_type fb e =
  (* printf "e = %a\n" d_plainexp e; *)
  let r = get_type fb e in
  (* printf "result = %a\n" d_acct r; *)
  match r with
  | `Type (TPtr (t,a)) -> `Type t (* Why this special case? Almost always taken if not `Struct. *)
  | x -> x (* Mostly for `Struct, but also rare cases with non-pointer `Type. Should they happen at all? *)

let get_val_type e: acc_typ =
  let fb = lazy (
    try Cilfacade.typeOf e
    with Cilfacade.TypeOfError _ -> voidType (* Why is this a suitable default? *)
  )
  in
  get_type fb e


let add_one side memo: unit =
  let mv = Memo.to_mval memo in
  let ignorable = is_ignorable mv in
  if M.tracing then M.trace "access" "add_one %a (ignorable = %B)\n" Memo.pretty memo ignorable;
  if not ignorable then
    side memo

let add_struct side memo: unit =
  if M.tracing then M.tracei "access" "add_struct %a\n" Memo.pretty memo;
  let rec dist_fields ty : Offset.Unit.t list =
    (* TODO: is_ignorable_type outside of TComp if ty itself is ignorable? *)
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
      List.map (fun x -> `Index ((), x)) (dist_fields t)
    | _ -> [`NoOffset]
  in
  begin match Memo.type_of_base memo, memo with (* based on outermost type *)
    | TComp _, _ -> (* TODO: previously just `Struct, do some `Type TComp-s also fall in here now? *)
      if M.tracing then M.trace "access" "struct case\n";
      begin match Memo.type_of memo with (* based on innermost type *)
        | t ->
          let oss = dist_fields t in
          (* 32 test(s) failed: ["02/26 malloc_struct", "04/49 type-invariants", "04/65 free_indirect_rc", "05/07 glob_fld_rc", "05/08 glob_fld_2_rc", "05/11 fldsense_rc", "05/15 fldunknown_access", "06/10 equ_rc", "06/16 type_rc", "06/21 mult_accs_rc", "06/28 symb_lockset_unsound", "06/29 symb_lockfun_unsound", "09/01 list_rc", "09/03 list2_rc", "09/05 ptra_rc", "09/07 kernel_list_rc", "09/10 arraylist_rc", "09/12 arraycollapse_rc", "09/14 kernel_foreach_rc", "09/16 arrayloop_rc", "09/18 nested_rc", "09/20 arrayloop2_rc", "09/23 evilcollapse_rc", "09/26 alloc_region_rc", "09/28 list2alloc", "09/30 list2alloc-offsets", "09/31 equ_rc", "09/35 list2_rc-offsets-thread", "09/36 global_init_rc", "29/01 race-2_3b-container_of", "29/02 race-2_4b-container_of", "29/03 race-2_5b-container_of"] *)
          List.iter (fun os ->
              add_one side (Memo.add_offset memo os)
            ) oss
        | exception Type_offset_error ->
          if M.tracing then M.trace "access" "Type_offset_error\n";
          add_one side memo
      end
    | _, (`Type _, _) when !unsound ->
      (* don't recognize accesses to locations such as (long ) and (int ). *)
      if M.tracing then M.trace "access" "unsound case\n";
      ()
    | _ ->
      if M.tracing then M.trace "access" "general case\n";
      add_one side memo
  end;
  if M.tracing then M.traceu "access" "add_struct\n"

let rec add_propagate side (t: typ) (o: Offset.Unit.t) =
  let memo = (`Type t, o) in
  if M.tracing then M.tracei "access" "add_propagate %a\n" Memo.pretty memo;
  add_struct side memo;

  let ts = typeSig t in
  let vars = TSH.find_all typeVar ts in
  List.iter (fun v ->
      add_struct side (`Var v, o)
    ) vars;

  let fields = TSH.find_all typeIncl ts in
  List.iter (fun f ->
      add_propagate side (TComp (f.fcomp, [])) (`Field (f, o))
    ) fields;

  if M.tracing then M.traceu "access" "add_propagate\n"

let add side e voffs =
  begin match voffs with
    | Some (v, o) ->
      if M.tracing then M.traceli "access" "add var %a%a\n" CilType.Varinfo.pretty v CilType.Offset.pretty o;
      let memo = (`Var v, Offset.Unit.of_cil o) in
      add_struct side memo
    | None ->
      if M.tracing then M.traceli "access" "add type %a\n" CilType.Exp.pretty e;
      let ty = get_val_type e in
      let (t, o) = match ty with
        | `Struct (c, o) -> (TComp (c, []), o)
        | `Type t -> (t, `NoOffset)
      in
      add_struct side (`Type t, o); (* TODO: this is also part of add_propagate, duplicated when called *)
      (* TODO: maybe this should not depend on whether voffs = None? *)
      if not (!unsound && isArithmeticType t) then
        add_propagate side t o
  end;
  if M.tracing then M.traceu "access" "add\n"

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
let incr_summary safe vulnerable unsafe _ grouped_accs =
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

let print_accesses memo grouped_accs =
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
        M.msg_group severity ~category:Race "Memory location %a (race with conf. %d)" Memo.pretty memo conf (msgs accs);
        safe_accs
    ) (AS.empty ())
  |> (fun safe_accs ->
      if allglobs && not (AS.is_empty safe_accs) then
        M.msg_group Success ~category:Race "Memory location %a (safe)" Memo.pretty memo (msgs safe_accs)
    )

let warn_global safe vulnerable unsafe memo accs =
  let grouped_accs = group_may_race accs in (* do expensive component finding only once *)
  incr_summary safe vulnerable unsafe memo grouped_accs;
  print_accesses memo grouped_accs
