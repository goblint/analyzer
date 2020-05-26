open Prelude.Ana
open Analyses
open GobConfig

module S =
struct

  let name () = "libfuns"

  include Analyses.DefaultSpec
  module D = struct

    module Key = struct
      let name () = "type prototypes"
      type t = typsig
      let equal v w = v = w
      let show = sprint d_typsig
      let compare v w = compare (show v) (show w) (* inefficient *)
      let to_yojson v = failwith "to_yojson undefined"
      include Printable.PrintSimple (struct
          type t' = t
          let name () = "read/write"
          let short i a = sprint d_typsig  a
        end)

      let hash = Hashtbl.hash
      let classify _ = 0
      let class_name _ = "no"
      let trace_enabled = false
      let invariant _ _ = None
      let tag _ = failwith "Unimplemented function tag"
      let short n t = sprint d_typsig t
    end

    module Val = struct
      module RW = struct
        let name () = "usage"
        type t = Read | Write | Spawn | Exec  [@@deriving show, to_yojson, eq, ord]
        include Printable.PrintSimple (struct
            type t' = t
            let name () = "read/write"
            let short _ = show
          end)
        let hash = Hashtbl.hash
        let invariant _ _ = None
        let tag _ = failwith "Unimplemented function tag"
        let short n t = "Short representation"
      end
      include SetDomain.Make (RW)
    end
    include MapDomain.MapBot_LiftTop (Key) (Val)
  end


  module G = Lattice.Unit
  module C = D

  let startstate v = D.bot ()
  let morphstate v d = d
  let exitstate  v = D.bot ()
  let otherstate v = D.bot ()

  let should_join _ _ = false
  let val_of = identity
  let context = identity
  (* let call_descr : fundec -> C.t -> string *)
  (* val part_access: (D.t, G.t, C.t) ctx -> exp -> varinfo option -> bool -> (Access.LSSSet.t * Access.LSSet.t) *)

  (* val sync  : (D.t, G.t, C.t) ctx -> D.t * (varinfo * G.t) list
  val query : (D.t, G.t, C.t) ctx -> Queries.t -> Queries.Result.t *)
  (* val vdecl : (D.t, G.t, C.t) ctx -> varinfo -> D.t *)
  let rec get_written_var ((host, offset): lval) = match host with
    | Mem e -> (match e with
                | Lval l -> get_written_var l
                | _ -> failwith "Keine Ahnung")
    | Var v -> v

  let rec possible_heap_pointer_from_lval ((host, offset): lval) = match host with
    | Mem e -> (match e with
                | Lval l -> possible_heap_pointer_from_lval l
                | _ -> failwith "Keine Ahnung")
    | Var v -> `Address (Base.AD.from_var (BaseDomain.get_heap_var (v.vtype |> typeSig)))

  let assign ctx lv e =
    let base = List.assoc "base" ctx.presub in
    let (cpa, f, dep) : (BaseDomain.CPA.t * BaseDomain.Flag.t * BaseDomain.PartDeps.t) = Obj.obj base in
    (* Find type of the pointer that is written here (if any)*)
    let heap_l = possible_heap_pointer_from_lval lv in
    (*let heap_r = BaseDomain.get_heap_var (e |> typeOf |> typeSig) in *)
    let left_value = BaseDomain.CPA.find (get_written_var lv) cpa in
    (* let right_value = BaseDomain.CPA.find heap_r cpa in *)

    (* Könnte das, was geschrieben wurde, ein Heappointer sein? *)
    print_endline @@ "heap_l:" ^ ValueDomain.Compound.short 100 heap_l;
    print_endline @@ "Left:" ^ ValueDomain.Compound.short 100 left_value;

    if ValueDomain.Compound.leq heap_l left_value then (
        print_endline "Heap value might be written here!"
    );

    (* print_endline @@ "Right:" ^ ValueDomain.Compound.short 100 right_value; *)
    ctx.local
  let branch ctx e b = ctx.local
  let body   ctx f = ctx.local
  let return ctx e f = ctx.local
  (* val intrpt: (D.t, G.t, C.t) ctx -> D.t
  val asm   : (D.t, G.t, C.t) ctx -> D.t *)


  let special ctx lv v exps = ctx.local
  let enter   ctx lv v exps = [ctx.local, ctx.local]
  let combine ctx lv e v exps d = d
end

let _ =
    MCP.register_analysis ~dep:["base"] (module S : Spec)
