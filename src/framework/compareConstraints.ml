open Batteries
open Analyses
open Goblint_constraint.ConstrSys
open GobConfig


module CompareGlobSys (SpecSys: ComparableSpecSys) =
struct
  open SpecSys
  module Sys = EQSys
  module LH = LHT
  module GH = GHT

  open Spec
  module G = Sys.G

  module PP = Hashtbl.Make (Node)

  let compare_globals g1 g2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      let v2 = try GH.find g2 k with Not_found -> G.bot () in
      let b1 = G.leq v1 v2 in
      let b2 = G.leq v2 v1 in
      if b1 && b2 then
        f_eq ()
      else if b1 then begin
        if get_bool "dbg.compare_runs.diff" then
          Logs.info "Global %a is more precise using left:\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v2,v1);
        f_le ()
      end else if b2 then begin
        if get_bool "dbg.compare_runs.diff" then
          Logs.info "Global %a is more precise using right:\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v1,v2);
        f_gr ()
      end else begin
        if get_bool "dbg.compare_runs.diff" then (
          Logs.info "Global %a is incomparable (diff):\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v1,v2);
          Logs.info "Global %a is incomparable (reverse diff):\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v2,v1);
        );
        f_uk ()
      end
    in
    GH.iter f g1;
    Logs.info "globals:\tequal = %d\tleft = %d\tright = %d\tincomparable = %d" !eq !le !gr !uk

  let compare_locals l1 l2 =
    let one_ctx (node,_) v h =
      PP.replace h node (try D.join v (PP.find h node) with Not_found -> v);
      h
    in
    (* these contain results where the contexts per node have been joined *)
    let h1 = PP.create 113 in
    let h2 = PP.create 113 in
    let _  = LH.fold one_ctx l1 h1 in
    let _  = LH.fold one_ctx l2 h2 in

    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f k v1 =
      if PP.mem h2 k then
        let v2 = PP.find h2 k in
        let b1 = D.leq v1 v2 in
        let b2 = D.leq v2 v1 in
        if b1 && b2 then
          incr eq
        else if b1 then begin
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a @@ %a is more precise using left:\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v2,v1);
          incr le
        end else if b2 then begin
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a @@ %a is more precise using right:\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v1,v2);
          incr gr
        end else begin
          if get_bool "dbg.compare_runs.diff" then (
            Logs.info "%a @@ %a is incomparable (diff):\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v1,v2);
            Logs.info "%a @@ %a is incomparable (reverse diff):\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v2,v1);
          );
          incr uk
        end
    in
    PP.iter f h1;
    (* let k1 = Set.of_enum @@ PP.keys h1 in
       let k2 = Set.of_enum @@ PP.keys h2 in
       let o1 = Set.cardinal @@ Set.diff k1 k2 in
       let o2 = Set.cardinal @@ Set.diff k2 k1 in
       Logs.info "locals: \tequal = %d\tleft = %d[%d]\tright = %d[%d]\tincomparable = %d" !eq !le o1 !gr o2 !uk *)
    Logs.info "locals: \tequal = %d\tleft = %d\tright = %d\tincomparable = %d" !eq !le !gr !uk

  let compare_locals_ctx h1 h2 =
    let eq, le, gr, uk, no2, no1 = ref 0, ref 0, ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      if not (LH.mem h2 k) then incr no2 else
        let v2 = LH.find h2 k in
        let b1 = D.leq v1 v2 in
        let b2 = D.leq v2 v1 in
        if b1 && b2 then
          f_eq ()
        else if b1 then begin
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a is more precise using left:\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v2,v1);
          f_le ()
        end else if b2 then begin
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a is more precise using right:\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v1,v2);
          f_gr ()
        end else begin
          if get_bool "dbg.compare_runs.diff" then (
            Logs.info "%a is incomparable (diff):\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v1,v2);
            Logs.info "%a is incomparable (reverse diff):\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v2,v1);
          );
          f_uk ()
        end
    in
    LH.iter f h1;
    let f k v2 =
      if not (LH.mem h1 k) then incr no1
    in
    LH.iter f h2;
    (* let k1 = Set.of_enum @@ PP.keys h1 in *)
    (* let k2 = Set.of_enum @@ PP.keys h2 in *)
    (* let o1 = Set.cardinal @@ Set.diff k1 k2 in *)
    (* let o2 = Set.cardinal @@ Set.diff k2 k1 in *)
    Logs.info "locals_ctx:\tequal = %d\tleft = %d\tright = %d\tincomparable = %d\tno_ctx_in_right = %d\tno_ctx_in_left = %d" !eq !le !gr !uk !no2 !no1

  let compare (name1,name2) (l1,g1) (l2,g2) =
    Logs.newline ();
    Logs.info "Comparing GlobConstrSys precision of %s (left) with %s (right):" name1 name2;
    compare_globals g1 g2;
    compare_locals l1 l2;
    compare_locals_ctx l1 l2;
    Logs.newline ();
end

module CompareHashtbl (Var: VarType) (Dom: Lattice.S) (VH: Hashtbl.S with type key = Var.t) =
struct
  module Var =
  struct
    include Printable.Std
    include Var
    let name () = "var"

    let pretty = pretty_trace
    include Printable.SimplePretty (
      struct
        type nonrec t = t
        let pretty = pretty
      end
      )
  end

  include PrecCompare.MakeHashtbl (Var) (Dom) (VH)
end

module CompareEqSys (Sys: EqConstrSys) (VH: Hashtbl.S with type key = Sys.Var.t) =
struct
  module Compare = CompareHashtbl (Sys.Var) (Sys.Dom) (VH)

  let compare (name1, name2) vh1 vh2 =
    Logs.newline ();
    Logs.info "Comparing EqConstrSys precision of %s (left) with %s (right):" name1 name2;
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1 ~name2 vh2 in
    Logs.info "EqConstrSys comparison summary: %t" (fun () -> msg);
    Logs.newline ();
end

module CompareGlobal (GVar: VarType) (G: Lattice.S) (GH: Hashtbl.S with type key = GVar.t) =
struct
  module Compare = CompareHashtbl (GVar) (G) (GH)

  let compare (name1, name2) vh1 vh2 =
    Logs.newline ();
    Logs.info "Comparing globals precision of %s (left) with %s (right):" name1 name2;
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1 ~name2 vh2 in
    Logs.info "Globals comparison summary: %t" (fun () -> msg);
    Logs.newline ();
end

module CompareNode (C: Printable.S) (D: Lattice.S) (LH: Hashtbl.S with type key = VarF (C).t) =
struct
  module Node =
  struct
    include Node
    let var_id _ = "nodes"
    let node x = x
    let is_write_only _ = false
  end
  module NH = Hashtbl.Make (Node)

  module Compare = CompareHashtbl (Node) (D) (NH)

  let join_contexts (lh: D.t LH.t): D.t NH.t =
    let nh = NH.create 113 in
    LH.iter (fun (n, _) d ->
        let d' = try D.join (NH.find nh n) d with Not_found -> d in
        NH.replace nh n d'
      ) lh;
    nh

  let compare (name1, name2) vh1 vh2 =
    Logs.newline ();
    Logs.info "Comparing nodes precision of %s (left) with %s (right):" name1 name2;
    let vh1' = join_contexts vh1 in
    let vh2' = join_contexts vh2 in
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1' ~name2 vh2' in
    Logs.info "Nodes comparison summary: %t" (fun () -> msg);
    Logs.newline ();
end
