(** Narrowing strategies for side-effects *)

open Batteries
open ConstrSys
open Messages



module type S = functor (S:EqConstrSys) -> functor (HM:Hashtbl.S with type key = S.v) -> functor (VS:Set.S with type elt = S.v) -> sig
  type data
  type eq_wrapper = S.v -> ((S.v -> S.d -> unit) -> S.d) -> S.d
  val active: bool
  val create_empty_data : unit -> data
  val copy_marshal: data -> data
  val relift_marshal: data -> data

  val register_start: data -> S.v -> S.d -> unit

  val get_wrapper:
    solve_widen:(S.v -> unit) ->
    init:(S.v -> unit) ->
    stable:unit HM.t ->
    data:data ->
    sides:VS.t HM.t ->
    add_sides: (S.v -> S.v -> unit) ->
    rho: S.d HM.t ->
    destabilize: (S.v -> unit) ->
    side: (?x:S.v -> S.v -> S.d -> unit) ->
    assert_can_receive_side: (S.v -> unit)
    -> eq_wrapper

end

(** Inactive *)

module Inactive:S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = unit
    type eq_wrapper = S.v -> ((S.v -> S.d -> unit) -> S.d) -> S.d
    let active = false
    let create_empty_data () = ()
    let copy_marshal _ = ()
    let relift_marshal _ = ()

    let register_start _ _ _  = ()

    let get_wrapper ~solve_widen ~init ~stable ~data ~sides ~add_sides ~rho ~destabilize ~(side:?x:S.v -> S.v -> S.d -> unit) ~assert_can_receive_side =
      (fun x eqx -> eqx (side ~x))
  end

module Narrow:S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type divided_side_mode = D_Widen | D_Narrow | D_Box [@@deriving show]

    type data = {
      prev_sides: VS.t HM.t;
      divided_side_effects: ((S.Dom.t * (int *  divided_side_mode) option) HM.t) HM.t;
      narrow_globs_start_values: S.Dom.t HM.t;
    }

    type eq_wrapper = S.v -> ((S.v -> S.d -> unit) -> S.d) -> S.d

    let active = false
    let create_empty_data () =
      let narrow_globs = GobConfig.get_bool "solvers.td3.narrow-globs.enabled" in
      {
        prev_sides = HM.create 10;
        divided_side_effects = HM.create (if narrow_globs then 10 else 0);
        narrow_globs_start_values = HM.create (if narrow_globs then 10 else 0);
      }

    let copy_marshal data =
      {
        prev_sides = HM.copy data.prev_sides;
        divided_side_effects = HM.map (fun k v -> HM.copy v) data.divided_side_effects;
        narrow_globs_start_values = HM.copy data.narrow_globs_start_values;
      }

    let relift_marshal data =
      let prev_sides = HM.create (HM.length data.prev_sides) in
      HM.iter (fun k v ->
          HM.replace prev_sides (S.Var.relift k) (VS.map S.Var.relift v)
        ) data.prev_sides;
      let divided_side_effects = HM.create (HM.length data.divided_side_effects) in
      HM.iter (fun k v ->
          let inner_copy = HM.create (HM.length v) in
          HM.iter (fun k (v, gas) -> HM.replace inner_copy (S.Var.relift k) ((S.Dom.relift v), gas)) v;
          HM.replace divided_side_effects (S.Var.relift k) inner_copy
        ) data.divided_side_effects;
      let narrow_globs_start_values = HM.create (HM.length data.narrow_globs_start_values) in
      HM.iter (fun k v ->
          HM.replace narrow_globs_start_values (S.Var.relift k) (S.Dom.relift v)
        ) data.narrow_globs_start_values;
      {
        prev_sides;
        divided_side_effects;
        narrow_globs_start_values;
      }

    let register_start data x d = HM.replace data.narrow_globs_start_values x d

    let narrow_globs_conservative_widen = GobConfig.get_bool "solvers.td3.narrow-globs.conservative-widen"
    let narrow_globs_immediate_growth = GobConfig.get_bool "solvers.td3.narrow-globs.immediate-growth"
    let narrow_globs_gas_default = GobConfig.get_int "solvers.td3.narrow-globs.narrow-gas"
    let narrow_globs_gas_default = if narrow_globs_gas_default < 0 then None else Some (narrow_globs_gas_default, D_Widen)
    let narrow_globs_eliminate_dead = GobConfig.get_bool "solvers.td3.narrow-globs.eliminate-dead"

    let get_wrapper ~solve_widen ~init ~stable ~data ~sides ~add_sides ~rho ~destabilize ~(side:?x:S.v -> S.v -> S.d -> unit) ~assert_can_receive_side =
      let eq_wrapper x eqx  =
        let rec side_acc acc changed x y d:unit =
          let new_acc = match HM.find_option acc y with
            | Some acc -> if not @@ S.Dom.leq d acc then Some (S.Dom.join acc d) else None
            | None -> Some d
          in
          Option.may (fun new_acc ->
              HM.replace acc y new_acc;
              if narrow_globs_immediate_growth then (
                let y_changed = divided_side D_Widen x y new_acc in
                if y_changed then
                  HM.replace changed y ();
              )
            ) new_acc;
        and divided_side (phase:divided_side_mode) x y d: bool =
          if tracing then trace "side" "divided side to %a from %a ## value: %a" S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty d;
          if tracing then trace "sol2" "divided side to %a from %a ## value: %a" S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty d;
          assert_can_receive_side y;
          init y;
          if tracing then trace "sol2" "stable add %a" S.Var.pretty_trace y;
          HM.replace stable y ();

          let sided = GobOption.exists (VS.mem x) (HM.find_option sides y) in
          if not sided then add_sides y x;
          if not (HM.mem data.divided_side_effects y) then HM.replace data.divided_side_effects y (HM.create 10);

          let y_sides = HM.find data.divided_side_effects y in
          let (old_side, narrow_gas) = HM.find_default y_sides x (S.Dom.bot (), narrow_globs_gas_default) in
          let phase = if phase = D_Box then
              if S.Dom.leq d old_side then D_Narrow else D_Widen
            else
              phase
          in
          if not (phase = D_Narrow && narrow_gas = Some (0, D_Widen)) then (
            let (new_side, narrow_gas) = match phase with
              | D_Widen ->
                let tmp = S.Dom.join old_side d in
                if not @@ S.Dom.equal tmp old_side then
                  let new_side =
                    if narrow_globs_conservative_widen && S.Dom.leq tmp (HM.find rho y) then
                      tmp
                    else
                      S.Dom.widen old_side tmp
                  in
                  let new_gas = Option.map (fun (x, _) -> (x, D_Widen)) narrow_gas in
                  (new_side, new_gas)
                else
                  (old_side, narrow_gas)
              | D_Narrow ->
                let result = S.Dom.narrow old_side d in
                let narrow_gas = if not @@ S.Dom.equal result old_side then
                    Option.map (fun (gas, phase) -> if phase = D_Widen then (gas - 1, D_Narrow) else (gas, phase)) narrow_gas
                  else
                    narrow_gas
                in
                (result, narrow_gas)
              | _ -> failwith "unreachable" (* handled above *)
            in

            if not (S.Dom.equal old_side new_side) then (
              if tracing then trace "side" "divided side to %a from %a changed (phase: %s) Old value: %a ## New value: %a" S.Var.pretty_trace y S.Var.pretty_trace x (show_divided_side_mode phase) S.Dom.pretty old_side S.Dom.pretty new_side;

              if S.Dom.is_bot new_side && narrow_gas = None then
                HM.remove y_sides x
              else
                HM.replace y_sides x (new_side, narrow_gas);

              let combined_side y =
                let contribs = HM.find_option data.divided_side_effects y in
                let join map = HM.fold (fun _ (value, _) acc -> S.Dom.join acc value) map (S.Dom.bot ()) in
                let combined = Option.map_default join (S.Dom.bot ()) contribs in
                let start_value = HM.find_default data.narrow_globs_start_values y (S.Dom.bot()) in
                S.Dom.join combined start_value
              in
              let y_oldval = HM.find rho y in
              let y_newval = if S.Dom.leq old_side new_side then
                  (* If new side is strictly greater than the old one, the value of y can only increase. *)
                  S.Dom.join y_oldval new_side
                else
                  combined_side y
              in
              if not (S.Dom.equal y_newval y_oldval) then (
                if tracing then trace "side" "value of %a changed by side from %a (phase: %s) Old value: %a ## New value: %a"
                    S.Var.pretty_trace y S.Var.pretty_trace x (show_divided_side_mode phase) S.Dom.pretty y_oldval S.Dom.pretty y_newval;
                HM.replace rho y y_newval;
                destabilize y;
              );
              true
            ) else
              false
          ) else
            false
        in
        let acc = HM.create 0 in
        let changed = HM.create 0 in
        Fun.protect ~finally:(fun () -> (
              if narrow_globs_eliminate_dead then begin
                let prev_sides_x = HM.find_option data.prev_sides x in
                Option.may (VS.iter (fun y ->
                    if not @@ HM.mem acc y then begin
                      ignore @@ divided_side D_Narrow x y (S.Dom.bot ());
                      if S.Dom.is_bot @@ HM.find rho y then
                        let casualties = S.postmortem y in
                        List.iter solve_widen casualties
                    end;
                  )) prev_sides_x;
                let new_sides = HM.fold (fun k _ acc -> VS.add k acc) acc VS.empty in
                if VS.is_empty new_sides then
                  HM.remove data.prev_sides x
                else
                  HM.replace data.prev_sides x new_sides;
              end;
              if narrow_globs_immediate_growth then
                HM.iter (fun y acc -> if not @@ HM.mem changed y then ignore @@ divided_side D_Narrow x y acc) acc
              else (
                HM.iter (fun y acc -> ignore @@ divided_side D_Box x y acc) acc
              )
            )) (fun () -> eqx (side_acc acc changed x))
      in
      (eq_wrapper: eq_wrapper)

  end

let choose () =
  if GobConfig.get_bool "solvers.td3.narrow-globs.enabled" then
    (module Narrow : S)
  else
    (module Inactive : S)
