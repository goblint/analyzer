open Goblint_constraint.ConstrSys
  (** Manage warrowing

      Widening will be delayed 'delay' times in each phase
      There will be at most 'gas' narrowing phases.
  *)
  module Warrow (L : Lattice.S) = struct
    let gas_default = ref (1,3)
    (** (value, delay, gas, narrowing_flag) 
        Narrowing flag denotes if the last update lead
        to a narrowing. This is required to maintain delay/gas values.
    *)
    type state = L.t * int * int * bool

    let warrow (current, delay, gas, narrow_flag) newval =
      let (delay0, _) = !gas_default in
      if L.equal current newval then (current, delay, gas, narrow_flag)

      else if L.leq newval current then (
        if narrow_flag then (L.narrow current newval, delay, gas, true)
        else if gas <= 0 then (current, delay, gas, false)
        else (L.narrow current newval, delay, gas - 1, true)
      )

      else (
        if narrow_flag then (L.join current newval, delay0, gas, false)
        else if delay <= 0 then (L.widen current (L.join current newval), 0, gas, false)
        else (L.join current newval, delay - 1, gas, false)
      )
  end


