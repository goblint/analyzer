open CircularIntOps
open CircularIntervalBase
open IntervalOps
open Warren
open Utilities

module CircularInterval (C : CircularIntOps) : IntervalOps with type t = C.t =
  struct
    include CircularIntervalBase(C);;

    (* Helper Modules *)
    module W = Warren(C);;
    module U = Utilities;;

    (* Addition/Substraction (Section 3.2) *)
    let add x y =
      match x,y with
      | Bot w, _ -> Bot w
      | _, Bot w -> Bot w
      | Top w, _ -> Top w
      | _, Top w -> Top w
      | Int(w0,a,b), Int(w1,c,d) -> 
          let w = max w0 w1 in
          Int (w, C.add w a c, C.add w b d);;

    let sub x y =
      match x,y with
      | Bot w, _ -> Bot w
      | _, Bot w -> Bot w
      | Top w, _ -> Top w
      | _, Top w -> Top w
      | Int(w0,a,b), Int(w1,c,d) -> 
          let w = max w0 w1 in
          Int (w, C.sub w a d, C.sub w b c);;
    
    (* Negation *)
    let neg x = sub (of_t (width x) C.zero C.zero) x;;

    (* Multiplication (Section 3.2) *)
    let mul_is_top w a c b d =
      C.geq (C.sub' (C.mul' b d) (C.mul' a c)) (C.top_value w);;

    let msb w x =
      if (C.gt x (north_pole_start w))
      then 1
      else 0;;

    let mul_u s t =
      match s,t with 
      | Bot w, _ -> Bot w
      | _, Bot w -> Bot w
      | Top w, _ -> Top w
      | _, Top w -> Top w
      | Int(w0,a,b), Int(w1,c,d) ->
          let w = max w0 w1 in
          if mul_is_top w a c b d then Top w
          else of_t w (C.mul w a c) (C.mul w b d);;

    let mul_s s t =
      match s,t with
      | Bot w, _ -> Bot w
      | _, Bot w -> Bot w 
      | Top w, _ -> Top w
      | _, Top w -> Top w
      | Int(w0,a,b), Int(w1,c,d) ->
          let w = max w0 w1 in
          let ma = msb w a and mb = msb w b and mc = msb w c and md = msb w d
          and build_interval la lb lc ld =
                if mul_is_top w la lb lc ld then Top w
                else of_t w (C.mul w la lb) (C.mul w lc ld)
          in
          match ma,mb,mc,md with
          | 0,0,0,0 -> build_interval a c b d
          | 1,1,1,1 -> build_interval a c b d
          | 1,1,0,0 -> build_interval a d b c
          | 0,0,1,1 -> build_interval b c a d
          | _ -> Top w;;

    let mul_us s t =
      intersect (mul_u s t) (mul_s s t);;

    let mul s t = 
      let spheres_s = cut_spheres s
      and spheres_t = cut_spheres t in
      least_upper_bound 
        (max (width s) (width t))
        (List.flatten 
          (U.cartesian_map mul_us spheres_s spheres_t));;

    (* Division/Modulo
     * Logic taken from C++ Code at 
     * http://code.google.com/p/wrapped-intervals/source/browse/trunk/lib/RangeAnalysis/WrappedRange.cpp *)

    (* NOTE 1: Evaluate if it really is enough to only look at the interval bounds
     * to determine the outcome of Modulo.
     * NOTE 2: Division by Zero is replaced with divison by One (if the interval
     * is not [0;0]). IMHO this should not be done, but the results of the
     * divisions in question should be discarded. *)

    let calc_fn =
      let zero_range w = of_t w C.zero C.zero in
      let calc w s t =
        let bs = bounds s and bt = bounds t in
        match bs,bt with
        | None, _ -> Bot w
        | _, None -> Bot w
        | Some (a,b), Some(c,d) ->
            let c = if C.eq c C.zero then C.one else c
            and d = if C.eq d C.zero then (C.max_value w) else d in
            let calc_results = U.cartesian_map (C.div w) [a;b] [c;d] in
            of_t w (C.min calc_results) (C.max calc_results)
      in
      fun split_fn s t ->
        let w = max (width s) (width t) in
        if s = (zero_range w) then s
        else if t = (zero_range w) then Top w 
        else
          let sp_s = split_fn s and sp_t = split_fn t in
          least_upper_bound w (U.cartesian_map (calc w) sp_s sp_t);;

    let div_s = calc_fn north_pole_split;;
    let div_u = calc_fn south_pole_split;;
    
    (* Modulo *)
    let rem s t =
      let w = max (width s) (width t) in
      match s,t with
      | Bot _, _ -> Bot w
      | _, Bot _ -> Bot w
      | Int(_,a,b), Int(_,c,d) ->
          if (C.eq a b) && (C.eq c d)
          then 
            let x = C.rem w a c in
            of_t w x x
          else Top w
      | _ -> Top w
    
    (* Logical Operations (Section 3.2) *)
    let log_fn f =
      fun s t ->
        let spheres_s = south_pole_split s 
        and spheres_t = south_pole_split t in
        least_upper_bound
          (max (width s) (width t))
          (U.cartesian_map f spheres_s spheres_t);;
    
    let logor = log_fn W.interval_or;;
    let logand = log_fn W.interval_and;;
    let logxor = log_fn W.interval_xor;;

    (* Truncation (Section 3.2)
     * Returns Top if the element count before truncation exceeds
     * the maximum count after truncation? *)
    let trunc_t x k =
      let msk = C.sub' (C.shift_left C.one k) C.one in
      C.logand x msk;;

    let trunc s k =
      if k >= (width s)
      then s
      else 
        match s with
        | Bot _ -> Bot k
        | Top _ -> Top k
        | Int(w,a,b) ->
            let ra = C.arith_shift_right w a k
            and rb = C.arith_shift_right w b k
            and ta = trunc_t a k
            and tb = trunc_t b k in
            if (C.eq ra rb) && (C.leq ta tb) then 
              (* higher bits match; a's lower bits are less than b's
               * -> possibly not Top *)
              of_t k ta tb
            else 
              if (C.eq (C.rem' (C.add' ra C.one) (C.top_value k)) rb) && (C.gt ta tb) then 
                (* a's higher bits are one less than b's; a's lower ones are
                 * greater than b's -> possibly not Top *)
                of_t k ta tb
              else Top k;;

    (* Shifting by k (Section 3.2) *)
    let one_bits_right w ones =
      if ones <= 0 then C.zero
      else if ones >= w then (C.max_value w)
      else C.shift_right (C.max_value w) (w - ones);;

    let zero_bits_right w zeros =
      if zeros <= 0 then (C.max_value w)
      else if zeros >= w then C.zero
      else C.wrap w (C.shift_left (C.max_value w) zeros);; 

    let shift_left_k s k =
      let w = (width s) in
      if k > w then of_t w C.zero C.zero
      else if k = 0 then s
      else
        match s with
        | Bot w -> Bot w
        | _ -> 
            let st = trunc s (w - k) in
            match st with 
            | Int(_,a,b) -> of_t w (C.shift_left a k) (C.shift_left b k)
            | _ -> of_t w C.zero (zero_bits_right w k);;

    let shift_right_k s k =
      let w = (width s) in
      if k >= w then of_t w C.zero C.zero
      else if k = 0 then s
      else 
        let default_result = of_t w C.zero (one_bits_right w (w - k)) in
        match s with
        | Bot _ -> Bot w
        | Top _ -> default_result
        | Int(_,a,b) -> 
            if contains s (south_pole w) then
              default_result
            else
              of_t w (C.shift_right a k) (C.shift_right b k);;
   
    let arith_shift_right_k s k = 
      (* NOTE: Why should (0111,1001) >>a 1 be Top? 
       * Arith. shift should probably create (1^(k+1)0^(w-k-1);
       * 0^(k+1)1^(w-k-1)) *)
      let w = (width s) in
      if k = 0 then s
      else
        let default_result = 
              of_t w 
                (zero_bits_right w (w - k - 1)) 
                (one_bits_right w (w - k - 1)) in
        match s with
        | Bot _ -> Bot w
        | Top _ -> default_result
        | Int(_,a,b) -> 
            if contains s (north_pole w) then
              default_result
            else
              of_t w (C.arith_shift_right w a k) (C.arith_shift_right w b k);;
    
    (* Variable Shifting (Section 3.2) *)
    let variable_shift f s t = 
      let w = max (width s) (width t) in
      let possible_ks = U.range w in
      match s,t with
      | Bot _, _ -> Bot w
      | _, Bot _ -> Bot w
      | _ ->
          let ks =
            match t with 
            | Int(_,a,b) -> 
                List.filter
                  (fun k -> contains_element t (C.of_int w k))
                  possible_ks
            | _ -> possible_ks
          in
          least_upper_bound w (List.map (f s) ks);;

    let shift_left = variable_shift shift_left_k;;
    let shift_right = variable_shift shift_right_k;;
    let arith_shift_right = variable_shift arith_shift_right_k;;
  end

(* Predefined Interval Types *)
module CNInt = CircularInterval(CircularNInt);;
module CInt32 = CircularInterval(CircularInt32);;
module CInt64 = CircularInterval(CircularInt64);;
module CBigInt = CircularInterval(CircularBigInt);;
