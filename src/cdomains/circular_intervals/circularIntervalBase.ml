open CircularIntOps
open IntervalOps

(* CircularIntervalBase:
 * 
 * Defines all non-arithmetic/bitwise operations used in circular intervals,
 * e.g.:
 * - relative ordering
 * - element count
 * - complement
 * - pseudo-join/pseudo-meet
 * - ...
 * This is done mainly for better code structure and the possiblity to test
 * internal operations separately from the exposed API of the CInterval module. *)
module CircularIntervalBase (C : CircularIntOps) =
  struct
    include IntervalBase(C);;

    (* Ordering (Section 3)
     * Which element does a (going clockwise) see first: b or c? *)
    let relative_compare f w a b c = f (C.sub w b a) (C.sub w c a);;
    let relative_leq = relative_compare C.leq;;
    let relative_lt = relative_compare C.lt;;
    let relative_gt = relative_compare C.gt;;
    let relative_geq = relative_compare C.geq;;

    (* Count Interval Elements (Section 3)
     *   #(a,b) = b-a+1
     * +/- may wrap around. *)
    let count x =
      match x with 
      | Bot _ -> C.zero
      | Top w -> C.top_value w
      | Int(w,a,b) ->
          let c = C.inc w (C.sub w b a) in
          if C.eq C.zero c
          then C.top_value w
          else c;;
    
    (* Element Inclusion (Section 3.1)
     * Straightforward: Does e lie between the (clockwisely defined)
     * interval boundaries? *)
    let contains_element i e =
      match i with
      | Top _ -> true
      | Bot _ -> false
      | Int (w,a,b) -> relative_leq w a e b;;

    let contains_two_elements i e1 e2 =
      (contains_element i e1) && (contains_element i e2);;

    (* Interval Inclusion (Section 3.1)
     *   s = (a,b) \in t = (c,d) -> a \in t and b \in t and
     *                              not (c \in s and d \in s)
     * NOTE: The paper does not include identity in the formal 
     * definition of the inclusion. But it is done here. *)
    let contains t s =
      match s,t with
      | Bot _, _ -> true
      | _, Top _ -> true
      | Top _, _ -> false
      | _, Bot _ -> false
      | Int(_,a,b), Int(_,c,d) -> 
          if (eql s t)
          then true (* Identity! *)
          else (contains_two_elements t a b) && not (contains_two_elements s c d);;

    (* South Pole Crossing (Fig. 3)
     * NOTE: The paper defines this using relative_leq; with a = 0L
     * and b = end of interval and c = start of interval; The problem is,
     * that e.g. relative_leq 0L 3L 3L = true. Thus, the right operation
     * here is relative_lt.
     * This is needed for 'extend'. *)
    let crosses_south_pole x =
      match x with 
      | Top _ -> true
      | Bot _ -> false
      | Int(w,a,b) -> relative_lt w C.zero b a;;

    (* Poles/Hemispheres (Section 3.2) *)
    let north_pole_end w = C.shift_left C.one (w - 1);;
    let north_pole_start w = C.dec w (north_pole_end w);;
    let north_pole w = of_t w (north_pole_start w) (north_pole_end w);;

    let south_pole_end w = C.zero;;
    let south_pole_start w = C.max_value w;;
    let south_pole w = of_t w (south_pole_start w) (south_pole_end w);;

    let left_hemisphere w = of_t w (south_pole_end w) (north_pole_start w);;
    let right_hemisphere w = of_t w (north_pole_end w) (south_pole_start w);;

    let north_pole_split s =
      match s with
      | Bot _ -> []
      | Top w -> [left_hemisphere w; right_hemisphere w]
      | Int(w,a,b) ->
          if contains s (north_pole w)
          then [of_t w a (north_pole_start w); of_t w (north_pole_end w) b]
          else [s];;

    let south_pole_split s =
      match s with
      | Bot _ -> []
      | Top w -> [left_hemisphere w; right_hemisphere w]
      | Int(w,a,b) ->
          if contains s (south_pole w)
          then [of_t w a (south_pole_start w); of_t w (south_pole_end w) b]
          else [s];;

    let cut_spheres s =
      let nspheres = north_pole_split s in
      List.fold_left 
        (fun spheres u -> (south_pole_split u) @ spheres)
        [] nspheres;;

    (* Complement Interval (Section 3)
     *   complement((a,b)) = (b+1,a-1) 
     * +/- may wrap around. *)
    let complement x = 
      match x with 
      | Top w -> Bot w
      | Bot w -> Top w
      | Int (w,a,b) -> of_t w (C.inc w b) (C.dec w a);;

    (* Biased Pseudo-Join (Section 3.1)
     * Either one of the intervals is included in the other one
     *  -> take the larger one;
     * or the both intervals cover the whole circle
     *  -> return Top;
     * or they overlap (either left or right)
     *  -> merge intervals;
     * or the intervals are disjunctive
     *  -> merge intervals by filling the smaller one of the gaps.
     * *)
    let span_whole_circle s t =
      match s,t with
      | Int(_,a,b), Int(_,c,d) ->
          (contains_two_elements t a b) && (contains_two_elements s c d)
      | Top _, _ -> true
      | _, Top _ -> true
      | _ -> false;;

    let overlaps s t =
      match s,t with
      | Int(_,a,b), Int(_,c,d) ->
          (contains_element t b) && (contains_element s c)
      | _ -> false;;

    let have_inner_gap s t =
      match s,t with
      | Int(w0,a,b), Int(w1,c,d) ->
          let w = max w0 w1 in
          let cbc = count (Int(w,b,c)) and cda = count (Int(w,d,a)) in
          C.lt cbc cda || (C.eq cbc cda && (C.leq a c))
      | _ -> false;;

    let join s t =
      if contains t s then t 
      else if contains s t then s 
      else if span_whole_circle s t then Top (max (width s) (width t))
      else
        match s,t with
        | Int(w0,a,b), Int(w1,c,d) ->
            let w = max w0 w1 in
            if overlaps s t then of_t w a d
            else if overlaps t s then of_t w c b
            else if have_inner_gap s t then of_t w a d
            else of_t w c b
        | _ -> Bot (width s);;

    (* Pseudo-Meet (Section 3.1)
     * Definition using Pseudo-Join and De-Morgan. *)
    let meet s t = complement (join (complement s) (complement t));;

    (* Least Upper Bound (Fig. 3) *)
    let bigger s t = 
      if (C.gt (count t) (count s))
      then t
      else s;;

    let gap s t =
      match s,t with
      | Int(w0,a,b), Int(w1,c,d) ->
          let w = max w0 w1 in
          if (not (contains_element t b)) && (not (contains_element s c))
          then complement (of_t w c b)
          else Bot w
      | _ -> Bot (max (width s) (width t));;

    let extend s t =
      if contains t s then t
      else if contains s t then s
      else if span_whole_circle s t then Top (max (width s) (width t))
      else
        match s,t with
        | Int (w0,a,b), Int (w1,c,d) ->
            let w = max w0 w1 in
            if overlaps t s then of_t w c b
            else of_t w a d
        | _ -> Bot (max (width s) (width t));;

    let sort_intervals =
      let compare_intervals s t = 
        match s,t with
        | Bot _, _ -> (-1)
        | _, Bot _ -> 1
        | Top _, _ -> 1
        | _, Top _ -> (-1)
        | Int(_,a,_), Int(_,c,_) -> C.compare a c
      in
      List.sort compare_intervals;;

    let least_upper_bound w intervals = 
      let f_helper f s = if crosses_south_pole s then extend f s else f
      and fg_helper (g,f) s = (bigger g (gap f s), extend f s)
      and sorted_intervals = sort_intervals intervals in
      let f0 = List.fold_left f_helper (Bot w) sorted_intervals in
      let (g,f) = List.fold_left fg_helper (Bot w,f0) sorted_intervals in
      complement (bigger g (complement f));;
    
    (* Intersection (Section 3.1) 
     * NOTE: The paper specifies (b,c) as result of overlapping
     * of s = (a,b) with t = (c,d) but seeing as this means
     * 
     *        a |-------------| b
     *                 c |----------------| d
     *
     * it should be (c,b). Same goes for spanning the whole circle. *)
    let intersect s t =
      match s,t with
      | Bot _, _ -> []
      | _, Bot _ -> []
      | Top _, _ -> [t]
      | _, Top _ -> [s]
      | Int(w0,a,b), Int(w1,c,d) ->
          let w = max w0 w1 in
          if (eql s t) then [t]
          else if span_whole_circle s t then [of_t w a d; of_t w c b]
          else if contains t s then [s]
          else if contains s t then [t]
          else if overlaps t s then [of_t w a d]
          else if overlaps s t then [of_t w c b]
          else [];;
  end
