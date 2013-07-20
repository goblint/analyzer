open CircularIntOps
open IntervalOps

(* Warren:
 *
 * Defines bitwise operations on Intervals as proposed by Warren,
 * e.g. logical OR with complexity O(width of bitvectors).
 * 
 * Source: Henry S. Warren - Hacker's Delight *)
module Warren (C : CircularIntOps) =
  struct
    module I = IntervalBase(C);;

    (* Helpers *)
    let logand3 a b c =
      C.logand (C.logand a b) c;;

    let interval_fn min_fn max_fn =
      fun u v ->
        match u,v with
        | Bot w, _ -> Bot w
        | _, Bot w -> Bot w
        | Top w, _ -> Top w
        | _, Top w -> Top w
        | Int(w0,a,b), Int(w1,c,d) ->
            let w = max w0 w1 in
            I.of_t w
              (min_fn w a b c d)
              (max_fn w a b c d);;

    (* Warren's OR: p. 75f, figs. 4-3/4-4 *)
    let min_or w a b c d =
      let rec loop m a c =
        if C.eq m C.zero then C.logor a c
        else if C.gt (logand3 (C.lognot w a) c m) C.zero
        then 
          let tmp = C.logand (C.logor a m) (C.lognot w m) in
          if C.leq tmp b
          then loop C.zero tmp c
          else loop (C.shift_right m 1) a c
        else if C.gt (logand3 a (C.lognot w c) m) C.zero
        then
          let tmp = C.logand (C.logor c m) (C.lognot w m) in
          if C.leq tmp d
          then loop C.zero a tmp
          else loop (C.shift_right m 1) a c
        else loop (C.shift_right m 1) a c
      in
      loop (C.shift_right (C.top_value w) 1) a c;;

    let max_or w a b c d =
      let rec loop m b d =
        if C.eq m C.zero then C.logor b d
        else if C.gt (logand3 b d m) C.zero 
        then 
          let tmp = C.logor (C.sub' b m) (C.sub' m C.one) in
          if C.geq tmp a
          then loop C.zero tmp d
          else
            let tmp = C.logor (C.sub' d m) (C.sub' m C.one) in
            if C.geq tmp c
            then loop C.zero b tmp
            else loop (C.shift_right m 1) b d
        else loop (C.shift_right m 1) b d
      in
      loop (C.shift_right (C.top_value w) 1) b d;;
    
    let interval_or = interval_fn min_or max_or;;

    (* Warren's AND: p. 76f, figs. 4-5/4-6
     * NOTE: Might be directly implemented or using DeMorgan. *)
    let min_and w a b c d =
      C.lognot w
        (max_or w
          (C.lognot w b)
          (C.lognot w a)
          (C.lognot w d)
          (C.lognot w c));;

    let max_and w a b c d =
      C.lognot w
        (min_or w
          (C.lognot w b)
          (C.lognot w a)
          (C.lognot w d)
          (C.lognot w c));;
    
    let interval_and = interval_fn min_and max_and;;

    (* Warren's XOR: p78
     * NOTE: Might be directly implemented or using AND/OR. *)
    let min_xor w a b c d = 
      C.logor
        (min_and w a b (C.lognot w d) (C.lognot w c))
        (min_and w (C.lognot w b) (C.lognot w a) c d);;

    let max_xor w a b c d =
      max_or w
        C.zero
        (max_and w a b (C.lognot w d) (C.lognot w c))
        C.zero
        (max_and w (C.lognot w b) (C.lognot w a) c d);;

    let interval_xor = interval_fn min_xor max_xor;;
  end
