open OUnit
open ArrayDomain

module I = Int64

module Idx = IntDomain.Trier
module Val = IntDomain.Trier

module S = SetDomain.Make(IntDomain.Integers)


module type S =
sig
  type t
  val get_int_d: int -> IntDomain.Trier.t
  val set_v: t -> int -> int -> t
  val get_v: t -> int -> int
  val enc  : int array -> t
end


module ArrayTestDomain 
  (D: ArrayDomain.S with type idx = Idx.t and type value = Val.t) 
: S with type t = D.t =
struct
  type t = D.t

  (* helper functions *)
  (* _v values:
     >=0-> definite
     -1 -> excluded (set not 0)
     -2 -> top 
     -3 -> bot 
  *)
  
  let get_int_d v =
    match v with
        -1 -> Idx.of_excl_list Cil.IInt [I.zero]
      | -2 -> Idx.top ()
      | -3 -> Idx.bot ()
      | _  -> Idx.of_int (I.of_int v)
  
  
  let set_t a i v =
    D.set a (Idx.of_int (I.of_int i)) v

  let set_v a i v =
    match v with
        -1 -> set_t a i (Idx.of_excl_list Cil.IInt [I.zero])
      | -2 -> set_t a i (Idx.top ())
      | -3 -> set_t a i (Idx.bot ())
      | _  -> set_t a i (Idx.of_int (I.of_int v))
  
  
  let get_v a i : int=
    let v = D.get a (Idx.of_int (I.of_int i)) in
        match v with 
  	  `Definite x -> (I.to_int x)
	  | `Excluded (x,_) when S.is_empty x -> -2
  	| _ ->  -1
	    
  let enc a =
    let b = ref (D.make (Array.length a) (Val.top ())) in
    for i = 0 to (Array.length a - 1) do
      b := set_v !b i a.(i)
    done;
    !b

end




module GeneralTests 
  (D: ArrayDomain.S with type idx = Idx.t and type value = Val.t) 
  (N: S with type t = D.t) =
struct
  
  open N

  (* tests for get/set and make*)
  let a1 = ref (enc [|1;2;3;4;5;6;7;8;9;10|])
  let a2 = ref (enc [|1;2;3;4;5;6;7;8;9;10|])
  let a3 = ref (enc [||])

  let test_equal () = 
    assert_equal ~cmp:D.equal ~printer:(D.short max_int) !a1 !a2

  exception E
  let test_nequal () = 
    try 
      assert_equal ~cmp:D.equal !a3 !a2;
      raise E
    with
        Failure _ -> ()
      | E -> raise (Failure "Eq not working")

  let test_set_and_get () = 
    a1 := set_v !a1 0 101;
    assert_equal (get_v !a1 0) 101;
    a1 := set_v !a1 1 102;
    assert_equal (get_v !a1 1) 102;
    a1 := set_v !a1 8 108;
    assert_equal (get_v !a1 8) 108

  (* tests for join *)
  let b1 = ref (enc [| 1;-3;  3; 4; 0; 0;-1; 2;-1|])
  let b2 = ref (enc [|-3; 2;103; 4; 5;-1;-1;-1; 2|])
  let b12j =   (enc [| 1; 2; -1; 4;-2;-2;-1;-1;-1|])

  let test_join () =
    b1 := D.join !b1 !b2;
    assert_equal ~cmp:D.equal ~printer:(D.short max_int) !b1 b12j
  

  (* tests for meet *)
  let c1 = ref (enc [| 1;-3;  3; 4; 0; 0;-1; 2;-1|])
  let c2 = ref (enc [|-3; 2;103; 4; 5;-1;-1;-1; 2|])
  let c12m =   (enc [|-3;-3; -3; 4;-3;-3;-1; 2; 2|])

  let test_meet () =
    c1 := D.meet !c1 !c2;
    assert_equal ~cmp:D.equal ~printer:(D.short max_int) !c1 c12m

  (* test big array *)
  let c1 = ref (enc [|42;42;42;42;42;42|])
  let c2 = ref (enc [|42;42;42;42;42;42;42;42;42;42;42;42;42;42;42;42;42;42;42;42|])
  let c3 = ref (enc [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20|])
  let c4 = ref (enc [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20|])


  let test_big () =
    assert_equal ~printer:(string_of_int) (get_v !c1 0) 42;
    assert_equal ~printer:(string_of_int) (get_v !c1 5) 42;
    c2 := set_v !c2 0 100;
    assert_equal ~printer:(string_of_int) (get_v !c2 0) 100;
    c2 := set_v !c2 1 200;
    assert_equal ~printer:(string_of_int) (get_v !c2 1) 200;
    c3 := set_v !c3 1 42;
    c3 := set_v !c3 2 42;
    c4 := set_v !c4 2 42;
    c4 := set_v !c4 1 42;
    assert_equal ~cmp:D.equal ~printer:(D.short max_int) !c3 !c4


  let test_cache_mem () = 
    let v = ref (D.make 30 (Val.top ())) in
      v := set_v !v 1 42;
      v := set_v !v 2 8;
      v := set_v !v 3 8;
      v := set_v !v 4 8;
      assert_equal ~printer:(string_of_int) 42  (get_v !v 1);
      v := set_v !v 5 8;
      v := set_v !v 6 8;
      v := set_v !v 7 8;
      assert_equal ~printer:(string_of_int) 42  (get_v !v 1);
      v := set_v !v 8 8;
      v := set_v !v 9 8;
      v := set_v !v 10 8;
      v := set_v !v 11 8;
      assert_equal ~printer:(string_of_int) 42  (get_v !v 1);
      v := set_v !v 12 8;
      v := set_v !v 13 8;
      v := set_v !v 14 8;
      assert_equal ~printer:(string_of_int) 42  (get_v !v 1);
      assert_equal ~printer:(string_of_int) (-2)(get_v !v 2);
      assert_equal ~printer:(string_of_int)  8  (get_v !v 14);
      assert_equal ~printer:(string_of_int)  8  (get_v !v 13);
      assert_equal ~printer:(string_of_int)  8  (get_v !v 12);
      assert_equal ~printer:(string_of_int)  8  (get_v !v 11)


  let test_unknown_idx () =
    let io = Val.join (get_int_d 0) (get_int_d 1) in
    let v = ref (D.make 10 (Val.bot ())) in
      v := set_v !v   2  2;
      v := set_v !v   1  2;
      v := set_v !v   0  0;
      assert_equal ~printer:(string_of_int)   2  (get_v !v 1);
      v := D.set !v  io  (get_int_d 0);
      assert_equal ~printer:(string_of_int) (-2) (get_v !v 1);
      assert_equal ~printer:(string_of_int) (-2) (get_v !v 2);
      assert_equal ~printer:(string_of_int)   0  (get_v !v 0)  


  let u1 = ref (enc [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20|])
  let u2 = ref (enc [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;42|])
  let u3 = ref (enc [|1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1|])
  let u4 = ref (enc [|1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1|])


  let test_unordered () = 
    assert_bool "basic no order 1" (not (D.leq !u3 !u4));
    assert_bool "basic no order 2" (not (D.leq !u4 !u3));
    assert_bool "basic no order 3" (not (D.leq !u1 !u2));
    assert_bool "basic no order 4" (not (D.leq !u2 !u1))


  let  test_order () = 
    assert_bool "basic order 1" (D.leq (D.bot()) (D.top()));
    assert_bool "basic order 2" (D.leq (D.bot()) ((D.join (D.top()) !u1)));
    assert_bool "basic order 3" (D.leq (D.bot()) ((D.join (D.top()) !u4)));
    assert_bool "basic order 4" (D.leq !u1 (D.join !u1 !u2));
    assert_bool "basic order 5" (D.leq !b1 b12j);
    assert_bool "basic order 6" (D.leq !b2 b12j)


  let test_order_make () = 
    let a = ref (D.make 5 (Val.top ())) in
    let b = ref (D.make 5 (Val.top ())) in
    assert_bool "order after make" (D.leq !a !b);
    assert_bool "order after make" (D.leq !b !a);
    a := set_v !a 3 42;
    b := set_v !b 3 41;
    assert_bool "order after set 1" (not (D.leq !a !b));
    assert_bool "order after set 1" (not (D.leq !b !a));
    b := set_v !b 3 42;
    assert_bool "order after set 2" (D.leq !a !b);
    assert_bool "order after set 2" (D.leq !b !a);
    b := set_v !b 3 (-2);
    assert_bool "element order 1"      (D.leq !a !b);
    assert_bool "element order 2" (not (D.leq !b !a))


  let uj1 = ref (enc [|(-2);(-2);(-2);(-1);(-1);(-1);  0 ;  0 ;0;0;1|])
  let uj2 = ref (enc [|(-2);(-1);  0 ;(-2);(-1);  0 ;(-2);(-1);0;1;0|])
  let uj3 = ref (enc [|(-1);(-1);  0 ;  0 ;0;1|])
  let uj4 = ref (enc [|(-1);  0 ;(-1);  0 ;1;0|])
  
  
  let test_order_jm () =
    assert_bool "bot vs. top"     (D.leq (D.bot()) (D.top()));
    assert_bool "bot vs. top" (not(D.leq (D.top()) (D.bot())));
    let a = ref (D.make 5 (Val.top())) in
    a := set_v !a 1 42;
    assert_bool "top is greatest" (D.leq !a (D.top()));
    assert_bool "bot is smallest" (D.leq (D.bot()) !a);
    assert_bool "join 1" (D.leq (D.bot()) (D.join !uj1 !uj2) );
    assert_bool "join 2" (D.leq (D.join !uj1 !uj2) (D.top()));
    assert_bool "join 3" (D.leq !uj1 (D.join !uj1 !uj2));
    assert_bool "join 4" (D.leq !uj2 (D.join !uj1 !uj2));
    assert_bool "meet 1" (D.leq (D.bot()) (D.meet !uj3 !uj4) );
    assert_bool "meet 2" (D.leq (D.meet !uj3 !uj4) (D.top()));
    assert_bool "meet 3" (D.leq (D.meet !uj3 !uj4) !uj3);
    assert_bool "meet 4" (D.leq (D.meet !uj3 !uj4) !uj4)


  (* all tests together *)
  let  test =  
    [ ("test_equal"       >:: test_equal ); 
      ("test_nequal"      >:: test_nequal);
      ("test_set_and_get" >:: test_set_and_get );
      ("test_join"        >:: test_join );
      ("test_meet"        >:: test_meet);
      ("test_big"         >:: test_big);
(*      ("test_cache_mem"   >:: test_cache_mem);*)
      ("test_unknown_idx" >:: test_unknown_idx);
      ("test_unordered"   >:: test_unordered);
      ("test_order"       >:: test_order);
      ("test_order_make"  >:: test_order_make);
      ("test_order_jm"    >:: test_order_jm);
   ]

end

