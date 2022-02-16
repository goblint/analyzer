open OUnit2

module GroupableDriver : MapDomain.Groupable with type t = string  =
struct
  include Printable.Strings
end

module LatticeDriver = Lattice.Fake (GroupableDriver)


module TestMap (M:MapDomain.S with type key = string and type value = string) =
struct
  let is_empty_top = ref true

  let get_empty () =
    try
      (is_empty_top := true;  M.top ())
    with Lattice.Unsupported _ ->
      (is_empty_top := false; M.bot ())

  let is_empty x =
    match (!is_empty_top) with
      | true  -> M.is_top x
      | false -> M.is_bot x


  let test_add_remove_find _ =
    let map = ref (get_empty ()) in
      begin
	assert_bool "can't get empty map" (is_empty !map);

	map := M.add "key1" "value11" !map;
	assert_equal "value11" (M.find "key1" !map);

	map := M.add "key1" "value12" !map;
	assert_equal "value12" (M.find "key1" !map);

	map := M.add "key2" "value21" !map;
	assert_equal "value21" (M.find "key2" !map);

	map := M.remove "key1" !map;
	begin
	  try ignore (M.find "key1" !map); assert_failure "problem removeing key1"
	  with Lattice.Unsupported _ -> ()
	end ;

      end

  let test_iter _ =
    let map = ref (get_empty ()) in
    let values = ["1","1";"2","2";"3","3";"4","4"]  in
    let fun1 k v =
      assert_equal k v;
      map := M.remove k !map
    in
      begin
	map := M.add_list values !map;
	M.iter fun1 !map;
	assert_bool "iter does not work" (is_empty !map)
      end

  let test_fold _ =
    let map = ref (get_empty ()) in
    let values = ["1","2";"2","3";"3","4";"4","5"]  in
    let result = "45342312" in
    let fun1 k v a = k^v^a in
      begin
	map := M.add_list values !map;
	assert_equal result (M.fold fun1 !map "")
      end

  let test_add_list _ =
    let map = ref (get_empty ()) in
    let values = ["1","2";"2","3";"3","4";"4","5"]  in
      map := M.add_list values !map;
      assert_equal "2" (M.find "1" !map);
      assert_equal "3" (M.find "2" !map);
      assert_equal "4" (M.find "3" !map);
      assert_equal "5" (M.find "4" !map)

  let test_map _ =
    let map = ref (get_empty ()) in
    let values = ["1","2";"2","3";"3","4";"4","5"]  in
    let fun1 n = n^"1" in
      map := M.add_list values !map;
      map := M.map fun1 !map;
      assert_equal "21" (M.find "1" !map);
      assert_equal "31" (M.find "2" !map);
      assert_equal "41" (M.find "3" !map);
      assert_equal "51" (M.find "4" !map)

  let test_add_list_set _ =
    let map = ref (get_empty ()) in
    let keys = ["1";"2";"3"] in
      map := M.add_list_set keys "v" !map;
      assert_equal "v" (M.find "1" !map);
      assert_equal "v" (M.find "2" !map);
      assert_equal "v" (M.find "3" !map)

  let test_add_list_fun _ =
    let map = ref (get_empty ()) in
    let fun1 k = k^"1" in
    let keys = ["1";"2";"3";"4"] in
      map := M.add_list_fun keys fun1 !map;
      assert_equal "11" (M.find "1" !map);
      assert_equal "21" (M.find "2" !map);
      assert_equal "31" (M.find "3" !map);
      assert_equal "41" (M.find "4" !map)


  let test_for_all _ =
    let map = ref (get_empty ()) in
    let values = ["1","1";"2","2";"3","3";"4","4"] in
    let fun1 k v = k = v in
      map := M.add_list values !map;
      assert_bool "for_all broken" (M.for_all fun1 !map)


  let test_map2 _ =
    let map1 = ref (get_empty ()) in
    let map2 = ref (get_empty ()) in
    let values1 = ["1","a";"2","b";"3","c";"4","d"] in
    let values2 = ["1","a";"2","b";"3","c";"5","d"] in
    let fun1 v1 v2 = assert_equal v1 v2; "1" in
    let fun2 v1 v2 a = v2^a in
      map1 := M.add_list values1 !map1;
      map2 := M.add_list values2 !map2;
      ignore (M.map2 fun1 !map1 !map2);
      assert_equal "111" (M.fold fun2 (M.map2 fun1 !map2 !map1) "")


  let test_long_map2 _ =
    let map1 = ref (get_empty ()) in
    let map2 = ref (get_empty ()) in
    let values1 = ["1","a";"2","b";"3","c";"4","d"] in
    let values2 = ["1","a";"2","b";"3","c";"4","d"] in
    let fun1 v1 v2 = assert_equal v1 v2; "1" in
    let fun2 v1 v2 a = v2^a in
      map1 := M.add_list values1 !map1;
      map2 := M.add_list values2 !map2;
      ignore (M.long_map2 fun1 !map1 !map2);
      assert_equal "1111" (M.fold fun2 (M.long_map2 fun1 !map2 !map1) "")


  let test () =
    [
      "test_add_remove_find" >:: test_add_remove_find;
      "test_add_list"        >:: test_add_list;
      "test_iter"            >:: test_iter;
      "test_fold"            >:: test_fold;
      "test_map"             >:: test_map;
      "test_add_list_set"    >:: test_add_list_set;
      "test_add_list_fun"    >:: test_add_list_fun;
      "test_for_all"         >:: test_for_all;
      "test_map2"            >:: test_map2;
      "test_long_map2"       >:: test_long_map2;
    ]

end


module Mbot = MapDomain.MapBot (GroupableDriver) (LatticeDriver)
module Mtop = MapDomain.MapTop (GroupableDriver) (LatticeDriver)

module Tbot = TestMap (Mbot)
module Ttop = TestMap (Mtop)


let test_Mbot_join_meet _ =
  let assert_eq =
    let printer a = Pretty.sprint ~width:80 (Mbot.pretty () a) in
    let cmp = Mbot.equal in
      assert_equal ~cmp:(cmp) ~printer:(printer)
  in
  let bot    = Mbot.bot () in (* bot is empty *)
  let mone   = Mbot.add "1" "1" bot in
  let mtwo   = Mbot.add "2" "2" bot in
  let m12    = Mbot.add_list ["1","1";"2","2"] bot in
  let m21    = Mbot.add_list ["2","2";"1","1"] bot in
    assert_eq bot    bot;
    assert_eq mone   mone;
    assert_eq mtwo   mtwo;
    assert_eq m12    m21;
    assert_eq bot   (Mbot.join bot   bot );
    assert_eq mone  (Mbot.join mone  bot );
    assert_eq mone  (Mbot.join bot   mone);
    assert_eq mone  (Mbot.join mone  mone);
    assert_eq mtwo  (Mbot.join mtwo  mtwo);
    assert_eq m21   (Mbot.join mone  mtwo);
    assert_eq m21   (Mbot.join mtwo  mone);
    assert_eq m21   (Mbot.join m12   mtwo);
    assert_eq m21   (Mbot.join mone  m21 );
    assert_eq m21   (Mbot.join m12   m21 );
    assert_eq bot   (Mbot.meet bot   bot );
    assert_eq mone  (Mbot.meet mone  mone);
    assert_eq m21   (Mbot.meet m21   m21 );
    assert_eq bot   (Mbot.meet bot   mone);
    assert_eq bot   (Mbot.meet mone  bot );
    assert_eq bot   (Mbot.meet bot   m21 );
    assert_eq bot   (Mbot.meet m21   bot );
    assert_eq mone  (Mbot.meet mone  m21 );
    assert_eq mtwo  (Mbot.meet m21   mtwo);
    ()

let test_Mtop_join_meet _ =
  let assert_eq =
    let printer a = Pretty.sprint ~width:80 (Mtop.pretty () a) in
    let cmp = Mtop.equal in
      assert_equal ~cmp:(cmp) ~printer:(printer)
  in
  let top    = Mtop.top () in (* bot is empty *)
  let mone   = Mtop.add "1" "1" top in
  let mtwo   = Mtop.add "2" "2" top in
  let m12    = Mtop.add_list ["1","1";"2","2"] top in
  let m21    = Mtop.add_list ["2","2";"1","1"] top in
    assert_eq top    top;
    assert_eq mone   mone;
    assert_eq mtwo   mtwo;
    assert_eq m12    m21;
    assert_eq top   (Mtop.join top   top );
    assert_eq top   (Mtop.join mone  top );
    assert_eq top   (Mtop.join top   mone);
    assert_eq mone  (Mtop.join mone  mone);
    assert_eq mtwo  (Mtop.join mtwo  mtwo);
    assert_eq top   (Mtop.join mone  mtwo);
    assert_eq top   (Mtop.join mtwo  mone);
    assert_eq mtwo  (Mtop.join m12   mtwo);
    assert_eq mone  (Mtop.join mone  m21 );
    assert_eq m21   (Mtop.join m12   m21 );
    assert_eq top   (Mtop.meet top   top );
    assert_eq mone  (Mtop.meet mone  mone);
    assert_eq m21   (Mtop.meet m21   m21 );
    assert_eq mone  (Mtop.meet top   mone);
    assert_eq mone  (Mtop.meet mone  top );
    assert_eq m21   (Mtop.meet top   m21 );
    assert_eq m21   (Mtop.meet m21   top );
    assert_eq m21   (Mtop.meet mone  m21 );
    assert_eq m21   (Mtop.meet m21   mtwo);
    ()

let test () = "mapDomainTest" >:::
  [ "MapBot"         >::: Tbot.test ();
    "MapTop"         >::: Ttop.test ();
    "test_Mbot_join" >::  test_Mbot_join_meet ;
    "test_Mtop_join" >::  test_Mtop_join_meet ;
  ]
