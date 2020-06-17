open BatteriesExceptionless

let test bson n =
  let open Printf in
  printf "\nCreate collection\n";
  let db = Mongo.create_local_default "goblint" "test" in
  printf "Inserting %d times %s\n" n (Bson.to_simple_json bson);
  let t = Unix.gettimeofday () in
  (*for i = 1 to n do
    Mongo.insert db [bson];
    done;*)
  Mongo.insert db (List.make n bson);
  let t1 = Unix.gettimeofday () -. t in
  printf "Done in %f s\n" t1;
  ignore @@ Mongo.drop_collection db;
  printf "Dropped collection\n"

let () =
  let open Bson in
  (* baseline:
      17.27s for 10M times Mongo.insert db [Bson.empty]
      0.172s for Mongo.insert db (List.make n Bson.empty)
  *)
  test empty (1000*1000)