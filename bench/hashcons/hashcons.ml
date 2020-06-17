(* ocamlbuild -pkg batteries hashcons.native && ./hashcons.native *)

let pdump s x = Printf.printf "dump %s = %s\n" s (Batteries.dump x)

module A = struct
  type s = A | B of int
  let x = B 1
  let y = B 1
  let z = B 2
  let c i = B i

  module HC = BatHashcons.MakeTable (struct type t = s let equal x y = compare x y = 0 let hash = Hashtbl.hash end)

  let h = HC.create 13

  let test x =
    let old_count = HC.count h in
    let hx = HC.hashcons h x in
    let new_count = HC.count h in
    Printf.printf "old_count: %d; tag: %d; hcode: %d; new_count %d\n" old_count hx.tag hx.hcode new_count

  let test =
    print_endline "module A: A | B of int";
    pdump "x" x;
    Printf.printf "x = y: %b\n" (x = y); (* true *)
    Printf.printf "x == y: %b\n" (x == y); (* true *)
    Printf.printf "c (2*3-5) == c (2-1): %b\n" (c (2*3-5) == c (2-1)); (* true *)
    test x;
    test y;
    test z;
    print_newline ();
end

module B = struct
  module S = BatSet
  type s = int S.t
  let x = S.add 1 S.empty
  let y = S.add 1 S.empty

  module HC = BatHashcons.MakeTable (struct type t = s let equal x y = compare x y = 0 let hash = Hashtbl.hash end)

  let h = HC.create 13

  let test x =
    let old_count = HC.count h in
    let hx = HC.hashcons h x in
    let new_count = HC.count h in
    Printf.printf "old_count: %d; tag: %d; hcode: %d; new_count %d\n" old_count hx.tag hx.hcode new_count

  let cmp sx sy x y =
    let ops = ["=", x=y; "==", x==y; "hash", Hashtbl.hash x = Hashtbl.hash y; "dump", Batteries.dump x = Batteries.dump y] in
    List.iter (fun (op, b) -> Printf.printf "%s %s %s: %b\n" sx op sy b) ops

  let test =
    print_endline "module B: int BatSet.t";
    pdump "x" x;
    pdump "y" y;
    cmp "x" "y" x y;
    test x; (* Why does this continue with tag = 3 instead of starting at 1 again? It's a different h, but the tag-counter seems to be shared between all of them. *)
    test y;
    print_newline ();
end

module C = struct
  type s = int list
  let x = [0; 1]
  let y = [1; 0]

  module HT = struct
    type t = s
    let equal x y = BatSet.(equal (of_list x) (of_list y))
    let hash x = Hashtbl.hash (BatSet.of_list x) (* this already leads to different hashes for x and y because of the different insert order *)
    let hash x = Hashtbl.hash (List.sort_uniq compare x)
  end
  module HC = BatHashcons.MakeTable (HT)

  let h = HC.create 13

  let test x =
    let old_count = HC.count h in
    let hx = HC.hashcons h x in
    let new_count = HC.count h in
    Printf.printf "old_count: %d; tag: %d; hcode: %d; new_count %d\n" old_count hx.tag hx.hcode new_count

  let cmp sx sy x y =
    let ops = ["=", x=y; "==", x==y; "hash", Hashtbl.hash x = Hashtbl.hash y; "dump", Batteries.dump x = Batteries.dump y; "HT.equal", HT.equal x y; "HT.hash", HT.hash x = HT.hash y] in
    List.iter (fun (op, b) -> Printf.printf "%s %s %s: %b\n" sx op sy b) ops

  let test =
    print_endline "module C: int list but treated as set";
    pdump "x" x;
    pdump "y" y;
    cmp "x" "y" x y;
    test x;
    test y;
    print_newline ();
end
