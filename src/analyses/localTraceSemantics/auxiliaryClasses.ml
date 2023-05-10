open LocalTraces
open Prelude.Ana
open Graph

(* ID Generator 
   maintains info what node has which ID such that the ID's are consistent among local traces *)
class id_generator = 
  object(self)
    val mutable currentID = 0
    val mutable edges:((node * CustomEdge.t * node) list) = []
    method increment () =
      currentID <- currentID + 1; 
      currentID

    (* Maybe prev_node and edge are enough to determine the ID of a dest_node? 
       This needs to be tried out at some point in the future *)
    method getID (prev_node:node) (edge:CustomEdge.t) (dest_programPoint:MyCFG.node) (dest_sigma:varDomain SigmaMap.t) (dest_tid:int) (dest_ls:VarinfoSet.t) =
      let id = List.fold (fun acc (prev_node_find, edge_find, {programPoint=p_find;sigma=s_find;id=id_find;tid=tid_find;lockSet=ls_find}) -> 
          if (NodeImpl.equal prev_node prev_node_find)&&(CustomEdge.equal edge edge_find)&&(Node.equal dest_programPoint p_find)&&(NodeImpl.equal_sigma dest_sigma s_find)&&(tid_find = dest_tid)&&(VarinfoSet.equal ls_find dest_ls) then id_find else acc) (-1) edges 
      in 
      if id = (-1) then ( 
        edges <- (prev_node, edge, {programPoint=dest_programPoint;sigma=dest_sigma;id=currentID+1;tid=dest_tid;lockSet=dest_ls})::edges; 
        self#increment ()) else (print_string ("id was found: "^(string_of_int id)^"\n"); id)
  end

let idGenerator = new id_generator

(* Random int Generator
   Generates values in the interval of [-seed; seed] *)
class random_int_generator =
  object(self)
    val mutable traceVarList:((int * varinfo * int) list) = []
    val mutable seed:int = 5

    method getRandomValue (hash:int) (var:varinfo) = 
      if List.is_empty traceVarList then Random.init 100;
      print_string ("random_int_generator#getRandomValue was invoked with hash="^(string_of_int hash)^", var="^(CilType.Varinfo.show var)^";
     \nwith current traceVarList="^(List.fold (fun acc (int_fold, vinfo_fold, value_fold) -> acc^"; ("^(string_of_int int_fold)^","^(CilType.Varinfo.show vinfo_fold)^","^(string_of_int value_fold)^")") "" traceVarList)^"\n");
      if List.exists ( fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list) ) traceVarList 
      then (print_string "random value exists already\n";
            let _,_,randomValue =List.find (fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list)) traceVarList
            in randomValue) 
      else
        ( print_string "new random value is generated\n";
          let randomValue = if(Random.int 2) = 0 then (*negative*) (Random.int seed) * (-1)
            else (*positive*) (Random.int seed) 
          in
          traceVarList <- (hash, var, randomValue)::traceVarList;
          randomValue)

    method getRandomValueFullCInt (hash:int) (var:varinfo) =
      if List.is_empty traceVarList then Random.self_init ();
      if List.exists ( fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list) ) traceVarList 
      then (
        let _,_,randomValue =List.find (fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list)) traceVarList
        in randomValue) 
      else
        ( print_string "new random value is generated\n";
          (* I want randomValue to have the range [intMin; intMax]*)
          let randomValue = if (Random.int 2) = 0 then (*negative*) ((Random.int 1073741823) + (Random.int 1073741823)) * -1
            else (*positive*) (Random.int 1073741823) + (Random.int 1073741823)
          in
          traceVarList <- (hash, var, randomValue)::traceVarList;
          randomValue)

  end

let randomIntGenerator = new random_int_generator

module VinfoMap = Map.Make(String)

(* Stores mapping of variable name to varinfo 
   This is needed because several created varinfos with same vname are not equal *)
class custom_vinfo_store =
  object(self)
    val mutable vinfoMap: varinfo VinfoMap.t = VinfoMap.empty

    method getGlobalVarinfo (name:string) =
      if VinfoMap.mem name vinfoMap 
      then (VinfoMap.find name vinfoMap)
      else (
        let newVinfo = makeGlobalVar name (TVoid([]))
        in
        vinfoMap <- VinfoMap.add name newVinfo vinfoMap;
        newVinfo
      )

    method getLocalVarinfo (name:string) (typ:typ) =
      if VinfoMap.mem name vinfoMap 
      then (VinfoMap.find name vinfoMap)
      else (
        let newVinfo = makeVarinfo false name typ
        in
        vinfoMap <- VinfoMap.add name newVinfo vinfoMap;
        newVinfo
      )
  end

let customVinfoStore = new custom_vinfo_store

(* Module for thread ID *)
module ThreadIDLocTrace = struct
  type t = int
  let is_write_only _ = false

  let equal (tid1:t) (tid2:t) = tid1 = tid2

  let hash tid = tid

  let compare tid1 tid2 = tid1 - tid2

  let show tid = string_of_int tid

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)

  let name () = "threadID"

  let tag tid = failwith ("tag")

  let arbitrary tid = failwith ("no arbitrary")

  let relift tid = failwith ("no relift")
end

module TIDSet = Set.Make(ThreadIDLocTrace)

(* TID record 
   Stores all existing thread IDs *)
class tid_record =
  object(self)
    val mutable tidSet : TIDSet.t = TIDSet.empty

    method addTID (tid:int) =
      tidSet <- TIDSet.add tid tidSet

    method existsTID (tid:int) =
      TIDSet.mem tid tidSet
  end

let tidRecord = new tid_record
