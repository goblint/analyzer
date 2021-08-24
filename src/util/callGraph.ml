open Analyses

module Make (S: Spec): Spec =
struct
  include S

  module CGNode = Printable.Prod (CilType.Fundec) (S.C)
  module CGNS = Set.Make (CGNode)
  module CGNH = BatHashtbl.Make (CGNode)

  let callers: CGNS.t CGNH.t = CGNH.create 113

  let init () =
    S.init ();
    CGNH.clear callers

  let enter ctx lv f args =
    let r = S.enter ctx lv f args in
    begin
      try
        let caller = (Node.find_fundec ctx.prev_node, ctx.context ()) in
        List.iter (fun (_, fd) ->
            let callee = (f, S.context fd) in
            CGNH.modify_def CGNS.empty callee (CGNS.add caller) callers
            (* CGNH.add callers callee caller *)
          ) r
      with Ctx_failure _ ->
        ()
    end;
    r

  let write_dot () =
    let out = open_out "callgraph.dot" in
    Printf.fprintf out "digraph callgraph {\n";

    let written_nodes = CGNH.create 113 in
    let write_node cgnode =
      if not (CGNH.mem written_nodes cgnode) then (
        CGNH.replace written_nodes cgnode ();
        Printf.fprintf out "\t%d [label=\"%s(%d)\"];\n" (CGNode.hash cgnode) ((fst cgnode).svar.vname) (S.C.hash (snd cgnode))
      )
    in

    CGNH.iter (fun callee callers' ->
        write_node callee;
        CGNS.iter write_node callers'
      ) callers;

    CGNH.iter (fun callee callers' ->
        CGNS.iter (fun caller ->
            Printf.fprintf out "\t%d -> %d;\n" (CGNode.hash caller) (CGNode.hash callee)
          ) callers'
      ) callers;

    Printf.fprintf out "}\n";
    close_out_noerr out

  let finalize () =
    S.finalize ();
    write_dot ()
end