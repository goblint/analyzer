open Ocamlbuild_plugin;; (* open the main API module *)
open Unix;;
open Pathname;;

let pwd = to_string pwd

let cildir =
  input_line (open_process_in "ocamlfind query cil") ;;

dispatch begin function
  | Before_options -> ()
  | After_options ->
      flag ["doc"]
        (S [A "-stars"; A"-short-functors";
            A "-hide-warnings"]);

      flag ["doc"; "docdir"]
        (S [A"-t"; A "Goblint API";
            A"-intro";A (pwd^"/src/main.camldoc");
            A "-I"; A (cildir);
            A (cildir^"/cil.mli");
            A (cildir^"/pretty.mli")]);
  | _ -> ()
end
