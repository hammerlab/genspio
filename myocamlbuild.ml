open Nonstd
open Solvuu_build.Std

let project_name = "genspio"
let version = "0.0.1-dev"

let build_tests =
  try Sys.getenv "WITH_TESTS" = "true" with _ -> false

let lib_deps = [
  "nonstd";
  "sosa";
]

let test_deps = [
]

let lib : Project.item =
  Project.lib project_name
    ~thread:()
    ~safe_string:()
    ~findlib_deps:lib_deps
    ~dir:"src/lib"
    ~style:(`Pack project_name)


let test : Project.item list =
  if build_tests
  then
    let test_lib =
      let name = project_name ^ "_test_lib" in
      Project.lib name
        ~thread:()
        ~findlib_deps:(lib_deps @ test_deps)
        ~safe_string:()
        ~dir:"src/test-lib/"
        ~style:(`Pack "tests")
        ~install:`No
        ~internal_deps:[lib]
    in
    [
      test_lib;
      Project.app (project_name ^ "-test")
        ~thread:()
        ~safe_string:()
        ~file:"src/test/main.ml"
        ~install:`No
        ~internal_deps:[lib; test_lib];
      Project.app (project_name ^ "-examples")
        ~thread:()
        ~safe_string:()
        ~file:"src/test/examples.ml"
        ~install:`No
        ~internal_deps:[lib; test_lib];
    ]
  else
    []

let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize_ascii project_name);
]

let build_doc () =
  let open Ocamlbuild_plugin in
  let lib = (match lib with Project.Lib l -> l | _ -> failwith "NOpe") in
  let mlis = lib.Project.mli_files in
  let mls = lib.Project.ml_files in
  let oredoc_call =
    "INPUT= \
     INDEX=README.md \
     TITLE_PREFIX=\"Genspio: \" \
     API=apidoc/ \
     OUTPUT_DIR=doc/ \
     TITLE_SUBSTITUTIONS= \
     oredoc " in
  let ocamldoc files =
    sprintf
      "ocamlfind ocamldoc -html -d apidoc/ -package '%s'  -thread \
       -charset UTF-8 -t \"Genspio API\" -keep-code -colorize-code \
       -I src/lib/  -I src/pure \
       %s " (String.concat "," lib_deps) (String.concat " " files) in
  let deps =
    ["README.md"]
    @ [Project.path_of_pack ~suffix:".cma" lib]
  in
  Solvuu_build.Util.Rule.rule
    ~name:"Build-genspio-docs"
    ~prods:["doc/index.html"]
    ~deps
    ~insert:`bottom
    begin fun env builder ->
      let files =
        Tools.run_ocamlfind_ocamldep_sort
          (Nonstd.List.map (mls @ mlis) ~f:(sprintf "%s/%s" lib.Project.dir)) in
      Seq [
        Cmd (Sh "mkdir -p doc apidoc");
        Cmd (Sh (ocamldoc files));
        Cmd (Sh oredoc_call);
      ]
    end



let () =
  Project.basic1 ~project_name ~version ~ocamlinit_postfix
    ~additional_rules:[build_doc]
    (lib :: test)
