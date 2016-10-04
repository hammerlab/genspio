open Nonstd
open Solvuu_build.Std

let project_name = "genspio"
let version = "master"

let build_tests =
  try Sys.getenv "WITH_TESTS" = "true" with _ -> false

let lib_deps = [
  "nonstd";
  "sosa";
]

let test_deps = [
  "pvem_lwt_unix";
  "ppx_deriving.std";
]

let lib : Project.item =
  Project.lib project_name
    ~thread:()
    ~findlib_deps:lib_deps
    ~dir:"src/lib"
    ~style:(`Pack project_name)
    ~pkg:project_name


let test : Project.item list =
  if build_tests
  then
    let test_lib =
      let name = project_name ^ "_test_lib" in
      Project.lib name
        ~thread:()
        ~findlib_deps:(lib_deps @ test_deps)
        ~dir:"src/test-lib/"
        ~style:(`Pack "tests")
        ~pkg:(project_name ^ ".test-lib")
        ~internal_deps:[lib]
    in
    [
      test_lib;
      Project.app (project_name ^ "-test")
        ~thread:()
        ~file:"src/test/main.ml"
        ~internal_deps:[lib; test_lib]
    ]
  else
    []

let ocamlinit_postfix = [
  sprintf "open %s" (String.capitalize_ascii project_name);
]

let () =
  Project.basic1 ~project_name ~version ~ocamlinit_postfix
    (lib :: test)
