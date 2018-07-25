Genspio: Generate Shell Phrases In OCaml
========================================

Genspio is a typed EDSL to generate shell scripts and commands from OCaml.

The idea is to build values of type `'a EDSL.t` with the
combinators in the `Genspio.EDSL` module, and compile them to POSIX
shell scripts (or one-liners) with functions from `Genspio.Compile`.
See the file 
[`src/examples/small.ml`](https://github.com/hammerlab/genspio/blob/master/src/examples/small.ml)
which generates a useful list of usage examples, or the
section [“Getting Started”](#getting-started) below.

The tests run the output of the compiler against a few shells that it tries to
find on the host (e.g. `dash`, `bash`, `busybox`, `mksh`, `zsh` … cf. the
example test results summary below).

If you have any questions, do not hesitate to submit an
[issue](https://github.com/hammerlab/genspio/issues).

Genspio's documentation root is at <https://smondet.gitlab.io/genspio-doc/>.

Build
-----

You can install the library though `opam`:

    opam install genspio

Or get the development version with `opam pin`:

    opam pin add genspio https://github.com/hammerlab/genspio.git

You can also build locally:

You need OCaml ≥ 4.03.0 together with
[`nonstd`](http://www.hammerlab.org/docs/nonstd/master/index.html),
[`sosa`](http://www.hammerlab.org/docs/sosa/master/index.html), and
[`jbuilder`](https://github.com/janestreet/jbuilder):

    ocaml please.mlt configure
    jbuilder build @install
    
Getting Started
---------------

Here is a quick example:

```ocaml
utop> open Genspio.EDSL;;

utop> 
let c =
  let username_one_way : c_string t =
    (* We lift the string "USER" to EDSL-land and use function `getenv`: *)
    getenv (string "USER") in
  let username_the_other_way : c_string t =
    (* The usual pipe operator is `||>` *)
    (exec ["whoami"] ||> exec ["tr"; "-d"; "\\n"])
    (* `get_stdout` takes `stdout` from a `unit t` as a `byte_array t` *)
    |> get_stdout
    (* `to_c_string` checks that a `byte_array t` can be casted to a `c_string` *)
    |> to_c_string 
  in
  let my_printf : string -> c_string t list -> unit t = fun fmt args ->
    (* The function `call` is like `exec` but operates on `c_string t` values
       instead of just OCaml strings: *)
    call (string "printf" :: string fmt :: args) in
  (* The operator `=$=` is `string t` equality, it returns a `bool t` that
     we can use with `if_seq`: *)
  if_seq (username_one_way =$= username_the_other_way)
     ~t:[
        my_printf "Username matches: `%s`\\n" [username_one_way];
     ]
     ~e:[
        my_printf "Usernames do not match: `%s` Vs `%s`\\n"
          [username_one_way; username_the_other_way];
     ]
;;
val c : unit t

utop> Sys.command (Genspio.Compile.to_one_liner c);;
Username matches: `smondet`
- : int = 0
```

**More examples:**

- There are many examples in
  [`src/examples/small.ml`](https://github.com/hammerlab/genspio/blob/master/src/examples/small.ml)
  which are used to generate the usage examples documentation webpage.
- The file 
  [`src/examples/downloader.ml`](https://github.com/hammerlab/genspio/blob/master/src/examples/downloader.ml)
  contains a (much) bigger example.
- The file 
  [`src/examples/vm_tester.ml`](https://github.com/hammerlab/genspio/blob/master/src/examples/vm_tester.ml)
  is a *“Makefile + scripts”* generator to setup Qemu virtual machines, they can
  be for instance used to run the tests on more exotic platforms.
- The project 
  [`hammerlab/secotrec`](https://github.com/hammerlab/secotrec) is a real-world,
  larger-scale use of Genspio (for now using version 0.0.0).


Testing
-------

To run the tests you also need `make` and there is an additional dependency on
the `uri` library, see:

    genspio_test=_build/default/src/test/main.exe
    jbuilder build $genspio_test
    $genspio_test --help
    

Try this:

    $genspio_test --important-shells bash,dash /tmp/gtests/
    cd /tmp/gtests/
    make run-all # Attempts to run all the tests on all the shells
    make check   # Checks that all the tests for the important ones succeeded

You can generate a markdown report with `make report` and check `report.md`.

Some failures are expected with not-really-POSIX or buggy shells like
[KSH93](https://en.wikipedia.org/wiki/Korn_shell), or on some corner cases
cf. [`#35`](https://github.com/hammerlab/genspio/issues/35).

You can check failures in the `<shell-test>/failures.md` files, see for instance
`ksh-StdML/failures.md` for the failures of the “KSH with standard Genspio
compilation to multi-line scripts” (similarly there are
`<shell-test>/successes.md` files).

Additional Documentation
------------------------

From here, one can explore:

- Some implementation [notes](./doc/exec-return-issue.md).
- More [information](./doc/extra-testing.md) on testing, e.g. on more exotic
  operating systems.


License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).
