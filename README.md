Genspio: Generate Shell Phrases In OCaml
========================================

Genspio is a typed EDSL to generate shell scripts and commands from OCaml.

The idea is to build values of type `Genspio.EDSL.t` with the
combinators in the `Genspio.EDSL` module, and compile them to POSIX
shell scripts (or one-liners) with functions from `Genspio.Compile`.

Genspio is still in *alpha* status. For now the EDSL is based on a big GADT and
compiles to POSIX one-liners or multi-line scripts.

The tests run the output of the compiler against a few shells that it tries to
find on the host (e.g. `dash`, `bash`, `busybox`, `mksh`, `zsh` … cf. the
example test results summary below).

If you have any questions, you may submit an
[issue](https://github.com/hammerlab/genspio/issues), or join
the authors on the public “Slack” channel of the Hammer Lab:
[![Slack Status](http://publicslack.hammerlab.org/badge.svg)](http://publicslack.hammerlab.org)

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
[`solvuu-build`](https://github.com/solvuu/solvuu-build):

    make
    
Getting Started
---------------

The idea is to build values of type `Genspio.EDSL.t` (through the combinators in
the [`Genspio.EDSL`](src/lib/EDSL.mli) module), and compile them with
functions from [`Genspio.Compile`](src/lib/compile.mli).

Here is a quick example:

```ocaml
utop> open Genspio.EDSL;;

utop> 
let c =
  let username_one_way =
    (* We lift the string "USER" to EDSL-land and use function `getenv`: *)
    getenv (string "USER") in
  let username_the_other_way =
    (* The usual pipe operator is `||>`,
       `output_as_string` takes `stdout` from a `unit t` as a `string t`. *)
    (exec ["whoami"] ||> exec ["tr"; "-d"; "\\n"]) |> output_as_string in
  let my_printf : string -> string t list -> unit t = fun fmt args ->
    (* The function `call` is like `exec` but operates on `string t` values
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


See `src/test/examples.ml` for a (much) bigger example, and 
[`hammerlab/secotrec`](https://github.com/hammerlab/secotrec) for real-world
use.


Testing
-------

To run the tests
also need
[`pvem_lwt_unix`](http://www.hammerlab.org/docs/pvem_lwt_unix/master/index.html)
and
[`ppx_deriving`](https://github.com/whitequark/ppx_deriving):

    export WITH_TESTS=true
    make
    _build/src/test/genspio-test.byte

The test should output a markdown report potentially mentioning other files
containing details about the failures (Here it is on Ubuntu Xenial, some
failures are expected with not-really-POSIX or buggy shells like
[KSH93](https://en.wikipedia.org/wiki/Korn_shell), or on some corner cases
cf. [`#35`](https://github.com/hammerlab/genspio/issues/35)):

```markdown
--------------------------------------------------------------------------------


### All Tests

Summary:

* Test "dash" (`'dash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 190 failures
    - time: 13.31 s.
    - version: `"Version: 0.5.8-2.1ubuntu2"`.
* Test "bash" (`'bash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 190 failures
    - time: 23.37 s.
    - version: `"GNU bash, version 4.3.46(1)-release (x86_64-pc-linux-gnu)"`.
* Test "sh" (`'sh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 190 failures
    - time: 13.59 s.
    - version: `""`.
* Test "busybox" (`'busybox' 'ash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 190 failures
    - time: 8.80 s.
    - version: `"BusyBox v1.22.1 (Ubuntu 1:1.22.0-15ubuntu1) multi-call binary."`.
* Test "ksh" (`'ksh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 20 / 190 failures
    - time: 14.78 s.
    - version: `"version         sh (AT&T Research) 93u+ 2012-08-01"`.
    - Cf. `/tmp/genspio-test-ksh-failures.txt`.
* Test "mksh" (`'mksh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 2 / 190 failures
    - time: 25.56 s.
    - version: `"Version: 52c-2"`.
    - Cf. `/tmp/genspio-test-mksh-failures.txt`.
* Test "posh" (`'posh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 2 / 190 failures
    - time: 24.40 s.
    - version: `"Version: 0.12.6"`.
    - Cf. `/tmp/genspio-test-posh-failures.txt`.
* Test "zsh" (`'zsh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 20 / 190 failures
    - time: 17.94 s.
    - version: `"zsh 5.1.1 (x86_64-ubuntu-linux-gnu)"`.
    - Cf. `/tmp/genspio-test-zsh-failures.txt`.

All “known” shells were tested ☺


--------------------------------------------------------------------------------
```

Tests can be tweaked with environment variables:

- `filter_tests`: is a comma-separated list of name *prefixes* to run only a
  subset of the tests (useful when dealing a specific issue).<br/>
  Example: `export filter_tests=redirect,with_failwith` runs 10 tests instead of
  more than 100.
- `important_shells`: is comma-separated list of shells for which 1 failure makes
  the whole test fail (i.e. if a shell like `ksh` is not “important,” the
  failures are reported but the test command still returns 0).<br/>
  The default is `bash,dash` (The Travis CI script also considers `busybox`
  important for GNU/Linux builds).
- `add_shells`: is a `++`-separated list of “shells,” each one defined as a
  comma-separated list: `<Name>,escape, <cmd-arg>, <cmd>`, where is
  `<cmd-arg>` is replaced with the actual command tested within `<cmd>`, e.g.:
- `only_dash`: run the tests only with `dash` (useful to speedup
  modify-compile-test loops while developing).
- `single_test_timeout`: the timeout for a signle test run
  (default: 5. seconds).

Here is an example of configuration with 2 additional testing shells, one of
them happening over SSH (the target host does not need OCaml).

```
export single_test_timeout=10
export add_shells='
Local-sh, escape, <cmd>,
    sh -c <cmd>
++
My-freebsd-box, escape, <command>,
   printf "%s" <command> | ssh fbd01 "sh -x"
'
```

Additional Documentation
------------------------

From here, one can explore:

- Some implementation [notes](./doc/exec-return-issue.md).
- More [information](./doc/extra-testing.md) on testing, e.g. on more exotic
  operating systems.


License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).
