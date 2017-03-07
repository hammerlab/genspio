Genspio: Generate Shell Phrases In OCaml
========================================

Genspio is still in *alpha* status. It is a typed embedded DSL to generate shell
scripts and commands from OCaml.

For now the EDSL is based on a big GADT and compiles to POSIX one-liners or
multi-line scripts.

The tests run the output of the compiler against a few shells that it tries to
find on the host (e.g. `dash`, `bash`, `busybox`, `mksh`, `zsh` … cf. the
example test results summary below).

Build
-----

You can install the library though `opam pin`:

    opam pin add genspio https://github.com/hammerlab/genspio.git

You can also build locally:

You need OCaml ≥ 4.03.0 together with
[`nonstd`](http://www.hammerlab.org/docs/nonstd/master/index.html),
[`sosa`](http://www.hammerlab.org/docs/sosa/master/index.html), and
[`solvuu-build`](https://github.com/solvuu/solvuu-build):

    make
    
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
    - 0 / 184 failures
    - time: 16.04 s.
    - version: `"Version: 0.5.8-2.1ubuntu2"`.
* Test "bash" (`'bash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 184 failures
    - time: 28.24 s.
    - version: `"GNU bash, version 4.3.46(1)-release (x86_64-pc-linux-gnu)"`.
* Test "sh" (`'sh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 184 failures
    - time: 15.69 s.
    - version: `""`.
* Test "busybox" (`'busybox' 'ash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 184 failures
    - time: 11.43 s.
    - version: `"BusyBox v1.22.1 (Ubuntu 1:1.22.0-15ubuntu1) multi-call binary."`.
* Test "ksh" (`'ksh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 19 / 184 failures
    - time: 20.85 s.
    - version: `"version         sh (AT&T Research) 93u+ 2012-08-01"`.
    - Cf. `/tmp/genspio-test-ksh-failures.txt`.
* Test "mksh" (`'mksh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 2 / 184 failures
    - time: 29.24 s.
    - version: `"Version: 52c-2"`.
    - Cf. `/tmp/genspio-test-mksh-failures.txt`.
* Test "posh" (`'posh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 2 / 184 failures
    - time: 29.13 s.
    - version: `"Version: 0.12.6"`.
    - Cf. `/tmp/genspio-test-posh-failures.txt`.
* Test "zsh" (`'zsh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 20 / 184 failures
    - time: 22.33 s.
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

```
export add_shells='
Local-sh, escape, <cmd>,
    sh -c <cmd>
++
My-gcloud-freebsd-sh, escape, <command>,
   printf "%s" <command> | gcloud compute ssh fbd01 --command "sh -x"
'
```
