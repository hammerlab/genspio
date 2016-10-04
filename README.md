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

To run the tests
also need
[`pvem_lwt_unix`](http://www.hammerlab.org/docs/pvem_lwt_unix/master/index.html):

    export WITH_TESTS=true
    make 
    ./genspio-test.byte
    
The test should output a markdown report potentially mentioning other files
containing details about the failures (Here it is on Ubuntu Xenial):

```markdown
--------------------------------------------------------------------------------


### All Tests

Summary:

* Test "dash" (`'dash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 55 failures
    - time: 0.49 s.
    - version: `"Version: 0.5.8-2.1ubuntu2"`.
* Test "bash" (`'bash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 55 failures
    - time: 0.71 s.
    - version: `"GNU bash, version 4.3.46(1)-release (x86_64-pc-linux-gnu)"`.
* Test "sh" (`'sh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 55 failures
    - time: 0.64 s.
    - version: `""`.
* Test "busybox" (`'busybox' 'ash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 55 failures
    - time: 0.44 s.
    - version: `"BusyBox v1.22.1 (Ubuntu 1:1.22.0-15ubuntu1) multi-call binary."`.
* Test "ksh" (`'ksh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 12 / 55 failures
    - time: 0.49 s.
    - version: `"version         sh (AT&T Research) 93u+ 2012-08-01"`.
    - Cf. `/tmp/genspio-test-ksh-failures.txt`.
* Test "mksh" (`'mksh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 55 failures
    - time: 0.50 s.
    - version: `"Version: 52c-2"`.
* Test "posh" (`'posh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 55 failures
    - time: 0.54 s.
    - version: `"Version: 0.12.6"`.
* Test "zsh" (`'zsh' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 55 failures
    - time: 1.76 s.
    - version: `"zsh 5.1.1 (x86_64-ubuntu-linux-gnu)"`.

All “known” shells were tested ☺


--------------------------------------------------------------------------------
```

Development Notes
-----------------

About dealing with `sh` / POSIX insanity:

- <http://stackoverflow.com/questions/7427262/how-to-read-a-file-into-a-variable-in-shell/22607352#22607352>
- <http://www.etalabs.net/sh_tricks.html> 
- <http://apenwarr.ca/log/?m=201102>
- <http://stackoverflow.com/questions/794902/whats-the-opposite-of-od1>

Opengroup specs:

- [printf command](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/printf.html)
  (and
  [format strings](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap05.html#tag_05))
- [od](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/od.html)
- [trap](http://pubs.opengroup.org/onlinepubs/000095399/utilities/trap.html),
  [kill](http://pubs.opengroup.org/onlinepubs/000095399/utilities/kill.html),
  and
  [`signal.h`](http://pubs.opengroup.org/onlinepubs/000095399/basedefs/signal.h.html)


One should not count on printf hexadecimal literals (`printf '\x42'`):
cf.
[man page](http://www.unix.com/man-page/POSIX/1posix/printf/) and
[issue](https://bugs.launchpad.net/ubuntu/+source/dash/+bug/1499473)
on
[Dash](https://en.wikipedia.org/wiki/Almquist_shell)
→ “Won't Fix.”

Put arbitrary content (incl. `\000`, `\n`, etc.) in a variable as hexadecimal:

```shell
IFS= read hexa_var << EOOOF
$(
cat /tmp/p1_out | {
while true; do
read dummy oct << EOF
$(dd bs=1 count=1 2>/dev/null |od -t x1)
EOF
echo "oct: $oct" >&2
printf "${oct}"
if [ "$oct" = "" ] ; then break ; fi
done
printf '\n'
}
)
EOOOF

echo "hexa: $hexa_var"
```


