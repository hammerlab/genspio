Implementation Notes
====================


The `exec` Schism
-----------------

Specification
of 
[`exec`](http://pubs.opengroup.org/onlinepubs/009695399/utilities/exec.html).

> If command is specified, exec shall not return to the shell; rather, the exit
> status of the process shall be the exit status of the program implementing
> command, which overlaid the shell. If command is not found, the exit status
> shall be 127. If command is found, but it is not an executable utility, the
> exit status shall be 126. **If a redirection error occurs (see
> [Consequences of Shell Errors](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_08_01)),
> the shell shall exit with a value in the range > 1-125**.
> Otherwise, exec shall return a zero exit status.


For a given `shell`, trying:

    $shell -c ' exec 4>&3 ; echo "Exec-returns: $?"' ; echo "Shell-returns: $?"

The POSIX ones:

* `shell=dash`, `shell=sh`, `shell='busbox ash'`: `Shell-returns: 2`
* `shell=ksh`, `shell=mksh`: `Shell-returns: 1`

The non-POSIX ones:

* `shell=bash`, `shell=zsh`:
  `Exec-returns: 1 Shell-returns: 0`
