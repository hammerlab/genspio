Genspio: Generate Shell Phrases In OCaml
========================================


## Notes

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


