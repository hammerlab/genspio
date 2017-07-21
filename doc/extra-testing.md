Additional Testing
==================

FreeBSD Box on Google Cloud
---------------------------

We can easily create FreeBSD nodes (i.e. without creating/uploading custom
images, cf. [forum](https://forums.freebsd.org/threads/56664/)):

```bash
gcloud compute instances create testing-fbsd \
       --image freebsd-10-3-release-amd64 \
       --image-project=freebsd-org-cloud-dev
```

Then, a function to get the IP address assigned by gcloud:

```bash
freebsd_ip_address () {
    gcloud compute instances describe testing-fbsd | awk -F ':' ' /natIP:/ { print $2 }'
}
freebsd_test () {
    ssh -i ~/.ssh/google_compute_engine $(freebsd_ip_address) 'uname -a'
}
```

The tests can be run with the `add_shells` variable over SSH, they usually
require a higher timeout than the default:

```bash
export add_shells="
Freebsd-gcloud, escape, <cmd>,
   printf '%s' <cmd> | ssh -i ~/.ssh/google_compute_engine $(freebsd_ip_address) 'sh -x'
"
export only_dash=true # We don't run all the other local tests this time
export single_test_timeout=50
_build/src/test/genspio-test.byte
```


We get the usual report:

* Test "dash" (`'dash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 190 failures
    - time: 12.97 s.
    - version: `"Version: 0.5.8-2.1ubuntu2"`.
* Test "Freebsd-gcloud" (`printf '%s' 'askjdeidjiedjjjdjekjdeijjjidejdejlksi () { <command>  ; } ;  askjdeidjiedjjjdjekjdeijjjidejdejlksi '\''<arg1>'\'' '\''<arg2>'\'' '\''<arg-n>'\''' | ssh -i ~/.ssh/google_compute_engine  42.42.42.42 'sh -x'`):
    - 0 / 190 failures
    - time: 165.19 s.
    - version: `"Command-line"`.


OpenWRT/ARM with Qemu
---------------------

The OpenWRt project provides a nice
wiki [page](https://wiki.openwrt.org/doc/howto/qemu) on running with Qemu.

This function downloads the required data and starts the virtual machine:

```bash
qemu_openwrt () {
    local tmp=/tmp/qemu_openwrt/
    mkdir -p $tmp
    cd $tmp
    if ! [ -f openwrt-realview-vmlinux-initramfs.elf ] ; then
        wget https://downloads.openwrt.org/snapshots/trunk/realview/generic/openwrt-realview-vmlinux-initramfs.elf
    fi
    if ! [ -f openwrt-realview-vmlinux.elf ] ; then
        wget https://downloads.openwrt.org/snapshots/trunk/realview/generic/openwrt-realview-vmlinux.elf
    fi
    if ! [ -f openwrt-realview-sdcard.img ] ; then
        wget https://downloads.openwrt.org/snapshots/trunk/realview/generic/openwrt-realview-sdcard.img
    fi
    qemu-system-arm -M realview-pbx-a9 -m 1024M \
                    -kernel openwrt-realview-vmlinux.elf \
                    -net nic  \
                    -net user,hostfwd=tcp::10022-:22 \
                    -nographic \
                    -sd openwrt-realview-sdcard.img \
                    -append "console=ttyAMA0 verbose debug root=/dev/mmcblk0p1"
}
```

The default installation does not contain the POSIX utility `od`, we can install
it with `opkg` (here over SSH for the sake of the example):

```bash
qemu_openwrt_depedencies () {
    ssh -oStrictHostKeyChecking=no -p 10022 root@localhost 'opkg update ; opkg install coreutils-od'
}
```

OpenWRT is ready to run the tests (yes! `M` for “megabytes” ☺):

```
root@OpenWrt:/# df -h
Filesystem                Size      Used Available Use% Mounted on
/dev/root                46.5M      2.9M     42.7M   6% /
tmpfs                   378.1M    612.0K    377.5M   0% /tmp
tmpfs                   512.0K         0    512.0K   0% /dev
```

The timeout has to be set pretty high to make all the tests succeed in time:

```bash
export add_shells="
OpenWRT-qemu-arm, escape, <cmd>,
   printf '%s' <cmd> | ssh  -oStrictHostKeyChecking=no -p 10022 root@localhost 'sh -x'
"
export only_dash=true # We don't run all the other local tests this time
export single_test_timeout=500
_build/src/test/genspio-test.byte
```

and the *current* results:

* Test "dash" (`'dash' '-x' '-c' '<command>' '--' '<arg1>' '<arg2>' '<arg-n>'`):
    - 0 / 190 failures
    - time: 13.47 s.
    - version: `"Version: 0.5.8-2.1ubuntu2"`.
* Test "OpenWRT-qemu-arm" (`printf '%s' 'askjdeidjiedjjjdjekjdeijjjidejdejlksi () { <command>  ; } ;  askjdeidjiedjjjdjekjdeijjjidejdejlksi '\''<arg1>'\'' '\''<arg2>'\'' '\''<arg-n>'\''' | ssh  -oStrictHostKeyChecking=no -p 10022 root@localhost 'sh -x'`):
    - 0 / 190 failures
    - time: 800.90 s.
    - version: `"Command-line"`.
