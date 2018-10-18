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

You can just:

    $genspio_test --important-shells sh /tmp/gtests/
    cd /tmp/gtests/
    tar czf sh-tests.tgz sh/*

`sh-tests.tgz` contains everything needed to run the tests (only requires
`make`).


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

The default installation does not contain the POSIX utility `od` nor `make`
(required for the tests), we can install it with `opkg` (here over SSH for the
sake of the example):

```bash
qemu_openwrt_depedencies () {
    ssh -oStrictHostKeyChecking=no -p 10022 root@localhost 'opkg update ; opkg install make coreutils-od'
}
```

You may just run the tests as above.

Note that this setup is also handled by the `vm_tests` example.

Using the VM-Tester Example
---------------------------

The file `src/examples/vm_tester.ml` is an example of use of Genspio which
provides a command line tool to generate “Qemu” environments.

You can build it with:

    export genspio_vm_tester=_build/default/src/examples/vm_tester.exe
    jbuilder build $genspio_vm_tester

A version is also available in the genspio-doc docker-images (the build of the
docker images sometimes lags behind Genspio's master branch):

    docker pull smondet/genspio-doc-dockerfiles:apps406
    docker run -it smondet/genspio-doc-dockerfiles:apps406 genspio-vm-tester --help

See the list of available virtual machines from:

    $genspio_vm_tester --help

As an example let's run some of the test suite in a Darwin VM. We need to first
generate the test-suite:

    $genspio_test --important-shells sh /tmp/Genspio-tests/ 
    
then we build the VM “environment” in `/tmp/vme-amd64-dw/`:

    $genspio_vm_tester --vm amd64-dw /tmp/vme-amd64-dw/ --ssh-port 20101 \
        --copy /tmp/Genspio-tests/sh-SlowFlow-cp/:genspio-sh-slcp \
        --copy /tmp/Genspio-tests/sh-StdML/:genspio-sh-stdml

A few files have been generated in `/tmp/vme-amd64-dw/`:

     Makefile
     _scripts/
     _tmp_Genspio-tests_sh-SlowFlow-cp_/
     _tmp_Genspio-tests_sh-StdML_/

This directory is mostly self-contained, independent from Genspio/OCaml/Opam, it
can be copied to any Unix host (requires `qemu` and `sshpass`).

See `make help` within the directory for a list of useful targets.

Start with `make configure` to check that the system has the right executables.
The output should look like:

```markdown
Configuration Report
====================

* `qemu-system-x86_64`: found.
* `sshpass`: found.

*Success!*
```

The, in one terminal, start the VM: `make start` (depending on the Operating
system the output will be more or less verbose/informative; Darwin being the
worst …).

Once the VM has booted you can test connecting to it:

    $(make ssh) uname -a

    Darwin charles.local 8.0.1 Darwin Kernel Version 8.0.1: Fri Apr 29 12:18:40 PDT 2005; root:xnu-792.obj/RELEASE_I386 x86 i386


→ the command `make ssh` **outputs** a valid `sshpass`/`ssh` command, we
actually excute the command with a `$( ... )` construct.

The next step is `make setup`, it runs the additional setup instructions as well
as the directory copies specified while generating the environment (`--copy …`).

Then to run the tests, just jump on the VM:

     `make ssh`
     
and run them (see the first `--copy` argument ⮥):

     cd genspio-sh-slcp
     make                # takes a long time...
     make report
     ...

You can always kill a VM with `make kill`


