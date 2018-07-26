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
