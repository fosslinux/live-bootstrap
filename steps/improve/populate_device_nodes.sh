#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# http://www.linuxfromscratch.org/lfs/view/6.1/chapter06/devices.html
mkdir -p "/dev"
test -c "/dev/null" || (rm -f "/dev/null" &&
                            mknod -m 666 "/dev/null" c 1 3)
test -c "/dev/zero" || mknod -m 666 "/dev/zero" c 1 5
test -c "/dev/random" || mknod -m 444 "/dev/random" c 1 8
test -c "/dev/urandom" || mknod -m 444 "/dev/urandom" c 1 9

test -c "/dev/ptmx" || mknod -m 666 "/dev/ptmx" c 5 2
test -c "/dev/tty" || mknod -m 666 "/dev/tty" c 5 0

test -e "/dev/stdout" || ln -s "/proc/self/fd/1" "/dev/stdout"

if mount --version >/dev/null 2>&1; then
    test -d "/dev/shm" || (mkdir /dev/shm && mount -t tmpfs tmpfs /dev/shm)
    test -d "/proc" || (mkdir /proc && mount -t proc proc /proc)
fi

if [ "${CHROOT}" = False ]; then
    test -c "/dev/tty1" || mknod -m 666 "/dev/tty1" c 4 1
    test -c "/dev/tty2" || mknod -m 666 "/dev/tty2" c 4 2
    test -c "/dev/console" || mknod -m 666 "/dev/console" c 5 1
    test -b "/dev/sda" || mknod -m 600 "/dev/sda" b 8 0
    test -b "/dev/sda1" || mknod -m 600 "/dev/sda1" b 8 1
    test -b "/dev/sda2" || mknod -m 600 "/dev/sda2" b 8 2
    test -b "/dev/sda3" || mknod -m 600 "/dev/sda3" b 8 3
    test -b "/dev/sdb" || mknod -m 600 "/dev/sdb" b 8 16
    test -b "/dev/sdb1" || mknod -m 600 "/dev/sdb1" b 8 17
    test -b "/dev/sdb2" || mknod -m 600 "/dev/sdb2" b 8 18
    test -b "/dev/sdb2" || mknod -m 600 "/dev/sdb3" b 8 19
    test -b "/dev/sdc" || mknod -m 600 "/dev/sdc" b 8 32
    test -b "/dev/sdc1" || mknod -m 600 "/dev/sdc1" b 8 33
    test -b "/dev/sdc2" || mknod -m 600 "/dev/sdc2" b 8 34
    test -b "/dev/sdc3" || mknod -m 600 "/dev/sdc3" b 8 35
fi
