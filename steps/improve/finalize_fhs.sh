#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

mkdir -p /etc /run /var/log /var/lock /var/spool /var/tmp /var/cache

# can't use /dev/null before mounting /dev
mount | grep '/dev' &> /junk || (mkdir -p /dev; mount -t devtmpfs none /dev)
rm /junk &> /dev/null || true

mount | grep '/proc' &> /dev/null || (mkdir -p /proc; mount -t proc proc /proc)
mount | grep '/sys' &> /dev/null || (mkdir -p /sys; mount -t sysfs sysfs /sys)
# Make /tmp a ramdisk (speeds up configure etc significantly)
mount | grep '/tmp' &> /dev/null || (mkdir -p /tmp; mount -t tmpfs tmpfs /tmp)
mount | grep '/dev/shm' &> /dev/null || (mkdir -p /dev/shm; mount -t tmpfs tmpfs /dev/shm)

if [ "${CHROOT}" = False ]; then
    rm /etc/mtab
    ln -s /proc/mounts /etc/mtab
fi

# Add /etc/resolv.conf
if [ ! -e "/etc/resolv.conf" ]; then
    echo 'nameserver 1.1.1.1' > /etc/resolv.conf
fi
if [ ! -e "/etc/resolv.conf/head" ]; then
    echo 'nameserver 1.1.1.1' > /etc/resolv.conf.head
fi

# /etc/passwd -- taken from LFS
if [ ! -e "/etc/passwd" ]; then
    cat > /etc/passwd << "EOF"
root:x:0:0:root:/root:/bin/bash
bin:x:1:1:bin:/dev/null:/usr/bin/false
daemon:x:6:6:Daemon User:/dev/null:/usr/bin/false
messagebus:x:18:18:D-Bus Message Daemon User:/run/dbus:/usr/bin/false
uuidd:x:80:80:UUID Generation Daemon User:/dev/null:/usr/bin/false
nobody:x:65534:65534:Unprivileged User:/dev/null:/usr/bin/false
EOF
fi

# /etc/group -- taken from LFS
if [ ! -e "/etc/group" ]; then
    cat > /etc/group << "EOF"
root:x:0:
bin:x:1:daemon
sys:x:2:
kmem:x:3:
tape:x:4:
tty:x:5:
daemon:x:6:
floppy:x:7:
disk:x:8:
lp:x:9:
dialout:x:10:
audio:x:11:
video:x:12:
utmp:x:13:
usb:x:14:
cdrom:x:15:
adm:x:16:
messagebus:x:18:
input:x:24:
mail:x:34:
kvm:x:61:
uuidd:x:80:
wheel:x:97:
users:x:999:
nogroup:x:65534:
EOF
fi
