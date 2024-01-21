#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Add the rest of the FHS that we will use and is not created pre-boot
rm -rf /sbin /usr/sbin
ln -s /usr/bin /usr/sbin
for d in bin lib sbin; do
    ln -s "/usr/${d}" "/${d}" || true # these might exist if rerunning
done

mkdir -p /etc /run /var/log /var/lock /var/spool /var/tmp /var/cache

# can't use /dev/null before mounting /dev
mount | grep '/dev' &> /junk || (mkdir -p /dev; mount -t devtmpfs none /dev)
rm /junk &> /dev/null || true

mount | grep '/proc' &> /dev/null || (mkdir -p /proc; mount -t proc proc /proc)
mount | grep '/sys' &> /dev/null || (mkdir -p /sys; mount -t sysfs sysfs /sys)
# Make /tmp a ramdisk (speeds up configure etc significantly)
mount | grep '/tmp' &> /dev/null || (mkdir -p /tmp; mount -t tmpfs tmpfs /tmp)
mount | grep '/dev/shm' &> /dev/null || (mkdir -p /dev/shm; mount -t tmpfs tmpfs /dev/shm)

# Add /etc/resolv.conf
echo 'nameserver 1.1.1.1' > /etc/resolv.conf
echo 'nameserver 1.1.1.1' > /etc/resolv.conf.head
