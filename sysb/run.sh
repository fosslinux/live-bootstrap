#!/usr/bin/bash

# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

. bootstrap.cfg

export PATH=/usr/bin:/usr/sbin

# Unload the current kernel before things go weird
kexec -u

populate_device_nodes() {
    # http://www.linuxfromscratch.org/lfs/view/6.1/chapter06/devices.html
    test -c /dev/null || mknod -m 666 /dev/null c 1 3
    test -c /dev/zero || mknod -m 666 /dev/zero c 1 5
    test -c /dev/ptmx || mknod -m 666 /dev/ptmx c 5 2
    test -c /dev/tty || mknod -m 666 /dev/tty c 5 0
    test -c /dev/random || mknod -m 444 /dev/random c 1 8
    test -c /dev/urandom || mknod -m 444 /dev/urandom c 1 9
}

create_hdx() {
    # Create all of the sd{a,b,c..}
    minor=0
    alpha="a b c d e f g h i j k l m n o p" # 16 disks -- more than enough
    # For each disk...
    for a in ${alpha}; do
        mknod -m 600 "/dev/sd${a}" b 8 "$((minor++))"
        # For each partition...
        for p in $(seq 15); do
            mknod -m 600 "/dev/sd${a}${p}" b 8 "$((minor++))"
        done
    done
}

# If there is no disk specified error out
if [ -z "${DISK}" ]; then
    echo "You must specify a disk where sysb will be located!"
    exit 1
fi

# Otherwise, add stuff from sysa to sysb
echo "Mounting sysc"
mkdir /sysc
# All the various structures that don't exist but needed to mount
mkdir -p /etc /dev
populate_device_nodes
create_hdx
mount -t ext4 "/dev/${DISK}" /sysc

# Copy over appropriate data
echo "Copying data into sysc"
cp -r /dev /sysc/
# Don't include /usr/src
find /usr -mindepth 1 -maxdepth 1 -type d -not -name src -exec cp -r {} /sysc/{} \;
sync

# switch_root into sysc 1. for simplicity 2. to avoid kexecing again
# spouts a few errors because we don't have /proc /sys or /dev mounted
echo "Switching into sysc"
exec switch_root /sysc /init
