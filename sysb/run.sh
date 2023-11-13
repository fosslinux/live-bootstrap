#!/usr/bin/bash

# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck source=/dev/null
. .env

# shellcheck source=sysa/helpers.sh
. helpers.sh

# Unload the current kernel before things go weird
kexec -u

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

# All the various structures that don't exist but needed to mount
mkdir -p /etc /dev
populate_device_nodes
create_hdx

ask_disk() {
    echo
    echo "What disk would you like to use for live-bootstrap?"
    echo "This disk may have pre-prepared sources on it."
    echo "If there is no partition we will make one".
    echo "Please provide in format sdxx (as you would find under /dev),"
    echo "or sdx if it is a blank disk. An ext4 partition is expected on"
    echo "existing disks."
    echo "You can type 'list' to get a list of disks to help you figure"
    echo "out which is the right disk."
    echo "NO WARRANTY IS PROVIDED FOR BUGGY BEHAVIOUR, INCLUDING THAT"
    echo "REGARDING DISKS & DATA."
    echo
    read -r DISK

    if [ "${DISK}" = "list" ]; then
        fdisk -l
        ask_disk
    elif [ -z "${DISK}" ] || ! [ -e "/dev/${DISK}" ]; then
        echo "Invalid."
        ask_disk
    fi
}

if [ -z "${DISK}" ] || ! [ -e "/dev/${DISK}" ]; then
    echo "You did not provide a valid disk in the configuration file."
    ask_disk

    echo "DISK=${DISK}" >> /usr/src/bootstrap.cfg
fi

# Is it a full disk, and not a partition?
# shellcheck disable=SC2012
if [ $(($(ls -l "/dev/${DISK}" | sed "s/.*, *//" | sed "s/ .*//") % 8)) -eq 0 ]; then
    echo "Creating partition table..."
    # Convince our ancient sfdisk to align to megabytes, not cylinders
    echo "2048;" | sfdisk -uS -S32 -H64 "/dev/${DISK}"
    fdisk -l "/dev/${DISK}"
    echo "Creating ext4 partition..."
    mkfs.ext4 "/dev/${DISK}1"
    DISK="${DISK}1"
fi
echo "DISK=${DISK}" >> /usr/src/bootstrap.cfg

SYSC=/sysc

# Otherwise, add stuff from sysa to sysb
echo "Mounting sysc"
mkdir /sysc
mount -t ext4 "/dev/${DISK}" /sysc

# Copy over appropriate data
echo "Copying data into sysc"
sys_transfer "${SYSC}" /sysc_src gzip patch
cp -r /sysa.tar.bz2 /sysc/sysa.tar.bz2
cp -r /sysc_src/sysa_saved.tar.bz2 /sysc/sysa_saved.tar.bz2
sync

# switch_root into sysc 1. for simplicity 2. to avoid kexecing again
# spouts a few errors because we don't have /proc /sys or /dev mounted
echo "Switching into sysc"
exec switch_root /sysc /init
