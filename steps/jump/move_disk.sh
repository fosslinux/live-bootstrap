#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -ex

# Create partition if it doesn't exist
if [ $(($(stat -c "%Lr" "/dev/${DISK}") % 8)) -eq 0 ]; then
    echo "Creating partition table..."
    echo ";" | sfdisk "/dev/${DISK}"
    fdisk -l "/dev/${DISK}"
    echo "Creating ext4 partition..."
    mkfs.ext4 "/dev/${DISK}1"
    DISK="${DISK}1"
fi

# Mount the partition, move everything into /external
mkdir -p /newroot
mount -t ext4 "/dev/${DISK}" /newroot
mkdir -p /newroot/external
mv /newroot/* /newroot/external/ 2>/dev/null || true # obviously errors trying to move external into itself

# Switch root
mkdir -p /rootonly
# This doesn't recursively mount - that's why we're able to copy everything over
mount --bind / /rootonly
cp -ar /rootonly/* /newroot/
sed -e 's/newroot//' /rootonly/etc/mtab | grep -v 'rootonly' > /newroot/etc/mtab
umount /rootonly
switch_root /newroot /init
