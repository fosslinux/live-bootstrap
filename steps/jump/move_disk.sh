#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e
# mount might fail if /etc doesn't exist because of fstab and mtab
mkdir -p /dev /etc
mount -t devtmpfs none /dev &> /junk || true # no /dev/null yet
rm /junk &> /dev/null || true

timeout=120
while ! dd if=/dev/${DISK} of=/dev/null bs=512 count=1; do
    sleep 1
    # shellcheck disable=SC2219
    let timeout--
    if [ "${timeout}" -le 0 ]; then
        echo "Timeout reached for disk to become accessible"
        false
    fi
done

# Create partition if it doesn't exist
# 'stat -c "%T"' prints the minor device type in hexadecimal.
# The decimal version (with "%Lr") is not available in this version of stat.
if [ $((0x$(stat -c "%T" "/dev/${DISK}") % 8)) -eq 0 ]; then
    echo "Creating partition table..."
    # Start at 1GiB, use -S32 -H64 to align to MiB rather than cylinder boundary
    echo "2097152;" | sfdisk -uS -S32 -H64 --force "/dev/${DISK}"
    fdisk -l "/dev/${DISK}"
    echo "Creating ext4 partition..."
    mkfs.ext4 -F -F "/dev/${DISK}1"
    DISK="${DISK}1"
    echo DISK="${DISK}" >> /steps/bootstrap.cfg
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
