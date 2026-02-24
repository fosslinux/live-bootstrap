#!/bin/sh
#
# SPDX-License-Identifier: GPL-3.0-or-later

mkdir -p /proc /etc
if [ ! -r /proc/mounts ]; then
    mount -t proc proc /proc
fi
ln -snf /proc/mounts /etc/mtab

exec /steps/improve/finalize_fhs.sh
