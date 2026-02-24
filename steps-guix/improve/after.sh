#!/bin/sh
#
# SPDX-License-Identifier: GPL-3.0-or-later

mkdir -p /etc
if [ ! -e /etc/mtab ]; then
    ln -s /proc/mounts /etc/mtab
fi

exec /steps/improve/after.sh
