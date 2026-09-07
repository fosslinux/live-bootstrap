#!/bin/sh
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

# shellcheck disable=SC1091
. /steps/bootstrap.cfg

# For VM/metal runs, stop immediately so the current disk state can be reused.
if [ "${CHROOT}" = False ]; then
    swapoff -a >/dev/null 2>&1 || true
    sync
    echo u > /proc/sysrq-trigger || true
    mount -o remount,ro / || true
    echo o > /proc/sysrq-trigger || poweroff -f || true
    while true; do sleep 1; done
fi

exit 0
