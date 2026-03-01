#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if match x${PAYLOAD_REQUIRED} xTrue; then
    mkdir -p /dev
    rm -f /dev/sda /dev/sdb /dev/sdc /dev/sdd
    mknod -m 600 /dev/sda b 8 0
    mknod -m 600 /dev/sdb b 8 16
    mknod -m 600 /dev/sdc b 8 32
    mknod -m 600 /dev/sdd b 8 48

    mkdir -p /external/distfiles
    payload-import /external/distfiles
fi
