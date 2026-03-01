#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if match x${PAYLOAD_REQUIRED} xTrue; then
    mkdir -p /dev
    test -b /dev/sda || mknod -m 600 /dev/sda b 8 0
    test -b /dev/sdb || mknod -m 600 /dev/sdb b 8 16
    test -b /dev/sdc || mknod -m 600 /dev/sdc b 8 32
    test -b /dev/sdd || mknod -m 600 /dev/sdd b 8 48

    mkdir -p /external/distfiles
    if test -f /external/distfiles/.payload_imported; then
        exit 0
    fi

    payload-import /external/distfiles
    touch /external/distfiles/.payload_imported
fi
