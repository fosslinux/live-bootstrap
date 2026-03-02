#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if [ "${PAYLOAD_REQUIRED}" = True ]; then
    mkdir -p /external/distfiles
    found_payload=0
    mkdir -p /dev

    if [ ! -r /proc/partitions ]; then
        echo "payload-import failed: /proc/partitions is unavailable." >&2
        exit 1
    fi

    while read -r major minor blocks name; do
        case "${major}" in
            ""|major|*[!0-9]*)
                continue
                ;;
        esac
        case "${minor}" in
            ""|minor|*[!0-9]*)
                continue
                ;;
        esac

        dev_path="/dev/lbpayload-${major}-${minor}"
        [ -b "${dev_path}" ] || mknod -m 600 "${dev_path}" b "${major}" "${minor}" >/dev/null 2>&1 || :

        if payload-import --probe "${dev_path}" >/dev/null 2>&1; then
            payload-import --device "${dev_path}" /external/distfiles
            found_payload=1
            break
        fi
    done < /proc/partitions

    if [ "${found_payload}" != 1 ]; then
        echo "payload-import failed: no payload image found in /proc/partitions devices." >&2
        exit 1
    fi
fi
