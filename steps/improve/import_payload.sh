#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if [ "${PAYLOAD_REQUIRED}" = True ]; then
    mkdir -p /external/distfiles
    found_payload=0

    # Probe all block devices reported by the running kernel instead of
    # assuming fixed names like /dev/sdb or /dev/hdb.
    while read -r major minor blocks name; do
        case "${name}" in
            ""|name|ram*|loop*|fd*|sr*|md*)
                continue
                ;;
        esac

        dev_path="/dev/${name}"
        if [ ! -b "${dev_path}" ]; then
            mknod -m 600 "${dev_path}" b "${major}" "${minor}" || :
        fi

        if payload-import --probe "${dev_path}"; then
            payload-import --device "${dev_path}" /external/distfiles
            found_payload=1
            break
        fi
    done < /proc/partitions

    if [ "${found_payload}" != 1 ]; then
        echo "payload-import failed: no payload image found on detected block devices." >&2
        exit 1
    fi
fi
