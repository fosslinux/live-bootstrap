#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if [ "${PAYLOAD_REQUIRED}" = True ]; then
    mkdir -p /external/distfiles
    if ! payload-import /external/distfiles; then
        echo "payload-import failed: no payload image found on block devices." >&2
        exit 1
    fi
fi
