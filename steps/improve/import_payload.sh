#!/bin/sh
#
# SPDX-FileCopyrightText: 2026 live-bootstrap contributors
# SPDX-License-Identifier: MIT

set -ex

if [ "${PAYLOAD_REQUIRED}" = True ]; then
    mkdir -p /external/distfiles
    payload-import /external/distfiles
fi
