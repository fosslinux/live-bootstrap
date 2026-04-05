#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

dist="${DISTFILES:-/external/distfiles}"
xz_src="/bootstrap-seeds/xz-5.0.6/bin/xz"
xz_dst="${dist}/bootstrap-exec-xz-i686-linux"

if [ ! -x "${xz_src}" ]; then
    echo "Missing required bootstrap seed executable: ${xz_src}" >&2
    exit 1
fi

install -D -m 0755 "${xz_src}" "${xz_dst}"
