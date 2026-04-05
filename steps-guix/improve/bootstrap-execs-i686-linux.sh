#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

dist="${DISTFILES:-/external/distfiles}"

install_exec() {
    local src="$1"
    local dst="$2"

    if [ ! -x "${src}" ]; then
        echo "Missing required bootstrap seed executable: ${src}" >&2
        exit 1
    fi

    install -D -m 0755 "${src}" "${dist}/${dst}"
}

install_exec /bootstrap-seeds/bash-5.3-1/bin/bash bootstrap-exec-bash-i686-linux
install_exec /bootstrap-seeds/coreutils-8.30/bin/mkdir bootstrap-exec-mkdir-i686-linux
install_exec /bootstrap-seeds/tar-1.30/bin/tar bootstrap-exec-tar-i686-linux
install_exec /bootstrap-seeds/xz-5.0.6/bin/xz bootstrap-exec-xz-i686-linux
