#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

dist="${DISTFILES:-/external/distfiles}"
work="/tmp/guix-bootstrap-artifacts-i686-linux"

require_exec() {
    local path="$1"
    if [ ! -x "${path}" ]; then
        echo "Missing required bootstrap seed executable: ${path}" >&2
        exit 1
    fi
}

require_dir() {
    local path="$1"
    if [ ! -d "${path}" ]; then
        echo "Missing required bootstrap seed directory: ${path}" >&2
        exit 1
    fi
}

export_exec() {
    local src="$1"
    local dst="$2"

    require_exec "${src}"
    install -D -m 0755 "${src}" "${dist}/${dst}"
}

build_static_binaries_seed() {
    local stage
    stage="${work}/static-binaries-0-i686-linux"

    rm -rf "${stage}"
    mkdir -p "${stage}/bin"

    require_dir /bootstrap-seeds/coreutils-8.30/bin
    cp -a /bootstrap-seeds/coreutils-8.30/bin/. "${stage}/bin/"
    for src in \
        /bootstrap-seeds/gawk-4.2.1/bin/gawk \
        /bootstrap-seeds/grep-3.1/bin/grep \
        /bootstrap-seeds/grep-3.1/bin/egrep \
        /bootstrap-seeds/grep-3.1/bin/fgrep \
        /bootstrap-seeds/sed-4.5/bin/sed \
        /bootstrap-seeds/tar-1.30/bin/tar \
        /bootstrap-seeds/xz-5.2.4/bin/xz \
        /bootstrap-seeds/gzip-1.9/bin/gzip \
        /bootstrap-seeds/bzip2-1.0.6/bin/bzip2 \
        /bootstrap-seeds/patch-2.7.6/bin/patch \
        /bootstrap-seeds/bash-5.3-1/bin/bash
    do
        require_exec "${src}"
        install -m 0755 "${src}" "${stage}/bin/"
    done

    ln -sf bash "${stage}/bin/sh"
    ln -sf gawk "${stage}/bin/awk"

    seed_make_repro_tar_xz "${stage}" "${dist}/static-binaries-0-i686-linux.tar.xz"
}

build_guile_seed() {
    require_dir /bootstrap-seeds/guile-2.0.9
    seed_make_repro_tar_xz \
        /bootstrap-seeds/guile-2.0.9 \
        "${dist}/guile-static-stripped-2.0.9-i686-linux.tar.xz"
}

build_mes_minimal_seed() {
    local stage
    stage="${work}/mes-minimal-stripped-0.19-i686-linux"

    rm -rf "${stage}"
    mkdir -p "${stage}/bin"

    require_exec /usr/bin/mes-m2
    install -m 0755 /usr/bin/mes-m2 "${stage}/bin/mes"

    seed_make_repro_tar_xz "${stage}" "${dist}/mes-minimal-stripped-0.19-i686-linux.tar.xz"
}

build_mescc_tools_seed() {
    local stage copied
    stage="${work}/mescc-tools-static-stripped-0.5.2-i686-linux"
    copied=0

    rm -rf "${stage}"
    mkdir -p "${stage}/bin"

    for f in M1 hex2 blood-elf kaem M2-Planet M2-Mesoplanet get_machine; do
        if [ -x "/usr/bin/${f}" ]; then
            install -m 0755 "/usr/bin/${f}" "${stage}/bin/${f}"
            copied=1
        fi
    done

    if [ "${copied}" -ne 1 ]; then
        echo "No mescc-tools binaries were found under /usr/bin." >&2
        exit 1
    fi

    seed_make_repro_tar_xz "${stage}" "${dist}/mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz"
}

rm -rf "${work}"
mkdir -p "${work}"

build_static_binaries_seed
build_guile_seed
build_mes_minimal_seed
build_mescc_tools_seed

export_exec /bootstrap-seeds/bash-5.3-1/bin/bash bootstrap-exec-bash-i686-linux
export_exec /bootstrap-seeds/coreutils-8.30/bin/mkdir bootstrap-exec-mkdir-i686-linux
export_exec /bootstrap-seeds/tar-1.30/bin/tar bootstrap-exec-tar-i686-linux
export_exec /bootstrap-seeds/xz-5.0.6/bin/xz bootstrap-exec-xz-i686-linux
