# SPDX-License-Identifier: GPL-3.0-or-later

src_get() { :; }
src_unpack() {
    mkdir -p static-binaries-0-i686-linux
}
src_prepare() { :; }
src_configure() { :; }

src_compile() {
    local stage dist
    stage="/tmp/static-binaries-0-i686-linux"
    dist="${DISTFILES}"

    rm -rf "${stage}"
    mkdir -p "${stage}/bin"

    cp -a /bootstrap-seeds/coreutils-8.30/bin/. "${stage}/bin/"
    cp -a /bootstrap-seeds/gawk-4.2.1/bin/gawk "${stage}/bin/"
    cp -a /bootstrap-seeds/grep-3.1/bin/grep "${stage}/bin/"
    cp -a /bootstrap-seeds/grep-3.1/bin/egrep "${stage}/bin/"
    cp -a /bootstrap-seeds/grep-3.1/bin/fgrep "${stage}/bin/"
    cp -a /bootstrap-seeds/sed-4.5/bin/sed "${stage}/bin/"
    cp -a /bootstrap-seeds/tar-1.30/bin/tar "${stage}/bin/"
    cp -a /bootstrap-seeds/xz-5.2.4/bin/xz "${stage}/bin/"
    cp -a /bootstrap-seeds/gzip-1.9/bin/gzip "${stage}/bin/"
    cp -a /bootstrap-seeds/bzip2-1.0.6/bin/bzip2 "${stage}/bin/"
    cp -a /bootstrap-seeds/patch-2.7.6/bin/patch "${stage}/bin/"
    cp -a /bootstrap-seeds/bash-5.3-1/bin/bash "${stage}/bin/"

    ln -sf bash "${stage}/bin/sh"
    ln -sf gawk "${stage}/bin/awk"

    seed_make_repro_tar_xz "${stage}" "${dist}/static-binaries-0-i686-linux.tar.xz"

    seed_install_exec "${stage}/bin/bash" "${dist}/bootstrap-exec-bash-i686-linux"
    seed_install_exec "${stage}/bin/mkdir" "${dist}/bootstrap-exec-mkdir-i686-linux"
    seed_install_exec "${stage}/bin/tar" "${dist}/bootstrap-exec-tar-i686-linux"
}

src_install() {
    install -D -m 0644 /dev/null "${DESTDIR}/usr/share/guix-seeds/static-binaries-0-i686-linux"
}
