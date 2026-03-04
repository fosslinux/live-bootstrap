# SPDX-License-Identifier: GPL-3.0-or-later

src_get() { :; }
src_unpack() { :; }
src_prepare() { :; }
src_configure() { :; }
BUILD_DIRNAME=.

src_compile() {
    local stage
    stage="/tmp/mes-minimal-stripped-0.19-i686-linux"

    rm -rf "${stage}"
    mkdir -p "${stage}/bin"

    seed_install_exec /usr/bin/mes "${stage}/bin/mes"
    seed_make_repro_tar_xz "${stage}" "${DISTFILES}/mes-minimal-stripped-0.19-i686-linux.tar.xz"
}

src_install() {
    install -D -m 0644 /dev/null "${DESTDIR}/usr/share/guix-seeds/mes-minimal-stripped-0.19-i686-linux"
}
