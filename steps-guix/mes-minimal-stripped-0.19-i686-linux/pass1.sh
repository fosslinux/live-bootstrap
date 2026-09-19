# SPDX-License-Identifier: GPL-3.0-or-later

src_get() { :; }
src_unpack() {
    mkdir -p mes-minimal-stripped-0.19-i686-linux
}
src_prepare() { :; }
src_configure() { :; }

src_compile() {
    local stage mes_src
    stage="/tmp/mes-minimal-stripped-0.19-i686-linux"
    mes_src=/usr/bin/mes-m2

    rm -rf "${stage}"
    mkdir -p "${stage}/bin"

    # steps/mes-0.27.1/pass1.kaem installs the Mes binary as /usr/bin/mes-m2.
    # Guix bootstrap seed expects the binary name "mes" inside the tarball.
    seed_require_file "${mes_src}"
    seed_install_exec "${mes_src}" "${stage}/bin/mes"
    seed_make_repro_tar_xz "${stage}" "${DISTFILES}/mes-minimal-stripped-0.19-i686-linux.tar.xz"
}

src_install() {
    install -D -m 0644 /dev/null "${DESTDIR}/usr/share/guix-seeds/mes-minimal-stripped-0.19-i686-linux"
}
