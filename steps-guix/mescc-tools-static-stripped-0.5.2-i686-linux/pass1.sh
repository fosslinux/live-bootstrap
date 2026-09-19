# SPDX-License-Identifier: GPL-3.0-or-later

src_get() { :; }
src_unpack() {
    mkdir -p mescc-tools-static-stripped-0.5.2-i686-linux
}
src_prepare() { :; }
src_configure() { :; }

src_compile() {
    local stage copied
    stage="/tmp/mescc-tools-static-stripped-0.5.2-i686-linux"
    copied=0

    rm -rf "${stage}"
    mkdir -p "${stage}/bin"

    for f in M1 hex2 blood-elf kaem M2-Planet M2-Mesoplanet get_machine; do
        if [ -x "/usr/bin/${f}" ]; then
            seed_install_exec "/usr/bin/${f}" "${stage}/bin/${f}"
            copied=1
        fi
    done

    if [ "${copied}" -ne 1 ]; then
        echo "No mescc-tools binaries were found under /usr/bin." >&2
        false
    fi

    seed_make_repro_tar_xz "${stage}" "${DISTFILES}/mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz"
}

src_install() {
    install -D -m 0644 /dev/null "${DESTDIR}/usr/share/guix-seeds/mescc-tools-static-stripped-0.5.2-i686-linux"
}
