# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove pregenerated gnulib files
    pushd ../gnulib-d271f86
    rm lib/unictype/ctype*.h
    rm lib/unicase/tolower.h
    popd

    . ../../import-gnulib.sh

    for patchname in fix-base64 fix-null-deref fix-null-state-deref fix-regcomp-uninit-token \
        fix-regexec-null-deref fix-uninit-structure fix-unused-value fix-width no-abort; do
        patch -d grub-core/lib/gnulib -p2 < "grub-core/lib/gnulib-patches/$patchname.patch"
    done

    ./autogen.sh

    cp -a INSTALL INSTALL.grub
    autoreconf-2.69 -vif
    mv INSTALL.grub INSTALL
}

src_configure() {
    CFLAGS="-Wno-error" ./configure --prefix="${PREFIX}" --sbindir="${PREFIX}/bin" --build=i686-pc-linux-musl
}

src_install() {
    default
    rm "${DESTDIR}${PREFIX}/share/info/dir"
    rm "${DESTDIR}${PREFIX}/share/man/man8/grub-install.8"
}
