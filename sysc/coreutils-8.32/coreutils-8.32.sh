# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=d43d89ea6efb13cc84ad7fd218ca4fa19c07fa6ab308edf9df62919ad8b00359

regenerate_files() {
    build-aux/gen-lists-of-programs.sh --autoconf > m4/cu-progs.m4
    build-aux/gen-lists-of-programs.sh --automake > src/cu-progs.mk
    build-aux/gen-single-binary.sh src/local.mk > src/single-binary.mk
    touch ChangeLog
    cp ../gnulib-d279bc/build-aux/po/Makefile.in.in po/Makefile.in.in

    . ../../import-gnulib.sh

    # Disable generation of man pages due to lack of needed perl 5.8
    # dependency.
    cp man/dummy-man man/help2man

    VERSION=$(basename ${BASH_SOURCE[0]} .sh | sed 's/coreutils-//')
    echo $VERSION > .tarball-version

    # We don't have autopoint from gettext yet.
    AUTOPOINT=true autoreconf-2.69 -fi
}

src_prepare() {
    default
    regenerate_files
}

src_configure() {
    # FORCE_UNSAFE_CONFIGURE disables "you should not run configure as root"
    # error from configuration system of coreutils.
    FORCE_UNSAFE_CONFIGURE=1 ./configure CFLAGS="-static" \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl
}

src_compile() {
    make PREFIX="${PREFIX}" MAKEINFO="true"
}

src_install() {
    make install PREFIX="${PREFIX}" MAKEINFO="true" DESTDIR="${DESTDIR}"
}
