# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

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

    VERSION=$(basename "${BASH_SOURCE[0]}" .sh | sed 's/coreutils-//')
    echo "$VERSION" > .tarball-version

    # We don't have autopoint from gettext yet.
    AUTOPOINT=true AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_prepare() {
    default
    regenerate_files
}

src_configure() {
    # FORCE_UNSAFE_CONFIGURE disables "you should not run configure as root"
    # error from configuration system of coreutils.
    # gl_cv_func_getcwd_path_max is set to improve reproducibility.
    # In some environments, the configure script would set it to
    # "no, but it is partly working", and in others it would set it
    # to "yes", producing different build outputs.
    FORCE_UNSAFE_CONFIGURE=1 ./configure CFLAGS="-static" \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl \
        gl_cv_func_getcwd_path_max="no, but it is partly working"
}

src_compile() {
    make PREFIX="${PREFIX}" MAKEINFO="true"
}

src_install() {
    make install PREFIX="${PREFIX}" MAKEINFO="true" DESTDIR="${DESTDIR}"
}
