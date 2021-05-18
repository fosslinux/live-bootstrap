# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Remove pre-generated files from source code.
remove_generated_files() {
    find . -name '*.info' -delete
    find . -name '*.gmo' -delete
}

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

    # We don't have autopoint from gettext yet.
    AUTOPOINT=true autoreconf-2.69 -fi
}

src_prepare() {
    default
    remove_generated_files
    regenerate_files
}

src_configure() {
    # FORCE_UNSAFE_CONFIGURE disables "you should not run configure as root"
    # error from configuration system of coreutils.
    FORCE_UNSAFE_CONFIGURE=1 ./configure CFLAGS="-static" \
        --prefix="${PREFIX}" \
        --target=i386-unknown-linux-gnu \
        --host=i386-unknown-linux-gnu \
        --build=i386-unknown-linux-gnu
}

src_compile() {
    make PREFIX="${PREFIX}" MAKEINFO="true"
}

src_install() {
    make install PREFIX="${PREFIX}" MAKEINFO="true" DESTDIR="${DESTDIR}"
}
