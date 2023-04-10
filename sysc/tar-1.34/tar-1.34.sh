# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    . ../../import-gnulib.sh

    # We don't have autopoint from gettext yet
    AUTOPOINT=true AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi

    # Remove bison pregenerated file
    rm gnu/parse-datetime.c
}

src_configure() {
    # gl_cv_func_getcwd_path_max is set to improve reproducibility.
    # In some environments, the configure script would set it to
    # "no, but it is partly working", and in others it would set it
    # to "yes", producing different build outputs.
    FORCE_UNSAFE_CONFIGURE=1 ./configure \
        --prefix="${PREFIX}" \
        --disable-nls \
        gl_cv_func_getcwd_path_max="no, but it is partly working"
}

src_compile() {
    make "${MAKEJOBS}" PREFIX="${PREFIX}" MAKEINFO="true"
}

src_install() {
    make install PREFIX="${PREFIX}" MAKEINFO="true" DESTDIR="${DESTDIR}"
}
