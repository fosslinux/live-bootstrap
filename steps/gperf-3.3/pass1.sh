# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm doc/gperf.{1,dvi,info,pdf,ps} doc/*.html
    touch doc/gperf.info doc/gperf.pdf

    # Useless tests things
    rm tests/*.exp tests/{languages,charsets}.gperf tests/lang-ucs2.in

    # Remove date from manpage
    sed -i 's/"$date" //' doc/help2man
    chmod +x doc/help2man

    GNULIB_SRCDIR=$(realpath ../gnulib-b08ee1d) \
        AUTOCONF=autoconf-2.71 \
        ./autogen.sh
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true PREFIX="${PREFIX}"
}

src_install() {
    make MAKEINFO=true PREFIX="${PREFIX}" DESTDIR="${DESTDIR}" install
}
