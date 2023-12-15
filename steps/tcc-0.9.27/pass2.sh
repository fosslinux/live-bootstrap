# SPDX-FileCopyrightText: 2021-2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022-23 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    touch config.h
}

src_compile() {
    # We have to compile using tcc-0.9.26 as tcc-0.9.27 is not self-hosting when built with mes
    tcc-0.9.26 \
        -v \
        -static \
        -o tcc \
        -D TCC_TARGET_I386=1 \
        -D CONFIG_TCCDIR=\""${LIBDIR}/tcc"\" \
        -D CONFIG_TCC_CRTPREFIX=\""${LIBDIR}"\" \
        -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
        -D CONFIG_TCC_LIBPATHS=\""${LIBDIR}:${LIBDIR}/tcc"\" \
        -D CONFIG_TCC_SYSINCLUDEPATHS=\""${PREFIX}/include/mes"\" \
        -D TCC_LIBGCC=\""${LIBDIR}/libc.a"\" \
        -D CONFIG_TCC_STATIC=1 \
        -D CONFIG_USE_LIBGCC=1 \
        -D TCC_VERSION=\"0.9.27\" \
        -D ONE_SOURCE=1 \
        tcc.c

    # libtcc1.a
    tcc-0.9.26 -c -D HAVE_CONFIG_H=1 lib/libtcc1.c
    tcc-0.9.26 -ar cr libtcc1.a libtcc1.o
}

src_install() {
    install -D libtcc1.a "${DESTDIR}${LIBDIR}/tcc/libtcc1.a"
    install -D tcc "${DESTDIR}${PREFIX}/bin/tcc"
}
