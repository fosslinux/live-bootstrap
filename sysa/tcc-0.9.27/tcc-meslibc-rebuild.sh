# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=66c799097c8dcc5505b383dcb07ce77fc6720b58e8441311fdb4b4dc6ede66ba

src_prepare() {
    default

    touch config.h
}

src_compile() {
    export libdir=${PREFIX}/lib/mes
    export incdir=${PREFIX}/include
    export bindir=${PREFIX}/bin

    mkdir -p ${libdir}/tcc

    tcc-0.9.26 \
        -v \
        -o tcc \
        -D TCC_TARGET_I386=1 \
        -D CONFIG_TCCDIR=\"${libdir}/tcc\" \
        -D CONFIG_TCC_CRTPREFIX=\"${libdir}\" \
        -D CONFIG_TCC_ELFINTERP=\"/mes/loader\" \
        -D CONFIG_TCC_LIBPATHS=\"${libdir}:${libdir}/tcc\" \
        -D CONFIG_TCC_SYSINCLUDEPATHS=\"${incdir}\" \
        -D TCC_LIBGCC=\"${libdir}/libc.a\" \
        -D CONFIG_TCC_STATIC=1 \
        -D CONFIG_USE_LIBGCC=1 \
        -D TCC_VERSION=\"0.9.27\" \
        -D ONE_SOURCE=1 \
        tcc.c
}

src_install() {
    install -D tcc ${DESTDIR}${bindir}/tcc
}
