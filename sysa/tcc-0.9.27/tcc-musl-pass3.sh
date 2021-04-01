# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    cp -r ../src/tcc-0.9.27 .
}

src_prepare() {
    # Note that tcc includes static-link and ignore-static-inside-array patches
    # since we do not build from clean checkout.

    :
}

src_compile() {
    export prefix=/after
    export libdir=${prefix}/lib/musl
    export incdir=${prefix}/include
    export bindir=${prefix}/bin

    mkdir -p ${libdir}/tcc

    tcc-musl \
        -v \
        -static \
        -o tcc-musl \
        -D TCC_TARGET_I386=1 \
        -D CONFIG_TCCDIR=\"${libdir}/tcc\" \
        -D CONFIG_TCC_CRTPREFIX=\"${libdir}\" \
        -D CONFIG_TCC_ELFINTERP=\"/musl/loader\" \
        -D CONFIG_TCC_LIBPATHS=\"${libdir}:${libdir}/tcc\" \
        -D CONFIG_TCC_SYSINCLUDEPATHS=\"${incdir}\" \
        -D TCC_LIBGCC=\"${libdir}/libc.a\" \
        -D CONFIG_TCC_STATIC=1 \
        -D CONFIG_USE_LIBGCC=1 \
        -D TCC_VERSION=\"0.9.27\" \
        -D ONE_SOURCE=1 \
        -I ${incdir} \
        tcc.c

    # libtcc1.a
    tcc-musl -c -D HAVE_CONFIG_H=1 lib/libtcc1.c
    ar cr libtcc1.a libtcc1.o
}

src_install() {
    install tcc-musl ${bindir}
    install -m 644 libtcc1.a ${libdir}
}
