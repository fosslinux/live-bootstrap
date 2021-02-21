# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    # Our cp does not support recursive copying
    tar -c -C ../src/ -f tcc-0.9.27.tar tcc-0.9.27/
    tar -xf tcc-0.9.27.tar
}

src_prepare() {
    patch -Np0 -i ../../patches/ignore-duplicate-symbols.patch
}

src_compile() {
    export prefix=/after
    export libdir=${prefix}/lib/musl
    export incdir=${prefix}/include/musl
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
        tcc.c

    # libtcc1.a
    ${TCC} -c -D HAVE_CONFIG_H=1 lib/libtcc1.c
    ${TCC} -ar cr libtcc1.a libtcc1.o
}

src_install() {
    install tcc-musl ${bindir}
    install -m 644 libtcc1.a ${libdir}
}
