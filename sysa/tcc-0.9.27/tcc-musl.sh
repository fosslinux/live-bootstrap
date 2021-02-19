# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    cp -r ../src/tcc-0.9.27 .
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

    # We first have to recompile using tcc-0.9.26 as tcc-0.9.27 is not self-hosting,
    # but when linked with musl it is.
    for TCC in tcc-0.9.26 tcc-musl; do
        ${TCC} \
            -v \
            -static \
            -o ${bindir}/tcc-musl \
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
        ${TCC} -ar cr ${libdir}/tcc/libtcc1.a libtcc1.o
    done
}

src_install() {
    # Remove old tcc binaries, keep one for tcc-0.9.27 with mes C library
    mv ${bindir}/tcc ${bindir}/tcc-mes
    rm ${bindir}/boot*-tcc ${bindir}/tcc-0.9.26 ${bindir}/mes-tcc
    ln -s ${bindir}/tcc-musl ${bindir}/tcc
}
