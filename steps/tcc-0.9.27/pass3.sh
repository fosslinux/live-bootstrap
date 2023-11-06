# SPDX-FileCopyrightText: 2021-2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022-23 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    touch config.h
}

src_compile() {
    # We first have to recompile using tcc-0.9.26 as tcc-0.9.27 is not self-hosting,
    # but when linked with musl it is.
    ln -sf "${PREFIX}/lib/mes/tcc/libtcc1.a" ./libtcc1.a

    for TCC in tcc-0.9.26 ./tcc-musl; do
        "${TCC}" \
            -v \
            -static \
            -o tcc-musl \
            -D TCC_TARGET_I386=1 \
            -D CONFIG_TCCDIR=\""${LIBDIR}/tcc"\" \
            -D CONFIG_TCC_CRTPREFIX=\""${LIBDIR}"\" \
            -D CONFIG_TCC_ELFINTERP=\"/musl/loader\" \
            -D CONFIG_TCC_LIBPATHS=\""${LIBDIR}:${LIBDIR}/tcc"\" \
            -D CONFIG_TCC_SYSINCLUDEPATHS=\""${PREFIX}/include"\" \
            -D TCC_LIBGCC=\""${LIBDIR}/libc.a"\" \
            -D CONFIG_TCC_STATIC=1 \
            -D CONFIG_USE_LIBGCC=1 \
            -D TCC_VERSION=\"0.9.27\" \
            -D ONE_SOURCE=1 \
            -B . \
            tcc.c

        # libtcc1.a
        rm -f libtcc1.a
        ${TCC} -c -D HAVE_CONFIG_H=1 lib/libtcc1.c
        ${TCC} -ar cr libtcc1.a libtcc1.o
    done
}

src_install() {
    install -D tcc-musl "${DESTDIR}${PREFIX}/bin/tcc-musl"
    ln -s tcc-musl "${DESTDIR}${PREFIX}/bin/tcc"
    install -D libtcc1.a "${DESTDIR}${LIBDIR}/tcc/libtcc1.a"
}
