# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later


src_prepare() {
    default

    # Remove unused generated files
    rm etc/Makefile.in etc/configure

    # Regenerate files
    for dir in bfd binutils gas gprof intl ld libiberty opcodes zlib; do
    (
        cd $dir
        AUTOPOINT=true ACLOCAL=aclocal-1.11 AUTOMAKE=automake-1.11 autoreconf-2.64 -fi
    )
    done

    ACLOCAL=aclocal-1.11 autoreconf-2.64 -fi

    # Rebuild bison files
    touch -- */*.y
    rm binutils/arparse.c binutils/arparse.h
    rm binutils/defparse.c binutils/defparse.h
    rm binutils/mcparse.c binutils/mcparse.h
    rm binutils/rcparse.c binutils/rcparse.h
    rm binutils/sysinfo.c binutils/sysinfo.h
    rm gas/bfin-parse.c gas/bfin-parse.h
    rm gas/m68k-parse.c gas/rl78-parse.c
    rm gas/rl78-parse.h gas/rx-parse.c
    rm gas/rx-parse.h gas/itbl-parse.c
    rm gas/itbl-parse.h gold/yyscript.c
    rm gold/yyscript.h intl/plural.c
    rm ld/deffilep.c ld/deffilep.h
    rm ld/ldgram.c ld/ldgram.h

    # Rebuild flex generated files
    touch -- */*.l */*/*.l
    rm binutils/arlex.c binutils/deflex.c binutils/syslex.c
    rm gas/bfin-lex.c gas/itbl-lex.c
    rm ld/ldlex.c

    # Remove prebuilt texinfo files
    rm bfd/doc/bfd.info binutils/doc/binutils.info
    rm gas/doc/as.info gprof/gprof.info ld/ld.info

    # Remove pregenerated opcodes files
    rm opcodes/i386-init.h opcodes/i386-tbl.h
    rm opcodes/ia64-asmtab.c
    rm opcodes/z8k-opc.h
    rm opcodes/aarch64-asm-2.c opcodes/aarch64-opc-2.c opcodes/aarch64-dis-2.c
    rm $(grep -l 'MACHINE GENERATED' opcodes/*.c opcodes/*.h)

    # Regenerate MeP sections
    ./bfd/mep-relocs.pl

    # Manpages
    rm */*.1 */*/*.1 */*/*.man
}

src_configure() {
    for dir in intl libiberty opcodes bfd binutils gas gprof ld zlib; do
    (
        cd $dir

        LD="true" AR="tcc -ar" CC="tcc" ./configure \
            --disable-nls \
            --enable-deterministic-archives \
            --enable-64-bit-bfd \
            --build=i386-unknown-linux-gnu \
            --host=i386-unknown-linux-gnu \
            --target=i386-unknown-linux-gnu \
            --program-prefix="" \
            --prefix="${PREFIX}" \
            --libdir="${LIBDIR}" \
            --with-sysroot= \
            --srcdir=. \
            --enable-compressed-debug-sections=all \
            lt_cv_sys_max_cmd_len=32768
    )
    done
}

src_compile() {
    make -C bfd headers
    for dir in libiberty zlib bfd opcodes binutils gas gprof ld; do
        make "${MAKEJOBS}" -C $dir tooldir=${PREFIX} CPPFLAGS="-DPLUGIN_LITTLE_ENDIAN" MAKEINFO=true
    done
}

src_install() {
    for dir in libiberty zlib bfd opcodes binutils gas gprof ld; do
        make -C $dir tooldir=${PREFIX} DESTDIR="${DESTDIR}" install MAKEINFO=true
    done

    # Create triplet symlinks
    (
        cd "${DESTDIR}${PREFIX}/bin"
        for f in *; do
            ln -s "${PREFIX}/bin/${f}" "i386-unknown-linux-musl-${f}"
        done
    )
}
