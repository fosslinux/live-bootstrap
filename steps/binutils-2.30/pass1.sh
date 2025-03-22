# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2023,2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove unused generated files
    rm etc/Makefile.in etc/configure

    # Rebuild bison files
    touch -- */*.y
    rm binutils/arparse.c binutils/arparse.h binutils/defparse.c \
        binutils/defparse.h binutils/mcparse.c binutils/mcparse.h \
        binutils/nlmheader.c binutils/rcparse.c binutils/rcparse.h \
        binutils/sysinfo.c binutils/sysinfo.h gas/bfin-parse.c \
        gas/bfin-parse.h gas/m68k-parse.c gas/rl78-parse.c gas/rl78-parse.h \
        gas/rx-parse.c gas/rx-parse.h gas/itbl-parse.c gas/itbl-parse.h \
        gold/yyscript.c gold/yyscript.h intl/plural.c ld/deffilep.c \
        ld/deffilep.h ld/ldgram.c ld/ldgram.h

    # Rebuild flex generated files
    touch -- */*.l */*/*.l
    rm binutils/arlex.c binutils/deflex.c binutils/syslex.c gas/bfin-lex.c \
        gas/itbl-lex.c ld/ldlex.c

    # Remove prebuilt documentation
    rm bfd/doc/bfd.info binutils/doc/binutils.info
    rm gas/doc/as.info gprof/gprof.info ld/ld.info
    rm */*.1 */*/*.1 */*/*.man
    rm libiberty/functions.texi

    # Remove gettext translation files
    rm */po/*.gmo

    # Remove pregenerated opcodes files
    rm opcodes/i386-init.h opcodes/i386-tbl.h opcodes/ia64-asmtab.c \
        opcodes/z8k-opc.h opcodes/aarch64-asm-2.c opcodes/aarch64-opc-2.c \
        opcodes/aarch64-dis-2.c
    rm $(grep -l 'MACHINE GENERATED' opcodes/*.c opcodes/*.h)
    rm include/opcode/riscv-opc.h

    # Generated headers and blobs
    rm bfd/go32stub.h bfd/libbfd.h bfd/bfd-in2.h bfd/libcoff.h \
        ld/emultempl/spu_icache.o_c ld/emultempl/spu_ovl.o_c \
        gprof/fsf_callg_bl.c gprof/bsd_callg_bl.c gprof/flat_bl.c

    # Generated testdata (xz-style attack vector)
    rm gas/testsuite/gas/sh/arch/*.s \
        gas/testsuite/gas/sh/arch/arch_expected.txt \
        ld/testsuite/ld-sh/arch/*.s \
        ld/testsuite/ld-sh/arch/arch_expected.txt \
        ld/testsuite/ld-versados/*.ro \
        gas/testsuite/gas/xstormy16/allinsn.sh \
        gas/testsuite/gas/tic4x/opcodes.s \
        binutils/testsuite/binutils-all/x86-64/pr22451.o.bz2

    # Clean out zlib/
    rm zlib/contrib/masmx86/*.obj \
        zlib/contrib/infback9/inffix9.h \
        zlib/contrib/blast/test.pk \
        zlib/contrib/puff/zeros.raw \
        zlib/contrib/dotzlib/DotZLib.chm \
        zlib/crc32.h zlib/inffixed.h

    # Regenerate crc table in libiberty/crc32.c
    cd libiberty
    sed -n '/^   #include <stdio.h>/,/^   \}$/p' crc32.c > crcgen.c
    tcc -o crcgen crcgen.c
    sed '/crc_v3\.txt/{n; q}' crc32.c > crc32.c.new
    ./crcgen >> crc32.c.new
    sed '1,/^};$/d' crc32.c >> crc32.c.new
    mv crc32.c.new crc32.c
    cd ..

    # bfd-in2.h is required to run autoreconf, but we don't have it yet
    cd bfd
    cp configure.ac configure.ac.bak
    sed -i "s/bfd-in3.h:bfd-in2.h //" configure.ac
    AUTOPOINT=true ACLOCAL=aclocal-1.11 AUTOMAKE=automake-1.11 autoreconf-2.64 -fi
    CC=tcc LD=true ./configure
    make headers
    mv configure.ac.bak configure.ac
    make clean
    cd ..

    # Regenerate files
    for dir in bfd binutils gas gprof gold intl ld libiberty opcodes zlib; do
        cd $dir
        AUTOPOINT=true ACLOCAL=aclocal-1.11 AUTOMAKE=automake-1.11 autoreconf-2.64 -fi
        cd ..
    done
    rm Makefile.in # autogen
    ACLOCAL=aclocal-1.11 autoreconf-2.64 -fi

    # Rebuild dependencies in libiberty/Makefile.in
    cd libiberty
    CC=tcc ./configure --enable-maintainer-mode
    make maint-deps
    make clean
    cd ..

    # Regenerate MeP sections
    ./bfd/mep-relocs.pl
}

src_configure() {
    for dir in intl libiberty opcodes bfd binutils gas gprof ld zlib; do
        cd $dir

        # BUILDFIXED=1 is specifically for zlib, to avoid needing to
        # regenerate inffixed.h. Instead, it generates the information from
        # it at runtime.
        # Similarly, DYANMIC_CRC_TABLE=1 for crc32.h.
        LD="true" AR="tcc -ar" CC="tcc" \
            CFLAGS="-DBUILDFIXED=1 -DDYNAMIC_CRC_TABLE=1" \
            ./configure \
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
        cd ..
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
    cd "${DESTDIR}${PREFIX}/bin"
    for f in *; do
        ln -s "${PREFIX}/bin/${f}" "i386-unknown-linux-musl-${f}"
    done
}
