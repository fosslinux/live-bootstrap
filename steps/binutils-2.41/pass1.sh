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

    # Remove unused parts
    rm -r gprofng zlib

    # Rebuild bison files
    touch -- */*.y
    rm binutils/arparse.c binutils/arparse.h binutils/defparse.c \
        binutils/defparse.h binutils/mcparse.c binutils/mcparse.h \
        binutils/rcparse.c binutils/rcparse.h binutils/sysinfo.c \
        binutils/sysinfo.h gas/config/bfin-parse.c gas/config/bfin-parse.h \
        gas/config/loongarch-parse.c gas/config/loongarch-parse.h \
        gas/config/m68k-parse.c gas/config/rl78-parse.c \
        gas/config/rl78-parse.h gas/config/rx-parse.c gas/config/rx-parse.h \
        gas/itbl-parse.c gas/itbl-parse.h gold/yyscript.c gold/yyscript.h \
        intl/plural.c ld/deffilep.c ld/deffilep.h ld/ldgram.c ld/ldgram.h

    # Rebuild flex generated files
    touch -- */*.l */*/*.l
    rm binutils/arlex.c binutils/deflex.c binutils/syslex.c \
        gas/config/bfin-lex.c gas/config/loongarch-lex.c gas/itbl-lex.c \
        ld/ldlex.c

    # Remove prebuilt docs
    find . -type f -name '*.info*' \
                   -not -wholename './binutils/sysroff.info' \
                   -delete
    find . -type f \( -name '*.1' -or -name '*.man' \) -delete
    rm libiberty/functions.texi

    # Remove gettext translation files
    find . -type f -name '*.gmo' -delete

    # Remove pregenerated opcodes files
    rm opcodes/i386-init.h opcodes/i386-tbl.h opcodes/i386-mnem.h \
        opcodes/ia64-asmtab.c opcodes/z8k-opc.h opcodes/aarch64-asm-2.c \
        opcodes/aarch64-opc-2.c opcodes/aarch64-dis-2.c \
        opcodes/msp430-decode.c opcodes/rl78-decode.c opcodes/rx-decode.c
    rm $(grep -l 'MACHINE GENERATED' opcodes/*.c opcodes/*.h)

    # Various other blobs/generated headers
    rm ld/emultempl/*.o_c
    rm gprof/bsd_callg_bl.c gprof/flat_bl.c gprof/fsf_callg_bl.c
    rm bfd/libcoff.h bfd/libbfd.h bfd/go32stub.h bfd/bfd-in2.h

    # Generated testsuite stuff (xz-style attack)
    rm libsframe/testsuite/libsframe.decode/DATA* \
        ld/testsuite/ld-x86-64/*.obj.bz2 ld/testsuite/ld-sh/arch/*.s \
        ld/testsuite/ld-sh/arch/arch_expected.txt \
        ld/testsuite/ld-i386/pr27193a.o.bz2 \
        gas/testsuite/gas/xstormy16/allinsn.sh \
        gas/testsuite/gas/tic4x/opcodes.s gas/testsuite/gas/sh/arch/*.s \
        gas/testsuite/gas/sh/arch/arch_expected.txt \
        binutils/testsuite/binutils-all/x86-64/pr22451.o.bz2 \
        binutils/testsuite/binutils-all/x86-64/pr26808.dwp.bz2 \
        binutils/testsuite/binutils-all/x86-64/pr27708.exe.bz2 \
        binutils/testsuite/binutils-all/nfp/*.nffw \
        binutils/testsuite/binutils-all/pr26112.o.bz2 \
        binutils/testsuite/binutils-all/pr26160.dwp.bz2

    # Regenerate crc table in libiberty/crc32.c
    cd libiberty
    sed -n '/^   #include <stdio.h>/,/^   \}$/p' crc32.c > crcgen.c
    gcc -o crcgen crcgen.c
    sed '/crc_v3\.txt/{n; q}' crc32.c > crc32.c.new
    ./crcgen >> crc32.c.new
    sed '1,/^};$/d' crc32.c >> crc32.c.new
    mv crc32.c.new crc32.c
    cd ..

    # bfd-in2.h is required to run autoreconf, but we don't have it yet
    cd bfd
    cp configure.ac configure.ac.bak
    sed -i "s/bfd-in3.h:bfd-in2.h //" configure.ac
    AUTOPOINT=true ACLOCAL=aclocal-1.15 AUTOMAKE=automake-1.15 autoreconf-2.69 -fi
    ./configure
    make headers
    mv configure.ac.bak configure.ac
    make distclean
    cd ..

    # Regenerate autoconf
    for dir in bfd binutils gas gold gprof intl ld libctf libiberty libsframe opcodes; do
        cd $dir
        AUTOPOINT=true ACLOCAL=aclocal-1.15 AUTOMAKE=automake-1.15 autoreconf-2.69 -fi
        cd ..
    done
    rm Makefile.in # autogen
    ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi

    # Rebuild dependencies in libiberty/Makefile.in
    cd libiberty
    ./configure --enable-maintainer-mode
    make maint-deps
    make distclean
    cd ..

    # Regenerate MeP sections
    ./bfd/mep-relocs.pl
}

src_configure() {
    for dir in intl libctf libiberty libsframe opcodes bfd binutils gas gprof ld; do
        cd $dir

        ./configure \
            --disable-nls \
            --enable-install-libiberty \
            --enable-deterministic-archives \
            --with-system-zlib \
            --build=i386-unknown-linux-musl \
            --host=i386-unknown-linux-musl \
            --target=i386-unknown-linux-musl \
            --program-prefix="" \
            --prefix="${PREFIX}" \
            --libdir="${LIBDIR}" \
            --with-sysroot= \
            --srcdir=.
        cd ..
    done
}

src_compile() {
    make -C bfd headers
    for dir in libiberty libsframe bfd opcodes libctf binutils gas gprof ld; do
        make "${MAKEJOBS}" -C $dir tooldir=${PREFIX} CFLAGS="-std=c99"
    done
}

src_install() {
    for dir in libiberty bfd opcodes libctf libsframe binutils gas gprof ld; do
        make -C $dir tooldir=${PREFIX} DESTDIR="${DESTDIR}" install
    done

    # Create triplet symlinks
    pushd "${DESTDIR}${PREFIX}/bin"
    for f in *; do
        ln -s "${PREFIX}/bin/${f}" "i386-unknown-linux-musl-${f}"
    done
    popd

    # FIXME: Binutils' manpages dates are not reproducible
    rm -r "${DESTDIR}${PREFIX}/share/man"
}
