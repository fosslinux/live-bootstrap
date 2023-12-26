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
    rm zlib/aclocal.m4 zlib/configure

    # Remove gprofng
    rm -r gprofng

    # Regenerate autoconf
    for dir in bfd binutils gas gold gprof intl ld libctf libiberty libsframe opcodes; do
        cd $dir
        AUTOPOINT=true ACLOCAL=aclocal-1.15 AUTOMAKE=automake-1.15 autoreconf-2.69 -fi
        cd ..
    done
    ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi

    # Regenerate directories with Makefile.am only
    pushd gold
    automake-1.15 -fai testsuite/Makefile
    popd

    # Rebuild bison files
    touch -- */*.y
    rm binutils/arparse.c binutils/arparse.h
    rm binutils/defparse.c binutils/defparse.h
    rm binutils/mcparse.c binutils/mcparse.h
    rm binutils/rcparse.c binutils/rcparse.h
    rm binutils/sysinfo.c binutils/sysinfo.h
    rm gas/config/bfin-parse.c gas/config/bfin-parse.h
    rm gas/config/loongarch-parse.c gas/config/loongarch-parse.h
    rm gas/config/m68k-parse.c gas/config/rl78-parse.c
    rm gas/config/rl78-parse.h gas/config/rx-parse.c
    rm gas/config/rx-parse.h gas/itbl-parse.c
    rm gas/itbl-parse.h gold/yyscript.c
    rm gold/yyscript.h intl/plural.c
    rm ld/deffilep.c ld/deffilep.h
    rm ld/ldgram.c ld/ldgram.h

    # Rebuild flex generated files
    touch -- */*.l */*/*.l
    rm binutils/arlex.c binutils/deflex.c binutils/syslex.c
    rm gas/config/bfin-lex.c gas/config/loongarch-lex.c gas/itbl-lex.c
    rm ld/ldlex.c

    # Remove prebuilt texinfo files
    find . -type f -name '*.info*' \
                   -not -wholename './binutils/sysroff.info' \
                   -delete

    # Remove pregenerated opcodes files
    rm opcodes/i386-init.h opcodes/i386-tbl.h opcodes/i386-mnem.h
    rm opcodes/ia64-asmtab.c
    rm opcodes/z8k-opc.h
    rm opcodes/aarch64-asm-2.c opcodes/aarch64-opc-2.c opcodes/aarch64-dis-2.c
    rm $(grep -l 'MACHINE GENERATED' opcodes/*.c opcodes/*.h)

    rm libiberty/functions.texi

    # Regenerate MeP sections
    ./bfd/mep-relocs.pl

    # Manpages
    find . -type f \( -name '*.1' -or -name '*.man' \) -delete
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
