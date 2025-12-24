# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm zlib.3.pdf \
        doc/crc-doc.1.0.pdf \
        contrib/puff/zeros.raw \
        contrib/blast/test.pk \
        contrib/dotzlib/DotZLib.chm

    rm crc32.h
    gcc -DMAKECRCH crc32.c -o gen_crc32h
    ./gen_crc32h

    echo "void makefixed(void); int main() { makefixed(); }" > makefixed_main.c
    gcc -DMAKEFIXED inflate.c crc32.c zutil.c inftrees.c \
        adler32.c inffast.c makefixed_main.c -o gen_inffixedh
    ./gen_inffixedh > inffixed.h

    echo "void makefixed9(void); int main() { makefixed9(); }" > makefixed9_main.c
    gcc -DMAKEFIXED -I. contrib/infback9/infback9.c zutil.c \
        contrib/infback9/inftree9.c makefixed9_main.c -o gen_inffix9h
    ./gen_inffix9h > contrib/infback9/inffix9.h
}

src_configure() {
    ./configure --prefix="${PREFIX}" --libdir="${LIBDIR}" --static
}
