# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>

# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    default_src_unpack
}

src_prepare() {
    default_src_prepare
    # This is needed for building with TCC
    sed -i 's/ix86_attribute_table\[\]/ix86_attribute_table\[10\]/' gcc/config/i386/i386.c
    # Needed for musl
    sed -i 's/struct siginfo/siginfo_t/' gcc/config/i386/linux-unwind.h

    rm configure
    autoconf-2.13
    for dir in intl libcpp; do
        cd $dir
        rm aclocal.m4
        aclocal-1.9 --acdir=../config
        cd ..
    done
    for dir in fixincludes gcc intl libcpp libiberty; do
        cd $dir
        rm configure
        autoconf-2.61
        cd ..
    done
    cd libmudflap
    autoreconf-2.61 -f
    cd ..

    for dir in fixincludes intl libmudflap; do
        cd $dir
        rm -f config.in
        autoheader-2.61
        cd ..
    done

    # Rebuild libtool files
    rm config.guess config.sub ltmain.sh
    libtoolize

    # Rebuild bison files
    # Workaround for bison being too new
    sed -i 's/YYLEX/yylex()/' gcc/c-parse.y
    rm gcc/c-parse.c
    rm gcc/gengtype-yacc.c gcc/gengtype-yacc.h
    rm intl/plural.c

    # Rebuild flex generated files
    rm gcc/gengtype-lex.c

    # Remove translation catalogs
    rm gcc/po/*.gmo
}

src_configure() {
    mkdir build
    cd build
    CC=tcc CFLAGS="-D HAVE_ALLOCA_H" ../configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}"/lib/musl \
        --build=i386-unknown-linux-gnu \
        --host=i386-unknown-linux-gnu \
        --disable-shared \
        --disable-nls \
        --disable-libmudflap
    cd ..

    sed -i 's/C_alloca/alloca/g' libiberty/alloca.c
    sed -i 's/C_alloca/alloca/g' include/libiberty.h
}

src_compile() {
    mkdir -p /usr/
    ln -sf "${PREFIX}"/include /usr/include
    make -C build LIBGCC2_INCLUDES=-I"${PREFIX}/include/musl"
}

src_install() {
    make -C build install
}
