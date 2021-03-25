# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>

# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    default_src_unpack
}

src_prepare() {
    default_src_prepare

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
    AUTOM4TE=autom4te-2.61 autoreconf-2.61 -f
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
    cp "${PREFIX}/"/share/automake-1.9/config.sub .

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
    ../configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}"/lib/musl \
        --build=i386-unknown-linux-gnu \
        --host=i386-unknown-linux-gnu \
        --disable-shared \
        --disable-nls \
        --disable-libmudflap
    cd ..
}

src_compile() {
    mkdir -p /usr/
    ln -sf "${PREFIX}"/include /usr/include
    make -C build LIBGCC2_INCLUDES=-I"${PREFIX}/include/musl"
}

src_install() {
    make -C build install
}
