# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>

# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    default
    tar xzf ${SOURCES}/automake-1.16.3/src/automake-1.16.3.tar.gz
}

src_prepare() {
    default
    # This is needed for building with TCC
    sed -i 's/ix86_attribute_table\[\]/ix86_attribute_table\[10\]/' gcc/config/i386/i386.c
    # Needed for musl
    sed -i 's/struct siginfo/siginfo_t/' gcc/config/i386/linux-unwind.h

    # Regenerating top level Makefile requires GNU Autogen and hence Guile,
    # but it is not essential for building gcc.
    rm configure Makefile.in fixincludes/fixincl.x

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
    AUTOMAKE=automake-1.10 ACLOCAL=aclocal-1.10 AUTOM4TE=autom4te-2.61 autoreconf-2.61 -f
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
    cp ../automake-1.16.3/lib/config.sub .

    # Rebuild bison files
    # Workaround for bison being too new
    sed -i 's/YYLEX/yylex()/' gcc/c-parse.y
    rm gcc/c-parse.c
    rm gcc/gengtype-yacc.c gcc/gengtype-yacc.h
    rm intl/plural.c

    # Rebuild flex generated files
    rm gcc/gengtype-lex.c

    # Remove translation catalogs
    find . -name '*.gmo' -delete

    # Pre-build texinfo files
    find . -name '*.info' -delete
}

src_configure() {
    mkdir build
    cd build

    for dir in libiberty libcpp gcc; do
        mkdir $dir
        cd $dir
        CC=tcc CFLAGS="-D HAVE_ALLOCA_H" ../../$dir/configure \
            --prefix="${PREFIX}" \
            --libdir="${PREFIX}"/lib/musl \
            --build=i386-unknown-linux-musl \
            --target=i386-unknown-linux-musl \
            --host=i386-unknown-linux-musl \
            --disable-shared \
            --program-transform-name=
        cd ..
    done
    cd ..

    sed -i 's/C_alloca/alloca/g' libiberty/alloca.c
    sed -i 's/C_alloca/alloca/g' include/libiberty.h
}

src_compile() {
    ln -s . build/build-i386-unknown-linux-musl
    mkdir build/gcc/include
    ln -s ../../../gcc/gsyslimits.h build/gcc/include/syslimits.h
    for dir in libiberty libcpp gcc; do
        make -C build/$dir LIBGCC2_INCLUDES=-I"${PREFIX}/include" STMP_FIXINC=
    done
}

src_install() {
    mkdir -p "${PREFIX}/lib/musl/gcc/i386-unknown-linux-musl/4.0.4/install-tools/include"
    make -C build/gcc install STMP_FIXINC= DESTDIR="${DESTDIR}"
}
