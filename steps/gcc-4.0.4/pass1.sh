# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>

# SPDX-License-Identifier: GPL-3.0-or-later

EXTRA_DISTFILES="automake-1.16.3.tar.xz"

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
        AUTOM4TE=autom4te-2.61 aclocal-1.9 --acdir=../config
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
    rm gcc/po/*.gmo libcpp/po/*.gmo

    # Pre-built texinfo files
    rm gcc/doc/*.info

    # Pre-built man files
    rm gcc/doc/*.1 gcc/doc/*.7
}

src_configure() {
    mkdir build
    cd build

    for dir in libiberty libcpp gcc; do
        mkdir $dir
        cd $dir
        CC=tcc CFLAGS="-D HAVE_ALLOCA_H" ../../$dir/configure \
            --prefix="${PREFIX}" \
            --libdir="${LIBDIR}" \
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
        make "${MAKEJOBS}" -C build/$dir LIBGCC2_INCLUDES=-I"${PREFIX}/include" STMP_FIXINC=
    done
}

src_install() {
    mkdir -p "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.0.4/install-tools/include"
    make -C build/gcc install STMP_FIXINC= DESTDIR="${DESTDIR}"
    mkdir -p "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.0.4/include"
    rm "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.0.4/include/syslimits.h"
    cp  gcc/gsyslimits.h "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.0.4/include/syslimits.h"
}
