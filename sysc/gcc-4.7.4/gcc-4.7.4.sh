# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>

# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Generated using gperf
    rm gcc/cp/cfns.h

    # Regenerating top level Makefile requires GNU Autogen and hence Guile,
    # but it is not essential for building gcc.
    rm configure Makefile.in fixincludes/fixincl.x

    # Remove unused generated files
    rm libgo/aclocal.m4 libgo/configure libgo/Makefile.in

    # Regenerate aclocal.m4 files
    # grep "generated automatically by aclocal" */aclocal.m4  -l | sed -e 's#/aclocal.m4##'  | tr "\n" " " | sed -e 's/ $/\n/'
    for dir in intl libcpp libdecnumber; do
        cd $dir
        rm aclocal.m4
        AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 aclocal-1.11 --acdir=../config
        cd ..
    done
    cd gcc
    rm aclocal.m4
    AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 aclocal-1.11 --acdir=../config
    cd ..
    cd fixincludes
    rm aclocal.m4
    AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 aclocal-1.11 --acdir=../gcc
    cd ..
    for dir in boehm-gc libffi libgfortran libgomp libitm libjava libmudflap libobjc libquadmath libssp lto-plugin zlib; do
        cd $dir
        rm aclocal.m4
        AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 aclocal-1.11
        cd ..
    done

    cd libstdc++-v3
    ACLOCAL=aclocal-1.11 AUTOMAKE=automake-1.11 AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 autoreconf-2.64 -fi
    cd ..

    # Regenerate configure scripts
    # Find all folders with configure script and rebuild them. At the moment we exclude boehm-gc folder due to
    # an error but we don't use that directory anyway (it's only needed for Objective C)
    for dir in $(find . -mindepth 2 -maxdepth 2 -name configure.ac | sed 's#/configure.ac##' | grep -v -x './libgo' | tr "\n" " " | sed -e 's/ $/\n/' -e 's/^boehm-gc //'); do
        pushd "$dir"
        rm configure
        autoconf-2.64 || autoconf-2.64
        popd
    done

    # Regenerate Makefile.in
    # Find all folders with Makefile.am and rebuild them. At the moment we exclude boehm-gc folder.
    for dir in $(find . -mindepth 2 -maxdepth 2 -name Makefile.am | sed 's#/Makefile.am##' | grep -v -x './libgo' | tr "\n" " " | sed -e 's/ $/\n/' -e 's/^boehm-gc //'); do
        pushd "$dir"
        rm Makefile.in
        AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 automake-1.11 --add-missing
        popd
    done

    for dir in libdecnumber libcpp libiberty gcc; do
        cd $dir
        rm -f config.in
        autoheader-2.64
        cd ..
    done

    # Rebuild libtool files
    rm config.guess config.sub ltmain.sh
    libtoolize
    cp "${PREFIX}/share/automake-1.15/config.sub" .

    # Workaround for bison being too new
    rm intl/plural.c

    # Rebuild flex generated files
    rm gcc/gengtype-lex.c

    # Regenerate crc table in libiberty/crc32.c
    pushd libiberty
    sed -n -e '39,66p' crc32.c > crcgen.c
    gcc -o crcgen crcgen.c
    head -n 70 crc32.c > crc32.c.new
    ./crcgen >> crc32.c.new
    tail -n +139 crc32.c >> crc32.c.new
    mv crc32.c.new crc32.c
    popd

    # Remove translation catalogs
    find . -name '*.gmo' -delete

    # Pre-built texinfo files
    find . -name '*.info' -delete
}

src_configure() {
    mkdir build
    cd build

    for dir in libiberty libcpp libdecnumber gcc libgcc libstdc++-v3; do
        mkdir $dir
        cd $dir
        ../../$dir/configure \
            --prefix="${PREFIX}" \
            --libdir="${LIBDIR}" \
            --build=i386-unknown-linux-musl \
            --target=i386-unknown-linux-musl \
            --host=i386-unknown-linux-musl \
            --disable-shared \
            --program-transform-name= \
            --enable-languages=c,c++ \
            --disable-sjlj-exceptions \
            --with-system-zlib
        cd ..
    done
    cd ..
}

src_compile() {
    ln -s . build/build-i386-unknown-linux-musl
    for dir in libiberty libcpp libdecnumber gcc; do
        # We have makeinfo now but it is not happy with gcc .info files, so skip it
        make "${MAKEJOBS}" -C build/$dir LIBGCC2_INCLUDES=-I"${PREFIX}/include" \
            STMP_FIXINC= GMPLIBS="-lmpc -lmpfr -lgmp" MAKEINFO=true
    done

    # host_subdir is necessary because we have slightly different build directory layout
    make "${MAKEJOBS}" -C build/libgcc PATH="${PATH}:../gcc" CC=../gcc/xgcc \
        host_subdir=build CFLAGS="-I../gcc/include -I/${PREFIX}/include"

    make "${MAKEJOBS}" -C build/libstdc++-v3 PATH="${PATH}:${PWD}/build/gcc" \
        CXXFLAGS="-I${PWD}/build/gcc/include -I ${PREFIX}/include"
}

src_install() {
    make -C build/gcc install STMP_FIXINC= DESTDIR="${DESTDIR}" MAKEINFO=true
    make -C build/libgcc install DESTDIR="${DESTDIR}" host_subdir=build
    make -C build/libstdc++-v3 install DESTDIR="${DESTDIR}"
    ln -s gcc "${DESTDIR}${PREFIX}/bin/cc"
    cp gcc/gsyslimits.h "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.7.4/include/syslimits.h"

    # Very strange mis-versoning error
    mkdir -p "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.7.4/include/"
    mv "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.0.4/include/"* "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.7.4/include/"
    rmdir "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.0.4/include"
    mv "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.0.4/"* "${DESTDIR}${LIBDIR}/gcc/i386-unknown-linux-musl/4.7.4/"
}
