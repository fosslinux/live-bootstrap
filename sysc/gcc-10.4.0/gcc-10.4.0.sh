# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove vendored zlib
    rm -r zlib/

    # Regen gperf file (because GCC's make rules suck)
    rm gcc/cp/cfns.h
    # (taken directly from gcc/cp/Make-lang.in)
    gperf -o -C -E -k '1-6,$' -j1 -D -N 'libc_name_p' -L C++ \
        gcc/cp/cfns.gperf --output-file gcc/cp/cfns.h

    # Regenerate autogen stuff
    autogen Makefile.def
    pushd fixincludes
    ./genfixes
    popd
    
    # Regenerate autotools
    # configure
    find . -name configure | sed 's:/configure::' | while read d; do
        pushd "${d}"
        AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fiv
        popd
    done
    # Because GCC is stupid, copy depcomp back in
    cp "${PREFIX}/share/automake-1.15/depcomp" .
    # Makefile.in only
    BACK="${PWD}"
    find . -type d \
        -exec test -e "{}/Makefile.am" -a ! -e "{}/configure" \; \
        -print | while read d; do
        d="$(readlink -f "${d}")"
        cd "${d}"
        # Find the appropriate configure script for automake
        while [ ! -e configure ]; do
            cd ..
        done
        automake-1.15 -fai "${d}/Makefile"
        cd "${BACK}"
    done

    # Remove bison generated files
    rm intl/plural.c

    # Remove flex generated files
    rm gcc/gengtype-lex.c

    # Remove unused generated files
    rm -r libgfortran/generated

    # intl/ Makefile is a bit broken because of new gettext
    sed -i 's/@USE_INCLUDED_LIBINTL@/no/' intl/Makefile.in

    # Regenerate crc table in libiberty/crc32.c
    pushd libiberty
    sed -n -e '38,65p' crc32.c > crcgen.c
    gcc -o crcgen crcgen.c
    head -n 69 crc32.c > crc32.c.new
    ./crcgen >> crc32.c.new
    tail -n +138 crc32.c >> crc32.c.new
    mv crc32.c.new crc32.c
    popd
    
    # Remove docs/translation
    find . -name "*.gmo" -delete
    find . -name "*.info" -delete
}

src_configure() {
    mkdir build
    cd build

    # std=gnu11 is the default for GCC10, so that is what it makes most
    # sense to build with. (default, std=gnu90 is too outdated).
    # For this GCC, we only build one stage, as extra is superfluous,
    # since we build GCC 12 straight after.
    CFLAGS="-std=gnu11" \
    LDFLAGS="-static" \
    ../configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --build=i386-unknown-linux-musl \
        --target=i386-unknown-linux-musl \
        --host=i386-unknown-linux-musl \
        --disable-bootstrap \
        --enable-static \
        --program-transform-name= \
        --enable-languages=c,c++ \
        --with-system-zlib \
        --disable-sjlj-exceptions \
        --disable-multilib \
        --enable-threads=posix \
        --disable-libsanitizer \
        --disable-libssp
}
