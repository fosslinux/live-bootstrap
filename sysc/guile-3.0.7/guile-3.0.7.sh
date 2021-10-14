# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Michael Schierl <schierlm@gmx.de>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # If we are in chroot mode, we can make no assumptions about the host
    # kernel. It appears the resulting binary is at least somewhat
    # kernel-specific (in ways other than hardcoded string). Hence disable
    # checksumming for guile binary under chroot.
    if [ "$CHROOT" = True ]; then
        sed -i '\|/usr/bin/guile$|d' ../../checksums
        sed -i '\|/usr/lib/musl/libguile-3.0.a$|d' ../../checksums
    fi

    find . -name '*.info*' -delete

    ../../import-gnulib.sh

    # Remove buildstamp
    sed -i "s/\`date -u +'%Y-%m-%d %T'.*\`/1970-01-01 00:00:00/" libguile/Makefile.am

    autoreconf-2.71 -fi

    # pp-syntax bootstrap: remove pre-gen file
    sha256sum module/ice-9/psyntax-pp.scm | tee psyntax-pp.sha256
    rm module/ice-9/psyntax-pp.scm

    ## now let us prepare to rebuild it
    echo '(primitive-load-path "psyntax-bootstrap/allsteps")' > module/ice-9/psyntax-pp.scm
    mkdir -p module/psyntax-bootstrap
    cp ../guile-psyntax-bootstrapping-guile-3.0.7/psyntax-bootstrap/*.scm module/psyntax-bootstrap
    cd module/ice-9
    cp psyntax.scm psyntax-patched.scm
    patch <../../../guile-psyntax-bootstrapping-guile-3.0.7/stage2.patch
    cd ../..
}

src_configure() {
    PKG_CONFIG_PATH="${PREFIX}/lib/musl/pkgconfig" ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared \
        --disable-jit
}

src_compile() {
    # pp-syntax: rebuild
    make config.h
    make libguile/scmconfig.h
    make .version
    cd lib
    make all
    cd ../meta
    make all
    cd ../libguile
    make all
    cd ../module
    make ice-9/psyntax-pp.scm.gen
    cd ..

    # Note that on 32-bit systems psyntax-pp is not identical to the original
    # and some identifier names are different.

    # Now proceed with the build
    default

    # Ordering of libguile.a is messed up
    mkdir libguile/.libs/order
    pushd libguile/.libs/order
    ar x ../libguile-3.0.a
    rm ../libguile-3.0.a
    ar cr ../libguile-3.0.a *.o
    popd

    # Recompile guile with fixed libguile
    rm libguile/guile
    make
}
