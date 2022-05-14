# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Michael Schierl <schierlm@gmx.de>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

EXTRA_DISTFILES="gnulib-901694b9.tar.gz guile-psyntax-bootstrapping.tar.gz"

src_prepare() {
    default

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
        --build=i386-unknown-linux-musl \
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
}
