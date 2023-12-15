# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Michael Schierl <schierlm@gmx.de>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# guile-psyntax-bootstrapping only supports Guile 3.0.7
# We need Guile 3.0.8 or later in order to have reproducible builds when
# parallelism (-jX) is enabled.
#
# To work around this issue, we;
# 1. run guile-psyntax-bootstrapping on Guile 3.0.7
# 2. take psyntax-pp.scm from Guile 3.0.7 and transplant into 3.0.9
# 3. compile guile 3.0.9

common_prepare() {
    find . -name '*.info*' -delete
    rm -r prebuilt/*/ice-9

    # Remove buildstamp
    sed -i "s/\`date -u +'%Y-%m-%d %T'.*\`/1970-01-01 00:00:00/" libguile/Makefile.am

    autoreconf-2.71 -fi
}

src_prepare() {
    # First, prepare Guile 3.0.7
    cd ../guile-3.0.7
    ../../import-gnulib-3.0.7.sh
    common_prepare

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

    # Now, Guile 3.0.9
    cd ../guile-3.0.9
    default
    ../../import-gnulib.sh
    common_prepare
}

src_configure() {
    for d in . ../guile-3.0.7; do
        pushd "${d}"
        PKG_CONFIG_PATH="${LIBDIR}/pkgconfig" ./configure \
            --prefix="${PREFIX}" \
            --libdir="${LIBDIR}" \
            --build=i386-unknown-linux-musl \
            --disable-shared \
            --disable-jit
        popd
    done
}

src_compile() {
    # pp-syntax: rebuild
    pushd ../guile-3.0.7
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
    popd

    # Transplant psyntax-pp.scm
    cp -f ../guile-3.0.7/module/ice-9/psyntax-pp.scm module/ice-9/

    # Now proceed with the build
    default
}
