# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Bastian Bittorf <bb@npl.de>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # Remove bison generated files
    rm y.tab.c y.tab.h

    # Remove prebuilt translation catalogs
    rm po/*.gmo

    # Skip documentation
    mv doc/Makefile.in Makefile.in.doc
    rm doc/*
    mv Makefile.in.doc doc/Makefile.in

    # Erroneously included file (configure checks do not work)
    rm lib/sh/strtoimax.c
    touch lib/sh/strtoimax.c

    # Rebuild configure script
    rm configure
    autoconf-2.69

    # avoid non-deterministic build:
    printf '%s\n%s\n' \
	   '#!/bin/sh' \
	   'echo "#define PIPESIZE 65536"' >builtins/psize.sh
}

src_configure() {
    # --build argument needed for reproducibility
    # bash_cv_dev_stdin and bash_cv_dev_fd are also used to
    # improve reproducibility because they make configure
    # skip checking for /dev/{fd,stdin,stdout,stderr} (build
    # output is affected by their availability otherwise).
    # size is part of binutils and is not yet available.
    ./configure --prefix="${PREFIX}" \
        --without-bash-malloc \
        --disable-nls \
        --build=i386-unknown-linux-musl \
        --enable-static-link \
        bash_cv_dev_stdin=absent \
        bash_cv_dev_fd=whacky
}

src_compile() {
    make -j1 PREFIX="${PREFIX}"
}

src_install() {
    install -D bash "${DESTDIR}${PREFIX}/bin/bash"
    install bash "${DESTDIR}${PREFIX}/bin/sh"
}
