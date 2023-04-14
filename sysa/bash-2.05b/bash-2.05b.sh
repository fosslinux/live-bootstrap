# SPDX-FileCopyrightText: © 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove bison generated files
    rm y.tab.c y.tab.h

    # Skip documentation
    mv doc/Makefile.in Makefile.in.doc
    rm doc/*
    mv Makefile.in.doc doc/Makefile.in

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
    CC=tcc LD=tcc AR="tcc -ar" SIZE=true ./configure --prefix="${PREFIX}" \
        --without-bash-malloc \
        --disable-nls \
        --build=i386-linux-gnu \
        --enable-static-link \
        bash_cv_dev_stdin=absent \
        bash_cv_dev_fd=whacky
}

src_compile() {
    make -j1
}

src_install() {
    install -D bash "${DESTDIR}${PREFIX}/bin/bash"
    # Work around weird symlink bug
    install bash "${DESTDIR}${PREFIX}/bin/sh"

    # Needs special handling b/c is currently running - tar doesn't like this
    rm -f "${PREFIX}/bin/bash" "${PREFIX}/bin/sh"
}
