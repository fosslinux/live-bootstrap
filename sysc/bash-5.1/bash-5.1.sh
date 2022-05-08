# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Bastian Bittorf <bb@npl.de>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # Remove bison generated files
    rm y.tab.c y.tab.h

    # Rebuild configure script
    rm configure
    autoconf-2.64

    # avoid non-deterministic build:
    printf '%s\n%s\n' \
	   '#!/bin/sh' \
	   'echo "#define PIPESIZE 65536"' >builtins/psize.sh
}

src_configure() {
    # --build argument needed for reproducibility
    ./configure --prefix="${PREFIX}" \
        --without-bash-malloc \
        --disable-nls \
        --build=i386-unknown-linux-musl \
        --enable-static-link
}

src_install() {
    # Do not install prebuilt .mo translation catalogs
    install -D bash "${DESTDIR}${PREFIX}/bin/bash"
    # Work around weird symlink bug
    install bash "${DESTDIR}${PREFIX}/bin/sh"

    # Needs special handling b/c is currently running - tar doesn't like this
    rm -f "${PREFIX}/bin/bash" "${PREFIX}/bin/sh"
}
