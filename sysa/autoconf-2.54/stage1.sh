# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm bin/autoconf.in
    rm Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 configure
    rm doc/standards.info doc/autoconf.info
    aclocal-1.7
    sed -i 's/2.54/2.53/' aclocal.m4
    cat config/m4.m4 >> aclocal.m4
    autoconf-2.53

    # Otherwise automake-1.7 fails to find autoconf
    ln -s "${PREFIX}"/bin/autoconf-2.53 "${PREFIX}"/bin/autoconf
    automake-1.7

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.54
}

src_compile() {
    # Workaround for racy make dependencies
    make -C bin autom4te
    make -C lib
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"

    ln -sf "${PREFIX}"/bin/autoconf-2.54 "${DESTDIR}${PREFIX}"/bin/autoconf
}
