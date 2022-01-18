# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=4f047b1c7b5b04b8617a18ca990ff2002fa780377391bd20e39b7ec11df66b0c

src_prepare() {
    autoreconf-2.59 -f

    rm help2man.info
    touch help2man.info
}

src_configure() {
    CC=tcc ./configure --prefix="${PREFIX}" --disable-nls
}

src_compile() {
    make MAKEINFO=true

    # fix a broken shebang
    chmod +w help2man
    tail -n +6 help2man > help2man.tmp
    echo "#!${PREFIX}/bin/perl" > help2man
    cat help2man.tmp >> help2man
    rm help2man.tmp
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
