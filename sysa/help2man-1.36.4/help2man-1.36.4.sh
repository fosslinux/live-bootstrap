# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    autoreconf-2.59 -f
}

src_configure() {
    CC=tcc ./configure --prefix="${PREFIX}" --disable-nls
}

src_compile() {
    make

    # fix a broken shebang
    tail -n +6 help2man > help2man.tmp
    echo "#!/${PREFIX}/bin/perl" > help2man
    cat help2man.tmp >> help2man
    rm help2man.tmp
}
