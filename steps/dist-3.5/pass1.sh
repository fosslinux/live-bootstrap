# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# We manually compile here because ./Configure uses metaconfig itself
# *sigh*

src_prepare() {
    default

    sed 's/@PERLVER@/5.6.2/' config.sh.in > config.sh
    find . -name Makefile.SH -delete
    rm -f Configure
}

src_compile() {
    cd mcon
    ./mconfig.SH
    perl ../bin/perload -o mconfig > metaconfig
    ./makegloss.SH
    cd ..

    cd kit
    ./manifake.SH
    cd ..
}

src_install() {
    mkdir -p "${DESTDIR}${PREFIX}/bin/" "${DESTDIR}${PREFIX}/lib/perl5/5.6.2"
    install mcon/metaconfig "${DESTDIR}${PREFIX}/bin/"
    install mcon/makegloss "${DESTDIR}${PREFIX}/bin/"
    install kit/manifake "${DESTDIR}${PREFIX}/bin/"
    cp -r mcon/U/ "${DESTDIR}${PREFIX}/lib/perl5/5.6.2/"
}
