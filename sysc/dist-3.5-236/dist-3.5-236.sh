# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=ad8e113ecc22a49b1c40e670f0620da86bde6199ec56366c3d46368cf46c184c

# We manually compile here because ./Configure uses metaconfig itself
# *sigh*

src_prepare() {
    default

    sed 's/@PERLVER@/5.10.1/' config.sh.in > config.sh
}

src_compile() {
    cd mcon
    ./mconfig.SH
    perl ../bin/perload -o mconfig > metaconfig
    cd ..

    cd kit
    ./manifake.SH
    cd ..
}

src_install() {
    install mcon/metaconfig "${PREFIX}/bin/"
    install kit/manifake "${PREFIX}/bin/"
    cp -r mcon/U/ "${PREFIX}/lib/perl5/5.10.1/"
}
