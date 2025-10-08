# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_compile() {
    :
}

src_install() {
    install -D lib/Devel/Tokenizer/C.pm "${DESTDIR}/usr/lib/perl5/$(get_perl_version)/Devel/Tokenizer/C.pm"
}
