# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_compile() {
    :
}

src_install() {
    install -D lib/Devel/Tokenizer/C.pm /usr/lib/perl5/5.6.2/Devel/Tokenizer/C.pm
}
