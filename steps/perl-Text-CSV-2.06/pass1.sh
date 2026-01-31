# SPDX-FileCopyrightText: 2026 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_configure() {
    perl Makefile.PL
}

src_install() {
    make DESTDIR="$DESTDIR" PREFIX="$PREFIX" pure_install
}
