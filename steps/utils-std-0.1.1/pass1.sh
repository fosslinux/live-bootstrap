# SPDX-FileCopyrightText: 2025 Haelwenn (lanodan) Monnier <contact@hacktivis.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
	default

	sed -i s/_Noreturn// libutils/err.h

	# getconf: to avoid changing libtool checksums, although likely means better values
	# which: to avoid changing checksums of packages like perl-5.36.3_0
	sed -i '/^commands="$/,/^"$/{ s,getconf,, ; s,which,, }' configure
}

src_configure() {
	# patch(1) strips out permissions
	chmod +x configure

	./configure PREFIX="${PREFIX}" CC=tcc AR=tcc\ -ar
}
