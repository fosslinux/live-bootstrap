# SPDX-FileCopyrightText: 2025 Haelwenn (lanodan) Monnier <contact@hacktivis.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
	default

	sed -i s/_Noreturn// libutils/err.h
	sed -i '/^commands="$/,/^"$/'s,getconf,, configure
}

src_configure() {
	./configure PREFIX="${PREFIX}" CC=tcc AR=tcc\ -ar
}
