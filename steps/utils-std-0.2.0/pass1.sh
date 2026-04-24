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
	chmod +x configure
	./configure PREFIX="${PREFIX}" CC=tcc AR=tcc\ -ar

	# Fiwix (as of 1.7.0) doesn't have sendfile(2), copy_file_range(2), syncfs(2), posix_fadvise(2)
	sed -i \
		-e /HAS_SENDFILE/d \
		-e /HAS_COPY_FILE_RANGE/d \
		-e /HAS_SYNCFS/d \
		-e /HAS_POSIX_FADVISE/d \
		config.h
}
