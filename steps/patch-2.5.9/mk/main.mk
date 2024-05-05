# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc

CFLAGS  = -I .
CPPFLAGS = -DHAVE_DECL_GETENV -DHAVE_DECL_MALLOC -DHAVE_DIRENT_H -DHAVE_LIMITS_H -DHAVE_GETEUID -DHAVE_MKTEMP -DPACKAGE_BUGREPORT= -Ded_PROGRAM=\"/nullop\" -Dmbstate_t=void\* -DRETSIGTYPE=int -DHAVE_MKDIR -DHAVE_RMDIR -DHAVE_FCNTL_H -DPACKAGE_NAME=\"patch\" -DPACKAGE_VERSION=\"2.5.9\" -DHAVE_MALLOC -DHAVE_REALLOC -DSTDC_HEADERS -DHAVE_STRING_H -DHAVE_STDLIB_H
LDFLAGS = -static

.PHONY: all
all: patch

patch: error.o getopt.o getopt1.o addext.o argmatch.o backupfile.o basename.o dirname.o inp.o maketime.o partime.o patch.o pch.o quote.o quotearg.o quotesys.o util.o version.o xmalloc.o
	$(CC) $^ $(LDFLAGS) -o $@
