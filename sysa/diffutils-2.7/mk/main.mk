# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc

CFLAGS  = -I . \
          -DNULL_DEVICE=\"/dev/null\" \
          -DHAVE_STRERROR=1 \
          -DREGEX_MALLOC=1 \
          -DHAVE_DIRENT_H \
          -DHAVE_DUP2=1 \
          -DHAVE_FORK=1

.PHONY: all

CMP_SRC = cmp cmpbuf error getopt getopt1 xmalloc version
CMP_OBJECTS = $(addsuffix .o, $(CMP_SRC))

DIFF_SRC = diff alloca analyze cmpbuf dir io util context ed ifdef normal side fnmatch getopt getopt1 regex version
DIFF_OBJECTS = $(addsuffix .o, $(DIFF_SRC))

all: cmp diff

cmp: $(CMP_OBJECTS)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

diff: $(DIFF_OBJECTS)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: all
	install -D cmp $(DESTDIR)$(PREFIX)/bin/cmp
	install diff $(DESTDIR)$(PREFIX)/bin
