# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

PACKAGE=grep
VERSION=2.4

CC      = tcc
LD      = tcc
AR      = tcc -ar

CFLAGS  = -DPACKAGE=\"$(PACKAGE)\" \
          -DVERSION=\"$(VERSION)\" \
          -DHAVE_DIRENT_H=1 \
          -DHAVE_UNISTD_H=1 \
          -DHAVE_STRERROR=1 \
          -DREGEX_MALLOC=1

.PHONY: all

GREP_SRC = grep dfa kwset obstack regex stpcpy savedir getopt getopt1 search grepmat
GREP_OBJECTS = $(addprefix src/, $(addsuffix .o, $(GREP_SRC)))

all: grep

grep: $(GREP_OBJECTS)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: all
	install -D grep $(DESTDIR)$(PREFIX)/bin/grep
	ln -sf $(PREFIX)/bin/grep $(DESTDIR)$(PREFIX)/bin/egrep
	ln -sf $(PREFIX)/bin/grep $(DESTDIR)$(PREFIX)/bin/fgrep
