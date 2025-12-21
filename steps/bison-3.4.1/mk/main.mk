# SPDX-FileCopyrightText: 2020 Giovanni Mascellani gio@debian.org
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC=tcc
AR="tcc -ar"

all: bison

bison: src.a lib.a
	$(CC) $(CFLAGS) -g -o $@ $^

%.a: FORCE
	set -e ;\
	DIR=$(basename $@ .a) ;\
	$(MAKE) CC=$(CC) AR=$(AR) CFLAGS=$(CFLAGS) -C $$DIR $@ ;\
	cp $$DIR/$@ $@

FORCE:

install:
	install -D bison-3.4 $(DESTDIR)$(PREFIX)/bin/bison-3.4
	install -d $(DESTDIR)$(PREFIX)/share/bison-3.4
	mv data/skeletons/ $(DESTDIR)$(PREFIX)/share/bison-3.4
	mv data/m4sugar/ $(DESTDIR)$(PREFIX)/share/bison-3.4
