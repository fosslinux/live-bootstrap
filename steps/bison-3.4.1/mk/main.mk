# SPDX-FileCopyrightText: 2020 Giovanni Mascellani gio@debian.org
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
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
	install -D bison $(DESTDIR)$(PREFIX)/bin/bison
	rm -rf $(PREFIX)/share/bison
	install -d $(DESTDIR)$(PREFIX)/share/bison
	mv data/skeletons/ $(DESTDIR)$(PREFIX)/share/bison
	mv data/m4sugar/ $(DESTDIR)$(PREFIX)/share/bison
