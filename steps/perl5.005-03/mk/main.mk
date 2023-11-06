# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>

# SPDX-License-Identifier: GPL-3.0-or-later

VERSION=5.005_03
PRIVLIB_EXP=$(PREFIX)/lib/perl5/$(VERSION)

CC      = tcc
CFLAGS  = -DPRIVLIB_EXP=\"$(PRIVLIB_EXP)\"

.PHONY: all

MINIPERL_SRC = av deb doio doop dump globals gv hv mg miniperlmain op perl perlio perly pp pp_ctl pp_hot pp_sys regcomp regexec run scope sv taint toke universal util
MINIPERL_OBJ = $(addsuffix .o, $(MINIPERL_SRC))

all: miniperl

miniperl: $(MINIPERL_OBJ)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: all
	install -D miniperl $(DESTDIR)$(PREFIX)/bin/perl
	mkdir -p "$(DESTDIR)$(PRIVLIB_EXP)"
	cp -r lib/* "$(DESTDIR)$(PRIVLIB_EXP)"
