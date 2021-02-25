# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>

# SPDX-License-Identifier: GPL-3.0-or-later

VERSION=5.6.2
PRIVLIB_EXP=$(PREFIX)/lib/perl5/$(VERSION)

CC      = tcc
CFLAGS  = -DPRIVLIB_EXP=\"$(PRIVLIB_EXP)\" \
          -DPERL_EXTERNAL_GLOB \
          -DPERL_CORE=1

.PHONY: all

MINIPERL_SRC = av deb doio doop dump globals gv hv mg miniperlmain op perl perlapi perlio perly pp pp_ctl pp_hot pp_sys regcomp regexec run scope sv taint toke universal utf8 util xsutils
MINIPERL_OBJ = $(addsuffix .o, $(MINIPERL_SRC))

all: miniperl

miniperl: $(MINIPERL_OBJ)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: all
	install miniperl $(PREFIX)/bin/perl
	mkdir -p "$(PRIVLIB_EXP)"
	cp -r lib/* "$(PRIVLIB_EXP)"
