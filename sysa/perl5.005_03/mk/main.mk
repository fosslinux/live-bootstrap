# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>

# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc

.PHONY: all

MINIPERL_SRC = av deb doio doop dump globals gv hv mg miniperlmain op perl perlio perly pp pp_ctl pp_hot pp_sys regcomp regexec run scope sv taint toke universal util
MINIPERL_OBJ = $(addsuffix .o, $(MINIPERL_SRC))

all: miniperl

miniperl: $(MINIPERL_OBJ)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: all
	install miniperl $(PREFIX)/bin/perl
	mkdir -p $(PREFIX)/lib/perl5/5.005_03
	cp -R lib/* $(PREFIX)/lib/perl5/5.005_03/
