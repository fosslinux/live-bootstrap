# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>

# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc

.PHONY: all

MINIPERL_SRC = av deb doio doop dump gv hv mg miniperlmain op perl perly pp pp_ctl pp_hot pp_sys regcomp regexec run scope sv taint toke util
MINIPERL_OBJ = $(addsuffix .o, $(MINIPERL_SRC))

all: miniperl

miniperl: $(MINIPERL_OBJ)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: all
	install -D miniperl $(DESTDIR)$(PREFIX)/bin/perl
