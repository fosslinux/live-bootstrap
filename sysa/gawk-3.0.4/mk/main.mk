# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>

# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc

CFLAGS = -I vms \
         -D__GNUC__ \
         -DRETSIGTYPE=void \
         -DSPRINTF_RET=int \
         -DHAVE_VPRINTF=1 \
         -DHAVE_STDARG_H=1 \
         -DDEFPATH=\"$(PREFIX)/share/awk\" \
         -DHAVE_ALLOCA=1 \
         -D__builtin_alloca=alloca \
         -DHAVE_SYSTEM=1 \
         -DHAVE_MEMCPY=1 \
         -DHAVE_STRERROR=1 \
         -DHAVE_STRFTIME=1 \
         -DHAVE_TZSET=1

.PHONY: all

GAWK_SRC = alloca array awktab builtin dfa eval field getopt getopt1 gawkmisc io main missing msg node random re regex version
GAWK_OBJ = $(addsuffix .o, $(GAWK_SRC))

all: gawk

gawk: $(GAWK_OBJ)
	$(CC) -o $@ $^

awktab.c: awk.y
	bison $^ -o $@

install: all
	install gawk "$(PREFIX)/bin"
	ln -s "$(PREFIX)/bin/gawk" "$(PREFIX)/bin/awk"
