# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>

# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc
AR      = tcc -ar

CFLAGS  = -I lib \
          -DVERSION=\"1.4.7\" \
          -DPACKAGE_BUGREPORT=\"bug-m4@gnu.org\" \
          -DPACKAGE_STRING=\"GNU\ M4\ 1.4.7\" \
          -DPACKAGE=\"m4\" \
          -DPACKAGE_NAME=\"GNU\ M4\" \
          -DHAVE_STDINT_H=1 \
          -DHAVE___FPENDING=1 \
          -DHAVE_DECL___FPENDING=1 \
          -D_GNU_SOURCE=1 \
          -D_GL_UNUSED= \
          -D__getopt_argv_const=const \
          -DSYSCMD_SHELL=\"/bin/sh\"

LDFLAGS = -L . -lm4

.PHONY: all

LIB_SRC = cloexec close-stream dup-safer error exitfail fd-safer fopen-safer getopt getopt1 mkstemp-safer regex obstack tmpfile-safer verror xalloc-die xasprintf xmalloc xvasprintf
LIB_OBJECTS = $(addprefix lib/, $(addsuffix .o, $(LIB_SRC)))

M4_SRC = m4 builtin debug eval format freeze input macro output path symtab
M4_OBJ = $(addprefix src/, $(addsuffix .o, $(M4_SRC)))

all: src/m4

src/m4: libm4.a $(M4_OBJ)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

libm4.a: $(LIB_OBJECTS)
	$(AR) cr $@ $^

%.o : %.c lib/config.h
	$(CC) -c -o $@ $< $(CFLAGS)

lib/config.h:
	touch lib/config.h

install: all
	install -D src/m4 $(DESTDIR)$(PREFIX)/bin/m4
