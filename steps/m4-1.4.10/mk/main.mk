# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>

# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc
AR      = tcc -ar

CFLAGS  = -I lib \
          -DVERSION=\"1.4.10\" \
          -DPACKAGE_BUGREPORT=\"bug-m4@gnu.org\" \
          -DPACKAGE_STRING=\"GNU\ M4\ 1.4.10\" \
          -DPACKAGE=\"m4\" \
          -DPACKAGE_NAME=\"GNU\ M4\" \
          -DHAVE_STDINT_H=1 \
          -DHAVE___FPENDING=1 \
          -DHAVE_DECL___FPENDING=1 \
          -D_GNU_SOURCE=1 \
          -D_GL_UNUSED= \
          -DGNULIB_CLOSE_STREAM=1 \
          -D__getopt_argv_const=const \
          -DSYSCMD_SHELL=\"/bin/sh\" \
          -DLIBDIR=\"$(PREFIX)/lib/i386-unknown-linux-musl\"

LDFLAGS = -L . -lm4

.PHONY: all

LIB_SRC = clean-temp cloexec close-stream closein closeout dup-safer error exitfail fatal-signal fd-safer fopen-safer getopt getopt1 gl_avltree_oset gl_linkedhash_list gl_list gl_oset localcharset mkstemp-safer regex obstack quotearg tmpdir verror version-etc version-etc-fsf xalloc-die xasprintf xmalloc xvasprintf
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
