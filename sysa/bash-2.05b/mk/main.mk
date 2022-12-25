# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

include common.mk

CFLAGS  = \
		  -I. \
		  -Iinclude \
		  -Ilib \
		  -Ilib/sh \
		  -Ibuiltins \
		  $(COMMON_CFLAGS)

LDFLAGS   = -L. -Lbuiltins -static
LIBRARIES = libsh.a builtins/libbuiltins.a libglob.a libtilde.a

SHLIB_FILES = clktck getcwd getenv oslib setlinebuf strcasecmp strerror strtod \
	vprint itos rename zread zwrite shtty inet_aton netopen \
	strpbrk timeval clock makepath pathcanon pathphys stringlist stringvec \
	tmpfile spell strtrans strindex shquote snprintf mailstat fmtulong \
	fmtullong strtoll strtoull strtoimax strtoumax fmtumax netconn mktime \
	strftime xstrchr zcatfd
# FIXME: for some reason these don't get picked up correctly in the
# final linking cmd
SHLIB_ODD_FILES = zcatfd strtoumax spell pathphys
SHLIB_OBJS = $(addprefix lib/sh/, $(addsuffix .o, $(SHLIB_FILES)))
SHLIB_ODD_OBJS = $(addprefix lib/sh/, $(addsuffix .o, $(SHLIB_ODD_FILES)))

MKBUILTINS_OBJS = builtins/mkbuiltins.o
BUILTINS_DEFS = $(addprefix builtins/, $(addsuffix .def, $(BUILTINS_DEF_FILES)))

GLOB_FILES = glob strmatch smatch xmbsrtowcs
GLOB_OBJS = $(addprefix lib/glob/, $(addsuffix .o, $(GLOB_FILES)))

TILDE_OBJS = lib/tilde/tilde.o

MKSYNTAX_OBJS = mksyntax.o
MKSIGNAMES_OBJS = support/mksignames.o

FILES = shell eval y.tab general make_cmd print_cmd dispose_cmd execute_cmd \
	variables copy_cmd error expr flags nojobs subst hashcmd hashlib mailcheck \
	trap input unwind_prot pathexp sig test version alias array arrayfunc \
	braces bracecomp bashhist bashline list stringlib locale findcmd redir \
	pcomplete pcomplib syntax xmalloc siglist
OBJS = $(addsuffix .o, $(FILES))

all: bash

# Builtins

mkbuiltins: $(MKBUILTINS_OBJS)
	$(CC) $(CFLAGS) $(MKBUILTINS_OBJS) $(LDFLAGS) -o $@
	./mkbuiltins -externfile builtins/builtext.h -structfile builtins/builtins.c -noproduction $(BUILTINS_DEFS)

# libsh

libsh.a: $(SHLIB_OBJS)
	$(AR) cr $@ $^

# libglob

libglob.a: $(GLOB_OBJS)
	$(AR) cr $@ $^

# libtilde

libtilde.a: $(TILDE_OBJS)
	$(AR) cr $@ $^

# The actual program

mksyntax: $(MKSYNTAX_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@ -lgetopt

syntax.c: mksyntax
	./mksyntax -o $@

mksignames: $(MKSIGNAMES_OBJS)
	$(CC) $^ $(LDFLAGS) -o $@ -lgetopt

signames.h: mksignames
	./mksignames $@

y.tab.c: parse.y
	yacc -d $^

trap.c: signames.h

bash: libsh.a libglob.a libtilde.a $(OBJS)
	$(CC) -o bash $(LIBRARIES) $(OBJS) $(SHLIB_ODD_OBJS) $(LDFLAGS) -lsh -lbuiltins -lglob -ltilde
