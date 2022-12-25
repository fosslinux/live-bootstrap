# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2019-2020 Giovanni Mascellani <gio@debian.org>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CFLAGS = -DVERSION=\"2.5.11\"
LDFLAGS = -static

all: flex

flex: ccl.o dfa.o ecs.o gen.o main.o misc.o nfa.o parse.o scan.o skel.o sym.o tblcmp.o yylex.o options.o scanopt.o buf.o
	tcc -o $@ $^ -ll $(LDFLAGS)

flex-tmp: ccl.o dfa.o ecs.o gen.o main.o misc.o nfa.o parse.o scan-tmp.o skel.o sym.o tblcmp.o yylex.o options.o scanopt.o buf.o
	tcc -o $@ $^ -ll $(LDFLAGS)

%.o: %.c
	tcc -g -c $(CFLAGS) -o $@ $<

scan.o: parse.h
scan-tmp.o: parse.h
yylex.o: parse.h

parse.c parse.h: parse.y
	yacc -d parse.y
	mv y.tab.h parse.h
	mv y.tab.c parse.c

scan-tmp.c: scan.lex.l
	lex scan.lex.l
	sed 's|yylex|flexscan|g' -i lex.yy.c
	mv lex.yy.c scan-tmp.c

scan.c: scan.l flex-tmp
	./flex-tmp scan.l
	mv lex.yy.c scan.c

skel.c: mkskel.sh flex.skl
	/bin/sh ./mkskel.sh ./flex.skl > skel.c

install: all
	install -D flex $(DESTDIR)$(PREFIX)/bin/flex
	ln -sf $(PREFIX)/bin/flex $(DESTDIR)$(PREFIX)/bin/lex
