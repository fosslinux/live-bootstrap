# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC := tcc
CFLAGS = -DVERSION=\"2.6.4\" \
         -DM4=\"m4\" \
         -DHAVE_LIMITS_H=1

all: flex

flex: buf.o ccl.o dfa.o ecs.o filter.o gen.o main.o misc.o nfa.o options.o parse.o regex.o scan.o scanflags.o scanopt.o skel.o sym.o tables.o tables_shared.o tblcmp.o yylex.o
	$(CC) -o $@ $^ -lm

flex-tmp: buf.o ccl.o dfa.o ecs.o filter.o gen.o main.o misc.o nfa.o options.o parse.o regex.o scan-tmp.o scanflags.o scanopt.o skel.o sym.o tables.o tables_shared.o tblcmp.o yylex.o
	$(CC) -o $@ $^ -lm

%.o: %.c
	$(CC) $(CFLAGS) -g -c -I. -o $@ $<

main.o: parse.h
scan.o: parse.h
scan-tmp.o: parse.h
yylex.o: parse.h

parse.c parse.h: parse.y
	yacc -d parse.y
	mv y.tab.h parse.h
	mv y.tab.c parse.c
	echo 'extern int yylval;' >> parse.h

scan-tmp.c: scan.l
	flex scan.l
	mv lex.yy.c scan-tmp.c

scan.c: scan.l flex-tmp
	./flex-tmp scan.l
	mv lex.yy.c scan.c

skel.c: mkskel.sh flex.skl
	/bin/sh ./mkskel.sh . m4 2.6.4 > skel.c

install: all
	install -D flex $(DESTDIR)$(PREFIX)/bin/flex
	ln -sf flex $(DESTDIR)$(PREFIX)/bin/lex
