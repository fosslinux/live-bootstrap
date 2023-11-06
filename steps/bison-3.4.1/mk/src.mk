# SPDX-FileCopyrightText: 2020 Giovanni Mascellani gio@debian.org
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src.a: AnnotationList.o assoc.o closure.o complain.o conflicts.o derives.o files.o fixits.o getargs.o gram.o graphviz.o ielr.o InadequacyList.o lalr.o location.o lr0.o main.o muscle-tab.o named-ref.o nullable.o output.o parse-gram.o print.o print-graph.o print-xml.o reader.o reduce.o relation.o Sbitset.o scan-code.o scan-gram.o scan-skel.o state.o symlist.o symtab.o tables.o uniqstr.o
	$(AR) r $@ $^

closure.o: parse-gram.h
parse-gram.h: parse-gram.c

%.o: %.c
	$(CC) $(CFLAGS) -g -c -I. -I.. -I../lib -o $@ $<

%.c: %.y
	bison -dv $<
	mv $(shell echo $@ | sed -e 's/c$$/tab.c/') $@
	mv $(shell echo $@ | sed -e 's/c$$/tab.h/') $(shell echo $@ | sed -e 's/c$$/h/')

%.c: %.l
	/bin/sh ../build-aux/ylwrap $< lex.yy.c $@ -- flex

.PRECIOUS: %.c %.o %.h
