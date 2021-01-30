CFLAGS =    -I . \
            -DVERSION=\"2.5.14\"

all: flex

flex: ccl.o dfa.o ecs.o gen.o main.o misc.o nfa.o parse.o scan.o skel.o sym.o tblcmp.o yylex.o options.o scanopt.o buf.o
	tcc -o $@ $^

%.o: %.c
	tcc -g -c $(CFLAGS) -o $@ $<

scan.o: parse.h
yylex.o: parse.h

parse.c parse.h: parse.y
	yacc -d parse.y
	mv y.tab.h parse.h
	mv y.tab.c parse.c

scan.c: scan.l
	flex scan.l
	mv lex.yy.c scan.c

skel.c: mkskel.sh flex.skl
	/bin/sh ./mkskel.sh ./flex.skl > skel.c

install: all
	install flex $(PREFIX)/bin
	ln -sf $(PREFIX)/bin/flex $(PREFIX)/bin/lex
