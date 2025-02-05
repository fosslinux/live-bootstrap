# SPDX-FileCopyrightText: 2019 Brian Callahan <bcallah@openbsd.org>
# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: CC0-1.0

CC =		tcc
CFLAGS =	-D__dead= -D__unused=
LDFLAGS =   -static
LIBS =      -lgetopt
PREFIX =	/usr
BINDIR =    /usr/bin
MANDIR =	/usr/share/man
PROG =		yacc

OBJS =	closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
	skeleton.o symtab.o verbose.o warshall.o portable.o

all: ${PROG}

${PROG}: ${OBJS}
	${CC} ${LDFLAGS} -o ${PROG} ${OBJS} ${LIBS}

install: all
	install -d ${DESTDIR}${BINDIR}
	install -d ${DESTDIR}${MANDIR}
	install -m 555 ${PROG} ${DESTDIR}${BINDIR}
	install -m 555 yyfix.sh ${DESTDIR}${BINDIR}/yyfix

test:
	@echo "No tests"

clean:
	rm -f ${PROG} ${OBJS}

distclean: clean
	rm -f Makefile config.h
