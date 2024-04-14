DEST	      = .

HDRS	      = defs.h

CFLAGS	      ?= -O -DNDEBUG

LDFLAGS	      ?=

LIBS	      ?=

LINKER	      ?= $(CC)

MAKEFILE      = Makefile

OBJS	      = closure.o \
		error.o graph.o \
		lalr.o \
		lr0.o \
		main.o \
		mkpar.o mstring.o \
		output.o \
		reader.o \
		symtab.o \
		verbose.o \
		warshall.o yaccpar.o

PRINT	      = pr -f -l88

PROGRAM	      = yacc

SRCS	      = closure.c \
		error.c graph.c \
		lalr.c \
		lr0.c \
		main.c \
		mkpar.c mstring.c \
		output.c \
		reader.c \
		symtab.c \
		verbose.c \
		warshall.c yaccpar.c

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@$(LINKER) $(LDFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS)

clean:;		@rm -f $(OBJS)

clobber:;	@rm -f $(OBJS) $(PROGRAM)

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DEST)

listing:;	@$(PRINT) Makefile $(HDRS) $(SRCS) | lpr

lint:;		@lint $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

###
closure.o: defs.h
error.o: defs.h
lalr.o: defs.h
lr0.o: defs.h
main.o: defs.h
mkpar.o: defs.h
output.o: defs.h
reader.o: defs.h
symtab.o: defs.h
verbose.o: defs.h
warshall.o: defs.h
btyaccpar.o: defs.h
yaccpar.o: defs.h
graph.o: defs.h
mstring.o: defs.h
