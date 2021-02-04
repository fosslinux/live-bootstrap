PRODUCT = m4
VERSION = 1.4

CC      = tcc
LD      = tcc
AR      = tcc -ar

CFLAGS  = -I lib \
          -DPRODUCT=\"$(PRODUCT)\" \
          -DVERSION=\"$(VERSION)\" \
          -DHAVE_SIGNAL_H=1 \
          -DPACKAGE_STRING=\"GNU\ M4\ 1.4.4\"

LDFLAGS = -L . -lm4

.PHONY: all

LIB_SRC = regex getopt getopt1 error obstack xmalloc xstrdup
LIB_OBJECTS = $(addprefix lib/, $(addsuffix .o, $(LIB_SRC)))

M4_SRC = m4 builtin debug eval format freeze input macro output path symtab
M4_OBJ = $(addprefix src/, $(addsuffix .o, $(M4_SRC)))

all: m4

m4: libm4.a $(M4_OBJ)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

libm4.a: $(LIB_OBJECTS)
	$(AR) cr $@ $^

install: all
	install m4 $(PREFIX)/bin
