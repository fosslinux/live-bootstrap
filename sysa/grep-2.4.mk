PACKAGE=grep
VERSION=2.4

CC      = tcc
LD      = tcc
AR      = tcc -ar

CFLAGS  = -DPACKAGE=\"$(PACKAGE)\" \
          -DVERSION=\"$(VERSION)\" \
          -DHAVE_DIRENT_H=1 \
          -DHAVE_UNISTD_H=1

LDFLAGS = 

.PHONY: all

GREP_SRC = grep dfa kwset obstack regex stpcpy savedir getopt getopt1 search grepmat
GREP_OBJECTS = $(addprefix src/, $(addsuffix .o, $(GREP_SRC)))

all: grep

grep: $(GREP_OBJECTS)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@
