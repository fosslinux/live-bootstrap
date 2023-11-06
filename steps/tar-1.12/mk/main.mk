# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC = tcc
AR = tcc -ar

# -DSIZEOF_UNSIGNED_LONG=4 forces use of simulated arithmetic
# This is to avoid running configure test to determine sizeof(long long)
CPPFLAGS = -DHAVE_FCNTL_H \
         -DHAVE_DIRENT_H \
         -DHAVE_GETCWD_H \
         -DHAVE_GETCWD \
         -DSIZEOF_UNSIGNED_LONG=4 \
         -DVERSION=\"1.12\" \
         -DPACKAGE=\"tar\"

CFLAGS = -I . -I lib
LDFLAGS = -L . -ltar -static

.PHONY: all

LIB_SRC = argmatch backupfile error fnmatch ftruncate getdate_stub getopt getopt1 getversion modechange msleep xgetcwd xmalloc xstrdup

LIB_OBJ = $(addprefix lib/, $(addsuffix .o, $(LIB_SRC)))

TAR_SRC = arith buffer compare create delete extract incremen list mangle misc names open3 rtapelib tar update
TAR_OBJ = $(addprefix src/, $(addsuffix .o, $(TAR_SRC)))

all: tar

libtar.a: $(LIB_OBJ)
	$(AR) cr $@ $^

tar: libtar.a $(TAR_OBJ)
	$(CC) $^ $(LDFLAGS) -o $@
