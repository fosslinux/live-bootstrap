# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC = tcc
AR = tcc -ar

# -DSIZEOF_UNSIGNED_LONG=4 forces use of simulated arithmetic
# This is to avoid running configure test to determine sizeof(long long)
CPPFLAGS = -DNO_UTIME \
         -Dstrlwr=unused

CFLAGS = -I . -static

.PHONY: all

GZIP_SRC = gzip bits crypt deflate getopt inflate lzw trees unlzh unlzw unpack unzip util zip
GZIP_OBJ = $(addsuffix .o, $(GZIP_SRC))

all: gzip

gzip: $(GZIP_OBJ)
	$(CC) $^ -o $@
