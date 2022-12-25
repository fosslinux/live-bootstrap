# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC = tcc
AR = tcc -ar

CPPFLAGS = -DENABLE_NLS=0 \
         -DHAVE_FCNTL_H \
         -DHAVE_ALLOCA_H \
         -DSED_FEATURE_VERSION=\"4.0\" \
         -DVERSION=\"4.0.9\" \
         -DPACKAGE=\"sed\"
CFLAGS = -I . -I lib
LDFLAGS = -L . -lsed -static

.PHONY: all

ifeq ($(LIBC),mes)
    LIB_SRC = getline
else
    LIB_SRC = alloca
endif

LIB_SRC += getopt1 getopt utils regex obstack strverscmp mkstemp

LIB_OBJ = $(addprefix lib/, $(addsuffix .o, $(LIB_SRC)))

SED_SRC = compile execute regexp fmt sed
SED_OBJ = $(addprefix sed/, $(addsuffix .o, $(SED_SRC)))

all: sed/sed

lib/regex.h: lib/regex_.h
	cp $< $@

lib/regex.o: lib/regex.h

libsed.a: $(LIB_OBJ)
	$(AR) cr $@ $^

sed/sed: libsed.a $(SED_OBJ)
	$(CC) $^ $(LDFLAGS) -o $@

install:
	install -D sed/sed $(DESTDIR)$(PREFIX)/bin/sed
