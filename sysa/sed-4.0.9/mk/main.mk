# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
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

.PHONY: all

LIB_SRC = alloca getopt1 getopt utils regex obstack strverscmp mkstemp
LIB_OBJ = $(addprefix lib/, $(addsuffix .o, $(LIB_SRC)))

SED_SRC = compile execute regexp fmt sed
SED_OBJ = $(addprefix sed/, $(addsuffix .o, $(SED_SRC)))

all: sed/sed

libsed.a: $(LIB_OBJ)
	$(AR) cr $@ $^

sed/sed: $(SED_OBJ) libsed.a
	$(CC) -o $@ $^

install:
	install sed/sed $(DESTDIR)$(PREFIX)/bin
