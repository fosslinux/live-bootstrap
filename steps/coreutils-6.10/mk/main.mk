# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

PACKAGE=coreutils
PACKAGE_NAME=GNU\ coreutils
PACKAGE_BUGREPORT=bug-coreutils@gnu.org
PACKAGE_VERSION=6.10
VERSION=6.10

CC      = tcc
LD      = tcc
AR      = tcc -ar

bindir=$(DESTDIR)$(PREFIX)/bin

CFLAGS  = -I . -I lib \
          -DPACKAGE=\"$(PACKAGE)\" \
          -DPACKAGE_NAME=\"$(PACKAGE_NAME)\" \
          -DGNU_PACKAGE=\"$(PACKAGE_NAME)\" \
          -DPACKAGE_BUGREPORT=\"$(PACKAGE_BUGREPORT)\" \
          -DPACKAGE_VERSION=\"$(PACKAGE_VERSION)\" \
          -DHOST_OPERATING_SYSTEM=\"Linux\" \
          -DVERSION=\"$(VERSION)\" \
          -DHAVE_LIMITS_H=1 \
          -DHAVE_DECL_FREE=1 \
          -DHAVE_LONG_LONG=1 \
          -DHAVE_UNSIGNED_LONG_LONG=1 \
          -DHAVE_DECL_MALLOC=1 \
          -DHAVE_DECL_STRERROR=1 \
          -DHAVE_STRERROR=1 \
          -DHAVE_MALLOC=1 \
          -DHAVE_STDLIB_H=1 \
          -DHAVE_WCHAR_H=1 \
          -DHAVE_SYS_TYPES_H=1 \
          -DHAVE_REALLOC=1 \
          -DHAVE_DECL_REALLOC=1 \
          -DHAVE_DECL_GETENV=1 \
          -DHAVE_DIRENT_H=1 \
          -DHAVE_DECL___FPENDING=0 \
          -DSTDC_HEADERS=1 \
          -DHAVE_ALLOCA_H=1 \
          -DHAVE_STRUCT_TIMESPEC=1 \
          -DHAVE_STRING_H=1 \
          -DHAVE_SYS_TIME_H=1 \
          -DHAVE_SETLOCALE=1 \
          -DHAVE_LOCALE_H=1 \
          -DTIME_WITH_SYS_TIME=1 \
          -DHAVE_STDINT_H=1 \
          -DLIBDIR=\"/usr/lib\" \
          -DHAVE_DECL_WCWIDTH=0 \
          -DHAVE_SYS_STAT_H=1 \
          -DHAVE_INTTYPES_H=1 \
          -DHAVE_DECL_MEMCHR=1 \
          -DHAVE_MEMORY_H=1 \
          -DPENDING_OUTPUT_N_BYTES=1 \
          -DLOCALEDIR=NULL \
          -DHAVE_FCNTL_H=1 \
          -DEPERM=1 \
          -DHAVE_DECL_STRTOUL=1 \
          -DHAVE_DECL_STRTOULL=1 \
          -DHAVE_DECL_STRTOL=1 \
          -DHAVE_DECL_STRTOLL=1 \
          -DHAVE_RMDIR=1 \
          -DRMDIR_ERRNO_NOT_EMPTY=39 \
          -DHAVE_DECL_FREE=1 \
          -DLSTAT_FOLLOWS_SLASHED_SYMLINK=1 \
          -DHAVE_DECL_DIRFD=1 \
          -DHAVE_GETCWD=1 \
          -Dmy_strftime=nstrftime \
          -DDIR_TO_FD\(Dir_p\)=-1 \
          -DUTILS_OPEN_MAX=1000 \
          -Dmajor_t=unsigned \
          -Dminor_t=unsigned \
          -DHAVE_GETTIMEOFDAY=1 \
          -DHAVE_TIME_R_POSIX=1 \
          -DHASH_ALGO_SHA256 \
          -DFLEXIBLE_ARRAY_MEMBER \
          -DS_IRWXUGO='(S_IRWXU | S_IRWXG | S_IRWXO)' \
          -DGNULIB_CANONICALIZE \
          -DO_BINARY=0

.PHONY: all install

SRC_DIR=src

COREUTILS = date mktemp

BINARIES = $(addprefix $(SRC_DIR)/, $(COREUTILS))

ALL=$(BINARIES) $(SRC_DIR)/sha256sum
all: $(BINARIES) $(SRC_DIR)/sha256sum

LIB_DIR = lib
LIB_SRC = acl alloca getdate fprintftime posixtm posixver strftime hash hash-pjw argmatch backupfile basename canon-host closeout cycle-check diacrit dirname dup-safer error exclude exitfail filemode fpending file-type fnmatch fopen-safer full-read full-write getline gettime hard-locale human idcache imaxtostr linebuffer localcharset long-options mbswidth md5 memcasecmp memcoll modechange offtostr physmem quote quotearg readtokens rpmatch safe-read safe-write same save-cwd savedir settime sha256 stpcpy stripslash umaxtostr unicodeio userspec version-etc version-etc-fsf xgetcwd xgethostname xmalloc xmemcoll xnanosleep readlink xstrtod xstrtol xstrtoul xstrtoimax xstrtoumax yesno strnlen getcwd sig2str mountlist canonicalize mkstemp memrchr euidaccess obstack strverscmp strftime xalloc-die close-stream tempname filenamecat xstrndup randint randread rand-isaac gethrxtime

LIB_OBJECTS = $(addprefix $(LIB_DIR)/, $(addsuffix .o, $(LIB_SRC)))

$(LIB_DIR)/libfettish.a: $(LIB_OBJECTS)
	$(AR) cr $@ $^

$(BINARIES) : % : %.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/sha256sum: $(SRC_DIR)/md5sum.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/mktemp: $(SRC_DIR)/mktemp.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: $(ALL)
	install -d $(bindir)
	install $^ $(bindir)
