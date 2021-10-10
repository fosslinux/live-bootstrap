# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

PACKAGE=coreutils
PACKAGE_NAME=GNU\ coreutils
PACKAGE_BUGREPORT=bug-coreutils@gnu.org
PACKAGE_VERSION=5.0
VERSION=5.0

CC      = tcc
LD      = tcc
AR      = tcc -ar

bindir = $(DESTDIR)$(PREFIX)/bin

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
          -DMB_LEN_MAX=16 \
          -DLIBDIR=\"$(PREFIX)/lib\" \
          -DHAVE_DECL_WCWIDTH=0 \
          -DHAVE_SYS_STAT_H=1 \
          -DHAVE_INTTYPES_H=1 \
          -DHAVE_DECL_MEMCHR=1 \
          -DHAVE_MEMORY_H=1 \
          -DPENDING_OUTPUT_N_BYTES=1 \
          -DCHAR_MIN=0 \
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
          -DENOTEMPTY=1 \
          -DLSTAT_FOLLOWS_SLASHED_SYMLINK=1 \
          -DHAVE_DECL_DIRFD=1 \
          -DLC_TIME=\"C\" \
          -DLC_COLLATE=\"C\" \
          -DHAVE_GETCWD=1 \
          -Dmy_strftime=nstrftime \
          -DDIR_TO_FD\(Dir_p\)=-1 \
          -DUTILS_OPEN_MAX=1000 \
          -Dmajor_t=unsigned \
          -Dminor_t=unsigned

.PHONY: all install

SRC_DIR=src

COREUTILS = basename cat chmod chroot cksum comm csplit cut dd dirname echo env expand expr factor false fmt fold head id join kill link ln logname mkfifo mkdir mknod nl od paste pathchk printf ptx pwd readlink rmdir seq sleep sort split sum sync tail tee tr tsort uname unexpand uniq unlink wc whoami test true yes

BINARIES = $(addprefix $(SRC_DIR)/, $(COREUTILS))

ALL=$(BINARIES) $(SRC_DIR)/cp $(SRC_DIR)/ls $(SRC_DIR)/install $(SRC_DIR)/md5sum $(SRC_DIR)/mv $(SRC_DIR)/rm $(SRC_DIR)/sha1sum
all: $(BINARIES) $(SRC_DIR)/cp $(SRC_DIR)/ls $(SRC_DIR)/install $(SRC_DIR)/md5sum $(SRC_DIR)/mv $(SRC_DIR)/rm $(SRC_DIR)/sha1sum

LIB_DIR = lib
LIB_SRC = acl alloca posixtm posixver strftime getopt getopt1 hash hash-pjw addext argmatch backupfile basename canon-host closeout cycle-check diacrit dirname dup-safer error exclude exitfail filemode __fpending file-type fnmatch fopen-safer full-read full-write getline getstr gettime hard-locale human idcache isdir imaxtostr linebuffer localcharset long-options makepath mbswidth md5 memcasecmp memcoll modechange offtostr path-concat physmem quote quotearg readtokens rpmatch safe-read safe-write same save-cwd savedir settime sha stpcpy stripslash strtoimax strtoumax umaxtostr unicodeio userspec version-etc xgetcwd xgethostname xmalloc xmemcoll xnanosleep xreadlink xstrdup xstrtod xstrtol xstrtoul xstrtoimax xstrtoumax yesno strnlen getcwd sig2str mountlist regex canonicalize mkstemp memrchr euidaccess ftw obstack strverscmp strftime tsearch

LIB_OBJECTS = $(addprefix $(LIB_DIR)/, $(addsuffix .o, $(LIB_SRC)))

$(LIB_DIR)/libfettish.a: $(LIB_OBJECTS)
	$(AR) cr $@ $^

$(BINARIES) : % : %.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/cp: $(SRC_DIR)/cp.o $(SRC_DIR)/copy.o $(SRC_DIR)/cp-hash.c $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/install: $(SRC_DIR)/install.o $(SRC_DIR)/copy.o $(SRC_DIR)/cp-hash.c $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/ls: $(SRC_DIR)/ls.o $(SRC_DIR)/ls-ls.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/md5sum: $(SRC_DIR)/md5.o $(SRC_DIR)/md5sum.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/mv: $(SRC_DIR)/mv.o $(SRC_DIR)/copy.o $(SRC_DIR)/remove.o $(SRC_DIR)/cp-hash.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/rm: $(SRC_DIR)/rm.o $(SRC_DIR)/remove.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(SRC_DIR)/sha1sum: $(SRC_DIR)/sha1sum.o $(SRC_DIR)/md5sum.o $(LIB_DIR)/libfettish.a
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

install: $(ALL)
	$(SRC_DIR)/install $^ $(bindir)
