# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>

# SPDX-License-Identifier: GPL-3.0-or-later

VERSION=5.6.2
PRIVLIB_EXP=$(PREFIX)/lib/perl5/$(VERSION)

CC      = tcc
AR      = tcc -ar
CFLAGS  = -DPRIVLIB_EXP=\"$(PRIVLIB_EXP)\" \
          -DPERL_CORE=1 \
          -I. \
          -DVERSION=\"$(VERSION)\"

MINICFLAGS = -DPERL_EXTERNAL_GLOB

.PHONY: all

LIBPERL_SRC = av deb doio doop dump globals gv hv mg op perl perlapi perlio perly pp pp_ctl pp_hot pp_sys regcomp regexec run scope sv taint toke universal utf8 util xsutils
LIBPERL_OBJ = $(addsuffix .o, $(LIBPERL_SRC))

# POSIX is dealt with separately, Errno has no .a files either
EXTENSIONS = ByteLoader Data/Dumper Fcntl File/Glob IO
EXTENSIONS_A := $(foreach f,$(EXTENSIONS), lib/auto/$f/$(notdir $f).a)

EXTRA_EXTENSIONS = POSIX
EXTRA_EXTENSIONS_A = $(foreach f,$(EXTRA_EXTENSIONS), lib/auto/$f/$(notdir $f).a)

all: perl

op-mini.c: op.c
	cp op.c op-mini.c

libperl.a: $(LIBPERL_OBJ)
	$(AR) cr $@ $^

miniperl: miniperlmain.o op-mini.o libperl.a
	$(CC) $(CFLAGS) $(MINICFLAGS) $^ $(LDFLAGS) -o $@

lib/re.pm:
	cp ext/re/re.pm lib/re.pm

config.sh:
	echo "CONFIGDOTSH=true" > $@

lib/Config.pm: config.sh miniperl configpm lib/re.pm
	./miniperl configpm $@

autosplit.pl: lib/Config.pm
	echo 'use AutoSplit; autosplit_lib_modules(@ARGV)' > $@

prepare_library: autosplit.pl miniperl lib/re.pm
	./miniperl -Ilib $< lib/*.pm lib/*/*.pm

lib/ExtUtils/Miniperl.pm: miniperl
	./miniperl minimod.pl > $@

writemain: writemain.SH config.sh
	spitshell=cat eunicefix=true ./$<

perlmain.c: writemain
	./writemain $(EXTENSIONS_A) $(EXTRA_EXTENSIONS_A) > $@

ext/DynaLoader/DynaLoader.pm: miniperl lib/Config.pm
	./miniperl -Ilib ext/DynaLoader/DynaLoader_pm.PL DynaLoader.pm
	mv DynaLoader.pm $@
	$(MAKE) prepare_library

ext/DynaLoader/XSLoader.pm: miniperl lib/Config.pm
	./miniperl -Ilib ext/DynaLoader/XSLoader_pm.PL XSLoader.pm
	mv XSLoader.pm $@

ext/DynaLoader/DynaLoader.xs: ext/DynaLoader/DynaLoader.pm ext/DynaLoader/XSLoader.pm
	cp ext/DynaLoader/dl_dlopen.xs $@

ext/DynaLoader/DynaLoader.c: ext/DynaLoader/DynaLoader.xs miniperl
	cd $(dir $@); \
	$(CURDIR)/miniperl -I$(CURDIR)/lib $(CURDIR)/lib/ExtUtils/xsubpp -noprototypes -typemap $(CURDIR)/lib/ExtUtils/typemap $(notdir $<) > $(notdir $@)

lib/auto/DynaLoader/DynaLoader.a: ext/DynaLoader/DynaLoader.o
	mkdir -p lib/auto/DynaLoader
	$(AR) cr $@ $^

ext/POSIX/POSIX.c: ext/POSIX/POSIX.xs miniperl
	cd $(dir $@); \
	$(CURDIR)/miniperl -I$(CURDIR)/lib $(CURDIR)/lib/ExtUtils/xsubpp -noprototypes -typemap $(CURDIR)/lib/ExtUtils/typemap $(notdir $<) > $(notdir $@)

lib/auto/POSIX/POSIX.a: ext/POSIX/POSIX.o
	mkdir -p lib/auto/POSIX
	$(AR) cr $@ $^
	mkdir -p ext/POSIX/blib/lib
	cp ext/POSIX/POSIX.pod ext/POSIX/POSIX.pm ext/POSIX/blib/lib
	cd ext/POSIX/blib; \
	../../../miniperl -I../../../lib $(CURDIR)/autosplit.pl lib/*.pm
	cp ext/POSIX/blib/lib/auto/POSIX/* lib/auto/POSIX/

define build_rule
    lib/auto/$1/$(notdir $1).a: ext/$1/$(notdir $1).o $(patsubst %.c,%.o,$(wildcard ext/$1/*.c))
	mkdir -p lib/auto/$1
	$(AR) cr lib/auto/$1/$(notdir $1).a ext/$1/$(notdir $1).o $(patsubst %.c,%.o,$(wildcard ext/$1/*.c))
	cp ext/$1/$(notdir $1).pm lib/auto/$1/

    ext/$1/$(notdir $1).c: ext/$1/$(notdir $1).xs miniperl lib/Config.pm
	cd ext/$1; \
	$(CURDIR)/miniperl -I$(CURDIR)/lib $(CURDIR)/lib/ExtUtils/xsubpp -noprototypes -typemap $(CURDIR)/lib/ExtUtils/typemap $(notdir $1).xs > $(notdir $1).c
endef
$(foreach f,$(EXTENSIONS),$(eval $(call build_rule,$f)))

lib/Errno.pm: miniperl
	cd ext/Errno; \
	../../miniperl -I../../lib Errno_pm.PL
	mv ext/Errno/Errno.pm $@

perl: perlmain.o lib/auto/DynaLoader/DynaLoader.a $(EXTENSIONS_A) lib/auto/POSIX/POSIX.a libperl.a lib/re.pm lib/Errno.pm
	$(CC) $(CFLAGS) perlmain.o lib/auto/DynaLoader/DynaLoader.a $(EXTENSIONS_A) lib/auto/POSIX/POSIX.a libperl.a -o $@

install: all
	install -D perl $(DESTDIR)$(PREFIX)/bin/perl
	mkdir -p "$(DESTDIR)$(PRIVLIB_EXP)"
	cp -r lib/* "$(DESTDIR)$(PRIVLIB_EXP)"

	install -m 644 ext/DynaLoader/XSLoader.pm "$(DESTDIR)$(PRIVLIB_EXP)"
	install -m 644 ext/DynaLoader/DynaLoader.pm "$(DESTDIR)$(PRIVLIB_EXP)"

	install -m 644 ext/ByteLoader/ByteLoader.pm "$(DESTDIR)$(PRIVLIB_EXP)"
	mkdir "$(DESTDIR)$(PRIVLIB_EXP)/Data/"
	install -m 644 ext/Data/Dumper/Dumper.pm "$(DESTDIR)$(PRIVLIB_EXP)/Data/"
	install -m 644 ext/Fcntl/Fcntl.pm "$(DESTDIR)$(PRIVLIB_EXP)"
	install -m 644 ext/File/Glob/Glob.pm "$(DESTDIR)$(PRIVLIB_EXP)/File/"
	install -m 644 ext/IO/IO.pm "$(DESTDIR)$(PRIVLIB_EXP)"
	mkdir "$(DESTDIR)$(PRIVLIB_EXP)/IO/"
	cp ext/IO/lib/IO/*.pm "$(DESTDIR)$(PRIVLIB_EXP)/IO/"
	install -m 644 ext/POSIX/POSIX.pm "$(DESTDIR)$(PRIVLIB_EXP)/"
	install -m 644 ext/POSIX/POSIX.pod "$(DESTDIR)$(PRIVLIB_EXP)/"
	cp lib/auto/POSIX/* "$(DESTDIR)$(PRIVLIB_EXP)/auto/POSIX/"
