# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>

# SPDX-License-Identifier: GPL-3.0-or-later

# Note: This makefile is not currently parallel-safe.

VERSION=5.10.1
PRIVLIB_EXP=$(PREFIX)/lib/perl5/$(VERSION)

CC      = gcc
AR      = ar
CFLAGS  = -DPRIVLIB_EXP=\"$(PRIVLIB_EXP)\" \
          -DPERL_CORE=1 \
          -I. \
          -DVERSION=\"$(VERSION)\" \
		  -DNO_PPPORT_H

MINICFLAGS = -DPERL_EXTERNAL_GLOB

.PHONY: all

LIBPERL_SRC = av scope op doop doio dump gv hv mg reentr mro perl perly pp \
			  pp_hot pp_ctl pp_sys regcomp regexec utf8 sv taint toke util \
			  deb run universal xsutils pad globals perlio perlapi numeric \
			  mathoms locale pp_pack pp_sort
LIBPERL_OBJ = $(addsuffix .o, $(LIBPERL_SRC))

EXTENSIONS = File/Glob Digest/SHA Data/Dumper Cwd
EXTENSIONS_A := $(foreach f,$(EXTENSIONS), lib/auto/$f/$(notdir $f).a)
EXTENSIONS_PM = ext/File/Glob/Glob.pm ext/Digest/SHA/lib/Digest/SHA.pm \
				ext/Data/Dumper/Dumper.pm

all: perl

# miniperl

opmini.c: op.c
	cp op.c opmini.c

perlmini.c: perl.c
	cp perl.c perlmini.c

generate_uudmap: generate_uudmap.o
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

uudmap.h: generate_uudmap 
	./generate_uudmap > $@ 

git_version.h: lib/Config_git.pl

lib/Config_git.pl:
	perl make_patchnum.pl

globals.o: uudmap.h

perl.o: git_version.h

perlmini.o: git_version.h

libperl.a: $(LIBPERL_OBJ)
	$(AR) cr $@ $^

miniperl: miniperlmain.o opmini.o perlmini.o libperl.a
	$(CC) $(CFLAGS) $(MINICFLAGS) $^ $(LDFLAGS) -o $@
	# An extremely dodgy hack
	sed -i 's/va\.a\.a/v5.8.1/' miniperl
	chmod +x miniperl

# full perl

lib/re.pm:
	cp ext/re/re.pm lib/re.pm

lib/Config.pm: config.sh miniperl configpm lib/re.pm
	./miniperl -Ilib configpm

writemain: writemain.SH
	spitshell=cat eunicefix=true ./$<

perlmain.c: writemain $(EXTENSIONS_A)
	./writemain $(EXTENSIONS_A) > $@

perl: perlmain.o libperl.a $(EXTENSIONS_A) lib/File/Glob.pm lib/Digest/SHA.pm \
	lib/DynaLoader.pm lib/XSLoader.pm lib/Data/Dumper.pm
	$(CC) $(CFLAGS) perlmain.o libperl.a $(EXTENSIONS_A) -o $@
	# An extremely dodgy hack (v2)
	sed -i 's/va\.a\.a/v5.8.1/' perl
	chmod +x perl

# extensions

autosplit.pl: lib/re.pm lib/Config.pm miniperl
	echo 'use AutoSplit; autosplit_lib_modules(@ARGV)' > $@

ext/DynaLoader/DynaLoader.pm: miniperl lib/Config.pm
	./miniperl -Ilib ext/DynaLoader/DynaLoader_pm.PL DynaLoader.pm
	mv DynaLoader.pm $@

ext/DynaLoader/XSLoader.pm: miniperl lib/Config.pm
	./miniperl -Ilib ext/DynaLoader/XSLoader_pm.PL XSLoader.pm
	mv XSLoader.pm $@

lib/DynaLoader.pm: ext/DynaLoader/DynaLoader.pm
	install $< $@

lib/XSLoader.pm: ext/DynaLoader/XSLoader.pm
	install $< $@

lib/File/Glob.pm: ext/File/Glob/Glob.pm
	install $< $@

lib/Digest/SHA.pm: ext/Digest/SHA/lib/Digest/SHA.pm
	install $< $@

lib/Data:
	mkdir $@

lib/Data/Dumper.pm: ext/Data/Dumper/Dumper.pm lib/Data
	install $< $@

define build_rule
lib/auto/$1/$(notdir $1).a: ext/$1/$(notdir $1).o $(patsubst %.c,%.o,$(wildcard ext/$1/*.c)) $(EXTENSIONS_PM)
	mkdir -p lib/auto/$1
	$(AR) cr lib/auto/$1/$(notdir $1).a ext/$1/$(notdir $1).o $(patsubst %.c,%.o,$(wildcard ext/$1/*.c))

ext/$1/$(notdir $1).c: miniperl lib/Config.pm
	cd ext/$1; \
	$(CURDIR)/miniperl -I$(CURDIR)/lib Makefile.PL; \
	$(CURDIR)/miniperl -I$(CURDIR)/lib $(CURDIR)/lib/ExtUtils/xsubpp -noprototypes -typemap $(CURDIR)/lib/ExtUtils/typemap `echo $(notdir $1) | sed 's/.*-//'`.xs > $(notdir $1).c
endef
$(foreach f,$(EXTENSIONS),$(eval $(call build_rule,$f)))

install: all
	install -D perl "$(DESTDIR)$(PREFIX)/bin/perl"
	mkdir -p "$(DESTDIR)$(PRIVLIB_EXP)"
	cp -r lib/* "$(DESTDIR)$(PRIVLIB_EXP)"
