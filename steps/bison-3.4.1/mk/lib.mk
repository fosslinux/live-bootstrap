# SPDX-FileCopyrightText: 2020 Giovanni Mascellani gio@debian.org
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

lib.a: allocator.o areadlink.o argmatch.o asnprintf.o basename.o basename-lgpl.o binary-io.o bitrotate.o bitset.o bitsetv.o calloc.o canonicalize-lgpl.o careadlinkat.o c-ctype.o cloexec.o close.o closeout.o close-stream.o concat-filename.o c-strcasecmp.o c-strncasecmp.o dirname.o dirname-lgpl.o dup2.o dup-safer.o dup-safer-flag.o exitfail.o fatal-signal.o fd-hook.o fd-safer.o fd-safer-flag.o fopen.o fopen-safer.o fprintf.o frexp.o frexpl.o fstat.o getdtablesize.o get-errno.o gethrxtime.o getprogname.o getrusage.o gettime.o gettimeofday.o gl_array_list.o gl_list.o gl_xlist.o hard-locale.o hash.o isnan.o isnand.o isnanf.o isnanl.o itold.o ldexpl.o localcharset.o localtime-buffer.o lstat.o main.o malloca.o malloc.o math.o mbrtowc.o mbsinit.o mbswidth.o memchr.o msvc-inval.o obstack.o obstack_printf.o open.o path-join.o perror.o pipe2-safer.o pipe-safer.o printf-args.o printf.o printf-frexp.o printf-frexpl.o printf-parse.o progname.o progreloc.o quotearg.o raise.o rawmemchr.o readlink.o realloc.o relocatable.o rename.o rmdir.o setenv.o sig-handler.o signbitd.o signbitf.o signbitl.o spawnattr_destroy.o spawnattr_init.o sprintf.o stat.o stat-time.o stat-w32.o stpcpy.o strchrnul.o strdup.o strerror.o stripslash.o strndup.o strnlen.o strverscmp.o timespec.o timevar.o unistd.o unlink.o unsetenv.o vasnprintf.o wait-process.o wctype-h.o xalloc-die.o xconcat-filename.o xmalloc.o xmemdup0.o xreadlink.o xsize.o xstrndup.o xtime.o yyerror.o error.o bitset/array.o bitset/list.o bitset/stats.o bitset/table.o bitset/vector.o fseterr.o spawn-pipe.o
	$(AR) r $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -g -c -I.. -I../lib -o $@ $<
