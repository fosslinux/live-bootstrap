#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e
. ./config.sh
set -u

TESTS="
lib/tests/scaffold/t.c
lib/tests/scaffold/01-return-0.c
lib/tests/scaffold/02-return-1.c
lib/tests/scaffold/03-call.c
lib/tests/scaffold/04-call-0.c
lib/tests/scaffold/05-call-1.c
lib/tests/scaffold/06-call-not-1.c
lib/tests/scaffold/06-not-call-1.c
lib/tests/scaffold/06-call-2.c
lib/tests/scaffold/06-call-string.c
lib/tests/scaffold/06-call-variable.c
lib/tests/scaffold/06-return-void.c
lib/tests/scaffold/07-include.c
lib/tests/scaffold/08-assign.c
lib/tests/scaffold/08-assign-negative.c
lib/tests/scaffold/08-assign-global.c
lib/tests/scaffold/10-if-0.c
lib/tests/scaffold/11-if-1.c
lib/tests/scaffold/12-if-eq.c
lib/tests/scaffold/13-if-neq.c
lib/tests/scaffold/14-if-goto.c
lib/tests/scaffold/15-if-not-f.c
lib/tests/scaffold/16-if-t.c
lib/tests/scaffold/17-compare-char.c
lib/tests/scaffold/17-compare-ge.c
lib/tests/scaffold/17-compare-gt.c
lib/tests/scaffold/17-compare-le.c
lib/tests/scaffold/17-compare-lt.c
lib/tests/scaffold/17-compare-unsigned-ge.c
lib/tests/scaffold/17-compare-unsigned-gt.c
lib/tests/scaffold/17-compare-unsigned-le.c
lib/tests/scaffold/17-compare-unsigned-lt.c
lib/tests/scaffold/17-compare-unsigned-char-le.c
lib/tests/scaffold/17-compare-unsigned-short-le.c
lib/tests/scaffold/17-compare-unsigned-long-le.c
lib/tests/scaffold/17-compare-and.c
lib/tests/scaffold/17-compare-or.c
lib/tests/scaffold/17-compare-and-or.c
lib/tests/scaffold/17-compare-assign.c
lib/tests/scaffold/17-compare-call.c
lib/tests/scaffold/18-assign-shadow.c
lib/tests/scaffold/20-while.c
lib/tests/scaffold/21-char-array-simple.c
lib/tests/scaffold/21-char-array.c
lib/tests/scaffold/22-while-char-array.c
lib/tests/scaffold/23-global-pointer-init-null.c
lib/tests/scaffold/23-global-pointer-init.c
lib/tests/scaffold/23-global-pointer-ref.c
lib/tests/scaffold/23-global-pointer-pointer-ref.c
lib/tests/scaffold/23-pointer-sub.c
lib/tests/scaffold/23-pointer.c
lib/tests/mes/30-oputs.c
lib/tests/mes/30-eputs.c
lib/tests/string/30-strlen.c
lib/tests/scaffold/30-exit-0.c
lib/tests/scaffold/30-exit-42.c
lib/tests/scaffold/32-call-wrap.c
lib/tests/scaffold/32-compare.c
lib/tests/scaffold/33-and-or.c
lib/tests/scaffold/34-pre-post.c
lib/tests/scaffold/35-compare-char.c
lib/tests/scaffold/36-compare-arithmetic.c
lib/tests/scaffold/37-compare-assign.c
lib/tests/scaffold/38-compare-call-2.c
lib/tests/scaffold/38-compare-call-3.c
lib/tests/scaffold/38-compare-call.c
lib/tests/scaffold/40-if-else.c
lib/tests/scaffold/41-ternary.c
lib/tests/scaffold/42-goto-label.c
lib/tests/scaffold/43-for-do-while.c
lib/tests/scaffold/44-switch.c
lib/tests/scaffold/44-switch-fallthrough.c
lib/tests/scaffold/44-switch-body-fallthrough.c
lib/tests/scaffold/45-void-call.c
lib/tests/scaffold/46-function-static.c
lib/tests/scaffold/47-function-expression.c
lib/tests/scaffold/48-global-static.c
lib/tests/assert/50-assert.c
lib/tests/mes/50-itoa.c
lib/tests/posix/50-getenv.c
lib/tests/stdlib/50-malloc.c
lib/tests/string/50-strcmp.c
lib/tests/string/50-strcpy.c
lib/tests/string/50-strncmp.c
lib/tests/posix/50-open-read.c
lib/tests/scaffold/51-pointer-sub.c
lib/tests/scaffold/54-argc.c
lib/tests/scaffold/54-argv.c
lib/tests/scaffold/55-char-array.c
lib/tests/scaffold/60-math.c
lib/tests/scaffold/61-array.c
lib/tests/scaffold/62-array.c
lib/tests/scaffold/63-struct.c
lib/tests/scaffold/63-struct-pointer.c
lib/tests/scaffold/63-struct-local.c
lib/tests/scaffold/63-struct-function.c
lib/tests/scaffold/63-struct-assign.c
lib/tests/scaffold/63-struct-array.c
lib/tests/scaffold/63-struct-array-assign.c
lib/tests/scaffold/63-struct-array-compare.c
lib/tests/scaffold/63-struct-cell.c
lib/tests/scaffold/64-make-cell.c
lib/tests/scaffold/65-read.c
lib/tests/scaffold/66-local-char-array.c
lib/tests/scaffold/70-stdarg.c
lib/tests/stdio/70-printf-hello.c
lib/tests/stdio/70-printf-simple.c
lib/tests/stdio/70-printf.c
lib/tests/stdlib/70-strtoull.c
lib/tests/string/70-strchr.c
lib/tests/scaffold/71-struct-array.c
lib/tests/scaffold/72-typedef-struct-def.c
lib/tests/scaffold/72-typedef-struct-def-local.c
lib/tests/scaffold/73-union-hello.c
lib/tests/scaffold/73-union.c
lib/tests/scaffold/74-multi-line-string.c
lib/tests/scaffold/75-struct-union.c
lib/tests/scaffold/76-pointer-arithmetic-pp.c
lib/tests/scaffold/76-pointer-arithmetic.c
lib/tests/scaffold/77-pointer-assign.c
lib/tests/scaffold/78-union-struct.c
lib/tests/scaffold/79-int-array-simple.c
lib/tests/scaffold/79-int-array.c
lib/tests/scaffold/7a-struct-char-array.c
lib/tests/scaffold/7b-struct-int-array-hello.c
lib/tests/scaffold/7b-struct-int-array-pointer.c
lib/tests/scaffold/7b-struct-int-array.c
lib/tests/scaffold/7c-dynarray.c
lib/tests/scaffold/7d-cast-char.c
lib/tests/scaffold/7e-struct-array-access.c
lib/tests/scaffold/7f-struct-pointer-arithmetic.c
lib/tests/scaffold/7g-struct-byte-word-field.c
lib/tests/scaffold/7h-struct-assign.c
lib/tests/scaffold/7i-struct-struct-simple.c
lib/tests/scaffold/7i-struct-struct.c
lib/tests/scaffold/7k-empty-for.c
lib/tests/scaffold/7k-for-each-elem-simple.c
lib/tests/scaffold/7k-for-each-elem.c
lib/tests/scaffold/7l-struct-any-size-array-simple.c
lib/tests/scaffold/7l-struct-any-size-array.c
lib/tests/scaffold/7m-struct-char-array-assign.c
lib/tests/scaffold/7n-struct-struct-array.c
lib/tests/scaffold/7o-struct-pre-post-simple.c
lib/tests/scaffold/7o-struct-pre-post.c
lib/tests/scaffold/7p-struct-cast.c
lib/tests/scaffold/7q-bit-field-simple.c
lib/tests/scaffold/7q-bit-field.c
lib/tests/scaffold/7r-sign-extend.c
lib/tests/scaffold/7s-struct-short.c
lib/tests/scaffold/7s-unsigned-compare.c
lib/tests/scaffold/7t-function-destruct.c
lib/tests/scaffold/7u-double.c
lib/tests/scaffold/7u-long-long.c
lib/tests/scaffold/7u-ternary-expression.c
lib/tests/scaffold/7u-call-ternary.c
lib/tests/scaffold/7u-inc-byte-word.c
lib/tests/scaffold/7u-struct-func.c
lib/tests/scaffold/7u-struct-size10.c
lib/tests/scaffold/7u-vstack.c
lib/tests/scaffold/70-array-in-struct-init.c
lib/tests/scaffold/70-struct-short-enum-init.c
lib/tests/scaffold/70-struct-post.c
lib/tests/scaffold/70-extern.c
lib/tests/setjmp/80-setjmp.c
lib/tests/stdio/80-sscanf.c
lib/tests/stdlib/80-qsort.c
lib/tests/stdlib/80-qsort-dupes.c
lib/tests/string/80-strncpy.c
lib/tests/string/80-strrchr.c
lib/tests/scaffold/82-define.c
lib/tests/scaffold/83-heterogenoous-init.c
lib/tests/scaffold/84-struct-field-list.c
lib/tests/scaffold/85-sizeof.c
"

if test -z "$bootstrap"; then
    TESTS="$TESTS
lib/tests/dirent/90-readdir.c
lib/tests/io/90-stat.c
lib/tests/mes/90-abtod.c
lib/tests/mes/90-dtoab.c
lib/tests/posix/90-execlp.c
lib/tests/posix/90-unsetenv.c
lib/tests/signal/90-signal.c
lib/tests/stdio/90-fopen.c
lib/tests/stdio/90-fopen-append.c
lib/tests/stdio/90-fread-fwrite.c
lib/tests/stdio/90-fseek.c
lib/tests/stdio/90-sprintf.c
lib/tests/stdlib/90-strtol.c
lib/tests/string/90-snprintf.c
lib/tests/string/90-strpbrk.c
lib/tests/string/90-strspn.c
lib/tests/scaffold/90-goto-var.c
lib/tests/scaffold/91-goto-array.c
lib/tests/scaffold/a0-call-trunc-char.c
lib/tests/scaffold/a0-call-trunc-short.c
lib/tests/scaffold/a0-call-trunc-int.c
lib/tests/scaffold/a0-math-divide-signed-negative.c
lib/tests/scaffold/a1-global-no-align.c
lib/tests/scaffold/a1-global-no-clobber.c
"
fi

XFAIL_TESTS="
lib/tests/stdio/90-sprintf.c
"

if test $compiler = mescc; then
    XFAIL_TESTS="$XFAIL_TESTS
lib/tests/scaffold/17-compare-unsigned-char-le.c
lib/tests/scaffold/17-compare-unsigned-short-le.c
lib/tests/scaffold/66-local-char-array.c
lib/tests/scaffold/72-typedef-struct-def-local.c
lib/tests/mes/90-abtod.c
lib/tests/mes/90-dtoab.c
lib/tests/scaffold/90-goto-var.c
lib/tests/scaffold/91-goto-array.c
"

    if test $mes_cpu = x86; then
        XFAIL_TESTS="$XFAIL_TESTS
"
    fi

    if test $mes_cpu = x86_64; then
        XFAIL_TESTS="$XFAIL_TESTS
lib/tests/scaffold/a0-call-trunc-int.c
"
    fi
fi

if test $mes_cpu = x86_64; then
    XFAIL_TESTS="$XFAIL_TESTS
lib/tests/stdio/70-printf-stdarg.c
"
fi

if test $compiler = gcc; then
    XFAIL_TESTS="$XFAIL_TESTS
"

    if test $mes_cpu = x86; then
        XFAIL_TESTS="$XFAIL_TESTS
lib/tests/mes/90-dtoab.c
"
    fi

    if test $mes_cpu = x86_64; then
        XFAIL_TESTS="$XFAIL_TESTS
lib/tests/stdio/70-printf-hello.c
lib/tests/stdio/70-printf-simple.c
lib/tests/stdio/70-printf.c
lib/tests/scaffold/70-extern.c
lib/tests/stdio/80-sscanf.c
lib/tests/posix/90-execlp.c
lib/tests/string/90-snprintf.c
"
    fi
fi

recheck=${recheck-false}
test_ext=.c
log_compiler="${SHELL} ${srcdest}build-aux/test-c.sh"
. ${srcdest}build-aux/test-suite.sh
