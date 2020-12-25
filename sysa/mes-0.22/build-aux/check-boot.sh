#! /bin/bash

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

scaffold/boot/00-zero.scm
scaffold/boot/01-true.scm
scaffold/boot/02-symbol.scm
scaffold/boot/03-string.scm
scaffold/boot/04-quote.scm
scaffold/boot/05-list.scm
scaffold/boot/06-tick.scm
scaffold/boot/07-if.scm
scaffold/boot/08-if-if.scm

scaffold/boot/10-cons.scm
scaffold/boot/11-list.scm
scaffold/boot/11-vector.scm
scaffold/boot/12-car.scm
scaffold/boot/13-cdr.scm
scaffold/boot/14-exit.scm
scaffold/boot/15-display.scm

scaffold/boot/16-if-eq-quote.scm

scaffold/boot/17-memq.scm
scaffold/boot/17-memq-keyword.scm
scaffold/boot/17-string-equal.scm
scaffold/boot/17-equal2.scm
scaffold/boot/17-string-append.scm
scaffold/boot/17-open-input-string.scm

scaffold/boot/20-define.scm
scaffold/boot/20-define-quoted.scm
scaffold/boot/20-define-quote.scm

scaffold/boot/21-define-procedure.scm
scaffold/boot/22-define-procedure-2.scm
scaffold/boot/23-begin.scm
scaffold/boot/24-begin-define.scm
scaffold/boot/25-begin-define-2.scm
scaffold/boot/26-begin-define-later.scm
scaffold/boot/27-lambda-define.scm
scaffold/boot/28-define-define.scm
scaffold/boot/29-lambda-define.scm
scaffold/boot/2a-lambda-lambda.scm
scaffold/boot/2b-define-lambda.scm
scaffold/boot/2c-define-lambda-recurse.scm
scaffold/boot/2d-define-lambda-set.scm
scaffold/boot/2d-compose.scm
scaffold/boot/2e-define-first.scm
scaffold/boot/2f-define-second.scm
scaffold/boot/2f-define-second-lambda.scm
scaffold/boot/2g-vector.scm

scaffold/boot/30-capture.scm
scaffold/boot/31-capture-define.scm
scaffold/boot/32-capture-modify-close.scm
scaffold/boot/33-procedure-override-close.scm
scaffold/boot/34-cdr-override-close.scm
scaffold/boot/35-closure-modify.scm
scaffold/boot/36-closure-override.scm
scaffold/boot/37-closure-lambda.scm
scaffold/boot/38-simple-format.scm
scaffold/boot/39-global-define-override.scm
scaffold/boot/3a-global-define-lambda-override.scm

scaffold/boot/40-define-macro.scm
scaffold/boot/41-when.scm
scaffold/boot/42-if-when.scm
scaffold/boot/43-or.scm
scaffold/boot/44-or-if.scm
scaffold/boot/45-pass-if.scm
scaffold/boot/46-report.scm
scaffold/boot/47-pass-if-eq.scm
scaffold/boot/48-let.scm
scaffold/boot/49-macro-override.scm
scaffold/boot/4a-define-macro-define-macro.scm
scaffold/boot/4b-define-macro-define.scm
scaffold/boot/4c-quasiquote.scm
scaffold/boot/4d-let-map.scm
scaffold/boot/4e-let-global.scm
scaffold/boot/4f-string-split.scm

scaffold/boot/50-keyword.scm
scaffold/boot/50-make-string.scm
scaffold/boot/50-string-join.scm
scaffold/boot/50-primitive-load.scm
scaffold/boot/53-closure-display.scm
scaffold/boot/60-let-syntax.scm
scaffold/boot/call-cc.scm
scaffold/boot/memory.scm
scaffold/boot/numbers.scm
"

XFAIL_TESTS=

test_ext=.scm
log_compiler="${SHELL} ${srcdest}build-aux/test-boot.sh"
. ${srcdest}build-aux/test-suite.sh
