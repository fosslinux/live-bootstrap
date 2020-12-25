#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2011-2018 Free Software Foundation, Inc.
# Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

# Adapted from GNU Automake

TEST_SUITE_LOG=test-suite.log
dry_run=false
create_global_log=cat
subdir=$(basename $(pwd))

TEST_LOGS=
for t in $TESTS; do
    b=$(dirname $t)/$(basename $t $test_ext)
    TEST_LOGS="$TEST_LOGS $b.log"
    if test -e $b.log \
            && test -e $b.trs\
            && ! $recheck; then
        echo `grep :test-result $b.trs | cut -d' ' -f 2`: $b
        continue
    fi
    d=$(dirname $t)
    case " `echo $XFAIL_TESTS` " in
        *[\ \	]$t[\ \	]*)
            fail=yes;;
        *)
            fail=no;;
    esac
    mkdir -p $d
    ${SHELL} ${srcdest}build-aux/test-driver\
       --test-name $t \
       --log-file $b.log\
       --trs-file $b.trs\
       --color-tests $colors\
       --enable-hard-errors no\
       --expect-failure $fail\
       -- $log_compiler\
       ${srcdest}$t
done

if test $colors = yes; then
    red='[0;31m'
    grn='[0;32m'
    lgn='[1;32m'
    blu='[1;34m'
    mgn='[0;35m'
    brg='[1m'
    std='[m'
else
    mgn= red= grn= lgn= blu= brg= std=
fi
bases="$TEST_LOGS"
bases=`for i in $bases; do echo $i; done | sed 's/\.log$//'`
bases=`echo $bases`
ws='[ 	]'
results=`for b in $bases; do echo $b.trs; done`
test -n "$results" || results=/dev/null
all=`  grep "^$ws*:test-result:"           $results | wc -l`
pass=` grep "^$ws*:test-result:$ws*PASS"  $results | wc -l`
fail=` grep "^$ws*:test-result:$ws*FAIL"  $results | wc -l`
skip=` grep "^$ws*:test-result:$ws*SKIP"  $results | wc -l`
xfail=`grep "^$ws*:test-result:$ws*XFAIL" $results | wc -l`
xpass=`grep "^$ws*:test-result:$ws*XPASS" $results | wc -l`
error=`grep "^$ws*:test-result:$ws*ERROR" $results | wc -l`
if test `expr $fail + $xpass + $error` -eq 0; then
  success=true
else
  success=false
fi
br='==================='; br=$br$br$br$br
result_count ()
{
    if test x"$1" = x"--maybe-color"; then
      maybe_colorize=yes
    elif test x"$1" = x"--no-color"; then
      maybe_colorize=no
    else
      echo "$@: invalid 'result_count' usage" >&2; exit 4
    fi
    shift
    desc=$1 count=$2
    if test $maybe_colorize = yes && test $count -gt 0; then
      color_start=$3 color_end=$std
    else
      color_start= color_end=
    fi
    echo "${color_start}# $desc $count${color_end}"
}
create_testsuite_report ()
{
  result_count $1 "TOTAL:" $all   "$brg"
  result_count $1 "PASS: " $pass  "$grn"
  result_count $1 "SKIP: " $skip  "$blu"
  result_count $1 "XFAIL:" $xfail "$lgn"
  result_count $1 "FAIL: " $fail  "$red"
  result_count $1 "XPASS:" $xpass "$red"
  result_count $1 "ERROR:" $error "$mgn"
}
{
  echo "${PACKAGE}: ${subdir}/${TEST_SUITE_LOG}"
  create_testsuite_report --no-color
  echo
  echo
  for b in $bases; do echo $b; done\
    | $create_global_log
} >${TEST_SUITE_LOG}.tmp || exit 1
mv ${TEST_SUITE_LOG}.tmp ${TEST_SUITE_LOG}
if $success; then
  col="$grn"
 else
  col="$red"
  test x"$V" = x0 || cat ${TEST_SUITE_LOG}
fi
echo "${col}$br${std}"
echo "${col}Testsuite summary for ${PACKAGE_NAME}${std}"
echo "${col}$br${std}"
create_testsuite_report --maybe-color
echo "$col$br$std"
if $success; then :; else
  echo "${col}See ${subdir}/${TEST_SUITE_LOG}${std}"
  if test -n "${PACKAGE_BUGREPORT}"; then
    echo "${col}Please report to ${PACKAGE_BUGREPORT}${std}"
  fi
  echo "$col$br$std"
fi
$success || exit 1
