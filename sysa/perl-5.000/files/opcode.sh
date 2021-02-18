#!/bin/sh -e
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# This file replaces opcode.pl

sed -e '1,/__END__/ d; s/[#].*$//g; /^$/d' opcode.pl | tr -s '\t' '\t' > data

exec 1> opcode.h

# Emit defines.
echo "typedef enum {";
awk '{print "\tOP_"toupper($1)","}' data
echo "	OP_max"
echo "} opcode;"
echo "#define MAXO " $(wc -l data | awk '{print $1}')

# Emit opnames.
printf "
#ifndef DOINIT
EXT char *op_name[];
#else
EXT char *op_name[] = {
"
awk -F'\t' '{print "\t\""$2"\","}' data
printf "};
#endif

"

# Emit function declarations.
awk -F'\t' '{print "OP *\t"$3"\t_((OP* op));"}' data | sort | uniq
awk '{print "OP *\tpp_"$1"\t_((void));"}' data

# Emit ppcode switch array.
printf "
#ifndef DOINIT
EXT OP * (*ppaddr[])();
#else
EXT OP * (*ppaddr[])() = {
"
awk '{print "\tpp_"$1","}' data
printf "};
#endif
"

# Emit check routines.
printf "
#ifndef DOINIT
EXT OP * (*check[])();
#else
EXT OP * (*check[])() = {
"
awk -F'\t' '{print "\t"$3",\t/* "$1" */"}' data
printf "};
#endif
"

# Emit allowed argument types.
printf "
#ifndef DOINIT
EXT U32 opargs[];
#else
EXT U32 opargs[] = {
"
awk -F'\t' -f opcode.awk data
printf "};
#endif
"

rm data
