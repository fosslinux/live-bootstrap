# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
    argsum = 0

    argsum = or(argsum,   1 * (index($4, "m") != 0))
    argsum = or(argsum,   2 * (index($4, "f") != 0))
    argsum = or(argsum,   4 * (index($4, "s") != 0))
    argsum = or(argsum,   8 * (index($4, "t") != 0))
    argsum = or(argsum,  16 * (index($4, "i") != 0))
    argsum = or(argsum,  32 * (index($4, "I") != 0))
    argsum = or(argsum,  64 * (index($4, "d") != 0))
    argsum = or(argsum, 128 * (index($4, "u") != 0))

    mul = 256

    arg_val="SLAHCFR"

    argstr=$5
    gsub(" ", "", argstr)
    split(argstr, args, "")
    for(i=1; i<=length(argstr); i+=1) {
        argnum = (index(argstr, "?") != 0) * 8
        argnum += index(arg_val, args[i])
        argsum += argnum * mul
        mul = lshift(mul, 4)
    }
    printf("\t0x%08x,\t/* %s */\n", argsum, $1)
}
