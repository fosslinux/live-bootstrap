# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove unused pregenerated files
    rm libsanitizer/include/sanitizer/netbsd_syscall_hooks.h \
        libsanitizer/sanitizer_common/sanitizer_syscalls_netbsd.inc
    rm -r libgfortran/generated
    rm gcc/testsuite/go.test/test/bench/go1/jsondata_test.go \
        gcc/testsuite/go.test/test/bench/go1/parserdata_test.go \
        gcc/testsuite/go.test/test/cmplxdivide1.go \
        gcc/testsuite/go.test/test/fixedbugs/issue6866.go
    rm gcc/testsuite/gcc.target/x86_64/abi/test_3_element_struct_and_unions.c \
        gcc/testsuite/gcc.target/x86_64/abi/test_basic_returning.c \
        gcc/testsuite/gcc.target/x86_64/abi/test_passing_floats.c \
        gcc/testsuite/gcc.target/x86_64/abi/test_passing_integers.c \
        gcc/testsuite/gcc.target/x86_64/abi/avx512fp16/test_passing_floats.c \
        gcc/testsuite/gcc.target/x86_64/abi/avx512fp16/test_basic_returning.c \
        gcc/testsuite/gcc.target/x86_64/abi/avx512fp16/test_3_element_struct_and_unions.c \
        gcc/testsuite/gcc.target/x86_64/abi/bf16/test_passing_floats.c \
        gcc/testsuite/gcc.target/x86_64/abi/bf16/test_3_element_struct_and_unions.c

    rm gcc/testsuite/gm2/projects/pim/run/pass/tower/advflex.c \
        gcc/testsuite/gm2/projects/pim/run/pass/tower/AdvParse.mod
    rm gcc/testsuite/gcc.dg/analyzer/flex*.c
    rm -r gcc/testsuite/gdc.test/compilable
    rm gcc/config/rs6000/rs6000-tables.opt \
        gcc/config/rs6000/fusion.md \
        gcc/config/h8300/mova.md \
        gcc/config/aarch64/aarch64-tune.md \
        gcc/config/nios2/ldstwm.md \
        gcc/config/riscv/t-elf-multilib \
        gcc/config/riscv/t-linux-multilib \
        gcc/config/arm/arm-tune.md \
        gcc/config/arm/arm-tables.opt \
        gcc/config/arm/ldmstm.md \
        gcc/config/arc/t-multilib \
        gcc/config/m68k/m68k-tables.opt \
        gcc/config/c6x/c6x-mult.md \
        gcc/config/c6x/c6x-tables.opt \
        gcc/config/c6x/c6x-sched.md \
        gcc/config/csky/csky_tables.opt \
        gcc/config/mips/mips-tables.opt \
        gcc/config/nvptx/nvptx-gen.opt \
        gcc/config/nvptx/nvptx-gen.h \
        gcc/config/loongarch/loongarch.opt \
        gcc/config/loongarch/loongarch-str.h
    rm libphobos/src/std/internal/unicode_tables.d \
        libphobos/src/std/internal/unicode_decomp.d \
        libphobos/src/std/internal/unicode_grapheme.d \
        libphobos/src/std/internal/unicode_norm.d
    rm libgo/go/math/bits/example_test.go \
        libgo/go/math/bits/bits_tables.go \
        libgo/go/math/big/accuracy_string.go \
        libgo/go/math/big/roundingmode_string.go \
        libgo/go/strconv/isprint.go \
        libgo/go/strconv/eisel_lemire.go \
        libgo/go/sort/zfuncversion.go \
        libgo/go/golang.org/x/net/route/zsys_*.go \
        libgo/go/golang.org/x/net/idna/*.go \
        libgo/go/golang.org/x/text/unicode/bidi/t*.go \
        libgo/go/golang.org/x/text/unicode/norm/tables*.go \
        libgo/go/golang.org/x/tools/internal/typeparams/typeterm.go \
        libgo/go/golang.org/x/tools/internal/typeparams/termlist.go \
        libgo/go/golang.org/x/crypto/curve25519/internal/field/fe_amd64.go \
        libgo/go/internal/syscall/windows/registry/zsyscall_windows.go \
        libgo/go/internal/syscall/windows/zsyscall_windows.go \
        libgo/go/encoding/gob/*_helpers.go \
        libgo/go/index/suffixarray/sais2.go \
        libgo/go/net/http/*_bundle.go \
        libgo/go/runtime/sizeclasses.go \
        libgo/go/runtime/fastlog2table.go \
        libgo/go/html/template/*_string.go \
        libgo/go/crypto/md5/md5block.go \
        libgo/go/crypto/tls/common_string.go \
        libgo/go/crypto/elliptic/internal/fiat/p*.go \
        libgo/go/crypto/ed25519/internal/edwards25519/field/fe_amd64.go \
        libgo/go/time/zoneinfo_abbrs_windows.go \
        libgo/go/unicode/tables.go \
        libgo/go/regexp/syntax/doc.go \
        libgo/go/regexp/syntax/op_string.go \
        libgo/go/regexp/syntax/perl_groups.go \
        libgo/go/image/internal/imageutil/impl.go \
        libgo/go/image/color/palette/palette.go \
        libgo/go/cmd/internal/objabi/*_string.go \
        libgo/go/cmd/go/internal/test/flagdefs.go \
        libgo/go/debug/dwarf/*_string.go \
        libgo/go/debug/macho/reloctype_string.go \
        libgo/go/internal/goexperiment/exp_*.go \
        libgo/go/time/tzdata/zipdata.go \
        libgo/go/go/constant/kind_string.go
    rm libgo/go/compress/bzip2/testdata/*.bin \
        libgo/go/go/internal/gccgoimporter/testdata/v1reflect.gox \
        libgo/go/go/internal/gccgoimporter/testdata/time.gox \
        libgo/go/go/internal/gccgoimporter/testdata/unicode.gox \
        libgo/go/go/internal/gccgoimporter/testdata/escapeinfo.gox \
        libgo/go/go/internal/gccgoimporter/testdata/libimportsar.a \
        libgo/go/go/internal/gcimporter/testdata/versions/*.a
    rm -r libgo/go/compress/flate/testdata \
        libgo/go/runtime/pprof/testdata \
        libgo/go/debug/*/testdata \
        libgo/go/internal/trace/testdata \
        libgo/go/time/testdata \
        libgo/go/internal/xcoff/testdata
    find fixincludes/tests -name "*.h" -delete
    rm gcc/m2/mc/mcp*.bnf
    rm -r gcc/m2/pge-boot \
        gcc/m2/mc-boot
    # Partially generated unused files
    rm libgcc/config/sh/lib1funcs.S \
        libgcc/config/sh/lib1funcs-4-300.S \
        libgcc/config/arc/lib1funcs.S

    # Remove vendored zlib
    rm -r zlib/

    # gperf files
    rm gcc/cp/cfns.h gcc/cp/std-name-hint.h
    # Generate it now, because gcc doesn't regenerate it for some reason
    # (taken directly from gcc/cp/Make-lang.in)
    gperf -o -C -E -k '1-6,$' -j1 -D -N 'libc_name_p' -L C++ \
        gcc/cp/cfns.gperf --output-file gcc/cp/cfns.h

    # Regenerate autogen stuff
    rm Makefile.in fixincludes/fixincl.x
    autogen Makefile.def
    pushd fixincludes
    ./genfixes
    popd

    # Regenerate autotools
    # configure
    find . -name configure | sed 's:/configure::' | while read d; do
        pushd "${d}"
        AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fiv
        popd
    done
    # A odd script
    pushd gcc/m2/gm2-libs
    autoconf-2.69 -f config-host.in > config-host
    popd
    # Because GCC is stupid, copy depcomp back in
    cp "${PREFIX}/share/automake-1.15/depcomp" .
    # Makefile.in only
    local BACK="${PWD}"
    find . -type d \
        -exec test -e "{}/Makefile.am" -a ! -e "{}/configure" \; \
        -print | while read d; do
        d="$(readlink -f "${d}")"
        cd "${d}"
        # Find the appropriate configure script for automake
        while [ ! -e configure ]; do
            cd ..
        done
        automake-1.15 -fai "${d}/Makefile"
        cd "${BACK}"
    done

    # Remove bison generated files
    rm intl/plural.c

    # Remove flex generated files
    rm gcc/gengtype-lex.cc

    # intl/ Makefile is a bit broken because of new gettext
    sed -i 's/@USE_INCLUDED_LIBINTL@/no/' intl/Makefile.in

    # Regenerate crc table in libiberty/crc32.c
    pushd libiberty
    sed -n '/^   #include <stdio.h>/,/^   \}$/p' crc32.c > crcgen.c
    gcc -o crcgen crcgen.c
    sed '/crc_v3\.txt/{n; q}' crc32.c > crc32.c.new
    ./crcgen >> crc32.c.new
    sed '1,/^};$/d' crc32.c >> crc32.c.new
    mv crc32.c.new crc32.c
    popd

    # Regenerate decDPD.h
    rm libdecnumber/decDPD.h
    gcc -std=c99 -o decDPD_generate decDPD_generate.c
    cp decDPD.h.preamble libdecnumber/decDPD.h
    ./decDPD_generate >> libdecnumber/decDPD.h

    # Remove docs/translation
    find . -name "*.gmo" -delete
    find . -name "*.info" -delete
    find . -type f -name '*.[1-9]' -delete
    rm libiberty/functions.texi
    # Sphinx-generated
    rm gcc/jit/docs/conf.py
    rm gcc/jit/docs/_build/texinfo/libgccjit.texi \
        gcc/ada/gnat_rm.texi \
        gcc/ada/gnat_ugn.texi

    rm gcc/doc/avr-mmcu.texi
    gcc -o gen-avr-mmcu-texi gcc/config/avr/gen-avr-mmcu-texi.cc
    ./gen-avr-mmcu-texi > gcc/doc/avr-mmcu.texi
}

src_configure() {
    mkdir build
    cd build

    LDFLAGS="-static" \
    ../configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --build="${TARGET}" \
        --target="${TARGET}" \
        --host="${TARGET}" \
        --enable-bootstrap \
        --enable-static \
        --disable-plugins \
        --disable-libssp \
        --disable-libsanitizer \
        --program-transform-name= \
        --enable-languages=c,c++ \
        --with-system-zlib \
        --disable-multilib \
        --enable-threads=posix
}

src_compile() {
    make "${MAKEJOBS}" BOOT_LDFLAGS="-static"
}
