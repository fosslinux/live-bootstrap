.. sectnum:: :start: 0
.. SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
.. SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
.. SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
.. SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>
.. SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>

.. SPDX-License-Identifier: CC-BY-SA-4.0


bootstrap-seeds
===============

This is where it all begins. We start with the two raw binary seeds ``hex0-seed`` and ``kaem-optional-seed``.

First, we use those seeds to rebuild themselves.

Note that all early compilers before ``mes`` are part of `stage0-posix <https://github.com/oriansj/stage0-posix>`_.

A kernel boostrapping option is also available at the beginning. The ``hex0-seed`` can be used to compile the ``builder-hex0`` kernel which has its own built-in shell, ``hex0`` compiler and ``src`` tool to load files into its file system. ``builder-hex0`` runs stage0-posix and then builds ``mes`` and ``tcc``. It then builds and launches the `Fiwix <https://github.com/mikaku/Fiwix>` kernel which runs the build until Linux takes over.


hex0
====

``hex0`` is fairly trivial to implement and for each pair of hexadecimals characters it outputs a byte. We have also added two types of line comments (``#`` and ``;``) to create a well commented lines like:

.. code:: scheme

    # :loop_options [_start + 0x6F]
        39D3            ; cmp_ebx,edx                 # Check if we are done
        74 14           ; je !loop_options_done       # We are done
        83EB 02         ; sub_ebx, !2                 # --options

In the first steps we use initial ``hex0`` binary seed to rebuild ``kaem-optional`` and ``hex0`` from their source.

``hex0`` code is somewhat tedious to read and write as it is basically a well documented machine code. We have to manually calculate all jumps in the code.

``hex0`` can be approximated with: ``sed 's/[;#].*$//g' $input_file | xxd -r -p > $output_file``


builder-hex0 (kernel bootstrap)
===============================
If the kernel-bootstrap option is enabled then the ``builder-hex0`` kernel boots from a hard drive and loads an enormous shell script which embeds files (loaded with the ``src`` command) and the initial commands to build ``hex0-seed``, ``kaem-optional-seed``, and the command which launches stage0-posix using ``kaem-optional-seed`` and the stage0-posix launch script ``kaem.x86``. Builder-hex0 is written in hex0 and can be compiled with any one of ``hex0-seed``, ``sed``, the tiny ``builder-hex0-mini`` boot kernel or it can build itself.


kaem-optional
=============

``kaem-optional`` is a trivial shell that can read list of commands together with their command line arguments from a file and executes them. It also supports line comments but has no other features.

hex1
====

This is the last program that has to be written in ``hex0`` language. ``hex1`` is a simple extension of ``hex0`` and adds a single character labels and allows calculating 32-bit offsets from current position in the code to the label. ``hex1`` code might look like:

.. code:: scheme

    :a #:loop_options
        39D3            ; cmp_ebx,edx                 # Check if we are done
        0F84 %b         ; je %loop_options_done       # We are done
        83EB 02         ; sub_rbx, !2                 # --options

hex2
====

``hex2`` is our final hex language that adds support for labels of arbitrary length. It also allows accessing them via 8, 16, 32-bit relative addresses (``!``, ``@``, ``%``) and via 16-bit or 32-bit (``$``, ``&``) absolute addresses:

.. code:: scheme

    :loop_options
        39D3                  ; cmp_ebx,edx                        # Check if we are done
        74 !loop_options_done ; je8 !loop_options_done             # We are done
        83EB 02               ; sub_ebx, !2                        # --options

catm
====

``catm`` allows concatenating files with ``catm output_file input1 input2 ... inputN``. This allows us to distribute common code in separate files. We will first use it to append ELF header to ``.hex2`` files. Before this step the ELF header had to be included in the source file itself.

M0
==

The ``M0`` assembly language is the simplest assembly language you can create that enables the creation of more complicated programs. It includes only a single keyword: ``DEFINE`` and leverages the language properties of ``hex2`` along with extending the behavior to populate immediate values of various sizes and formats.

Thus ``M0`` code looks like:

.. code:: bash

    DEFINE cmp_ebx,edx 39D3
    DEFINE je 0F84
    DEFINE sub_ebx, 81EB

    :loop_options
        cmp_ebx,edx                         # Check if we are done
        je %loop_options_done               # We are done
        sub_ebx, %2                         # --options

cc_x86
======

The ``cc_x86`` implements a subset of the C language designed in ``M0`` assembly. It is a somewhat limited subset of C but complete enough to make it easy to write a more usable C compiler written in the C subset that ``cc_x86`` supports.

At this stage we start using `M2libc <https://github.com/oriansj/M2libc/>`_ as our C library. In fact, ``M2libc`` ships two versions of C library. There is a single-file library that contains just enough to build ``M2-Planet`` and there is a full version that is rather well-featured.

M2-Planet
=========

This is the only C program that we build with ``cc_x86``. `M2-Planet <https://github.com/oriansj/M2-Planet>`_ supports a larger subset of C than ``cc_x86`` and we are somewhat closer to C89 (it does not implement all C89 features but on the other hand it does have some C99 features). ``M2-Planet`` also includes a very basic preprocessor, so we can use stuff like ``#define``, ``#ifdef``.

``M2-Planet`` is also capable of using full ``M2libc`` C library that has more features and optimizations compared to bootstrap version of ``M2libc``.

``M2-Planet`` supports generating code for various architectures including ``x86``, ``amd64``, ``armv7``, ``aarch64``, ``riscv32`` and ``riscv64``. Up until this point bootstrap has been very architecture specific. From now on we still have platform specific bits of code but they are usually handled as conditionals in the same application rather than having completely different applications.

mescc-tools
===========

Now we build ``blood-elf`` used to generate debug info, C version of ``hex2`` (also called ``hex2``) and C version of ``M0`` called ``M1``. These are more capable than their platform specific hex counterparts and are fully cross-platform. Thus we can now have the whole toolchain written in C.

Then we rebuild ``mescc-tools`` again, so all our tools are using new toolchain written in C.

Finally, we build `kaem` which is a more capable version of `kaem-optional` and adds support for variables, environmental variables, conditionals and aliases. It also has various built-ins such as `cd` and `echo`.

M2-Mesoplanet
=============

``M2-Mesoplanet`` is a preprocessor that is more capable than ``M2-Planet`` and supports ``#include`` statements. It can also launch compiler, assembler and linker with the correct arguments, so we don't need to invoke them
manually.

At the moment it is only used to build ``mescc-tools-extra``.

M2-Planet
=========

We rebuild ``M2-Planet`` with ``M2-Planet``.

From here, we can move on from the lowest level stuff.

mescc-tools-extra
=================

``mescc-tools-extra`` contains some additional programs, namely filesystem
utilities ``cp`` and ``chown``. This allows us to have one unified
directory for our binaries. Furthermore, we also build ``sha256sum``, a
checksumming tool, that we use to ensure reproducibility and authenticity
of generated binaries. We also build initial ``untar``, ``ungz`` and ``unbz2``
utilities to deal with compressed archives.

``/sysa``
=========

We now move into the ``/sysa`` directory. As stage0-posix has no
concept of ``chdir()`` (not added until very late in stage0-posix),
we have to copy a lot of files into the root of the initramfs, making it
very messy. We get into the move ordered directory ``/sysa`` here,
copying over all of the required binaries from ``/``.

mes 0.24
========

GNU ``mes`` is a scheme interpreter. It runs the sister project ``mescc``,
which is a C compiler written in scheme, which links against the Mes C
Library. All 3 are included in this same repository. There are two stages
to this part:

1. Compiling an initial mes using ``M2-Planet``. Note that this is
   *only* the Mes interpreter, not the libc or anything else.
2. We then use this to recompile the Mes interpreter as well as building
   the libc. This second interpreter is faster and less buggy.

tinycc 0.9.26
=============

``tinycc`` is a minimal C compiler that aims to be small and fast. It
complies with all C89 and most of C99 standards.

First, we compile janneke’s fork of tcc 0.9.26 using ``mescc``,
containing 27 patches to make it operate well in the bootstrap
environment and make it compilable using ``mescc``. This is a
non-trivial process and as seen within tcc. kaem has many different parts
within it: a. tcc 0.9.26 is first compiled using ``mescc``. b. The mes
libc is recompiled using tcc (``mescc`` has a non-standard ``.a``
format), including some additions for later programs. c. tcc 0.9.26 is
recompiled 5(!) times to add new features that are required for other
features, namely ``long long`` and ``float``. Each time, the libc is
also recompiled.

tinycc 0.9.27
=============

Now, we compile upstream tcc 0.9.27, the latest release of tinycc, using
the final version of tcc 0.9.26.

From this point onwards, until further notice, all programs are compiled
using tinycc 0.9.27.

Note that now we begin to delve into the realm of old GNU software,
using older versions compilable by tinycc. Prior to this point, all tools
have been adapted significantly for the bootstrap; now, we will be using
old tooling instead.

Fiwix 1.4.0-lb1 (kernel bootstrap)
==================================

If the kernel bootstrap option is enabled then the Fiwix kernel is built next.
This is a Linux clone which is much simpler to understand and build than Linux.
This version of Fiwix is a fork of 1.4.0 that contains many modifications and
enhancements to support live-boostrap.

lwext4 1.0.0 (kernel bootstrap)
===============================

If the kernel bootstrap option is enabled then `lwext4 <https://github.com/gkostka/lwext4>`
is built next. This is a library for creating ext2/3/4 file systems from user land.
This is combined with a program called ``make_fiwix_initrd.c`` which creates
and populates an ext2 files system which Fiwix uses for an initial ram drive (initrd).
This file system contains all of the files necessary to build Linux.

kexec-fiwix (kernel bootstrap)
==============================

If the kernel bootstrap option is enabled then a C program `kexec-fiwix` is compiled
and run which places the Fiwix ram drive in memory and launches the Fiwix kernel.

make 3.82
=========

GNU ``make`` is now built so we have a more robust building system.
``make`` allows us to do things like define rules for files rather than
writing complex kaem scripts.

gzip 1.2.4
==========

``gzip`` is the most common compression format used for software source
code. It is more capable than ``ungz`` from ``stage0-posix`` and also supports
compression.

tar 1.12
========

We build GNU Tar 1.12, the last version compilable with mes libc.

sed 4.0.9
=========

You are most likely aware of GNU ``sed``, a line editor.

patch 2.5.9
===========

``patch`` is a very useful tool at this stage, allowing us to make
significantly more complex edits, including just changes to lines.

bzip2 1.0.8
===========

``bzip2`` is a compression format that compresses more than ``gzip``. It
is preferred where we can use it, and makes source code sizes smaller.

coreutils 5.0
=============

GNU Coreutils is a collection of widely used utilities such as ``cat``,
``chmod``, ``chown``, ``cp``, ``install``, ``ln``, ``ls``, ``mkdir``,
``mknod``, ``mv``, ``rm``, ``rmdir``, ``tee``, ``test``, ``true``, and
many others.

A few of the utilities cannot be easily compiled with Mes C library, so
we skip them.

The ``cp`` in this stage replaces the ``mescc-tools-extra`` ``cp``.

heirloom devtools
=================

``lex`` and ``yacc`` from the Heirloom project. The Heirloom project is
a collection of standard UNIX utilities derived from code by Caldera and
Sun. Differently from the analogous utilities from the GNU project, they
can be compiled with a simple ``Makefile``.

bash 2.05b
==========

GNU ``bash`` is the most well known shell and the most complex piece of
software so far. However, it comes with a number of great benefits over
kaem, including proper POSIX sh support, globbing, etc.

Bash ships with a bison pre-generated file here which we delete.
Unfortunately, we have not bootstrapped bison but fortunately for us,
heirloom yacc is able to cope here.

flex 2.5.11
===========

``flex`` is a tool for generating lexers or scanners: programs that
recognize lexical patters.

Unfortunately ``flex`` also depends on itself for compiling its own
scanner, so first flex 2.5.11 is compiled, with its scanner definition
manually modified so that it can be processed by lex from the Heirloom
project (the required modifications are mostly syntactical, plus a few
workarounds to avoid some flex advanced features).

tcc 0.9.27 (patched)
====================

We recompile ``tcc`` with some patches needed to build musl.

musl 1.1.24
===========

``musl`` is a C standard library that is lightweight, fast, simple,
free, and strives to be correct in the sense of standards-conformance
and safety. ``musl`` is used by some distributions of GNU/Linux as their
C library. Our previous Mes C library was incomplete which prevented us
from building many newer or more complex programs.

``tcc`` has slight problems when building and linking ``musl``, so we
apply a few patches. In particular, we replace all weak symbols with
strong symbols and will patch ``tcc`` in the next step to ignore
duplicate symbols.

tcc 0.9.27 (musl)
=================

We recompile ``tcc`` against musl. This is a two stage process. First we
build tcc-0.9.27 using tcc-0.9.26  that itself links to Mes C library but produces
binaries linked to musl. Then we recompile newly produced tcc with
itself. Interestingly, tcc-0.9.27 linked against musl is self hosting.

musl 1.1.24 (tcc-musl)
======================

We now rebuild ``musl`` with the just built ``tcc-musl``, which fixes a
number of bugs, particularly regarding floats, in the first ``musl``.

tcc 0.9.27 (musl v2)
====================

Now that we have a ‘fixed’ ``musl``, we now recompile ``tcc`` as ``tcc``
uses floats extensively.

sed 4.0.9
=========

``sed`` is rebuilt against musl.

bzip2 1.0.8
===========

``bzip2`` is rebuilt unpatched with the new tcc and musl fixing issues
with reading files from stdin that existed in the previous build.

m4 1.4.7
========

``m4`` is the first piece of software we need in the autotools suite,
flex 2.6.4 and bison. It allows macros to be defined and files to be
generated from those macros.

flex 2.6.4
==========

We recompile unpatched GNU ``flex`` using older flex 2.5.11. This is
again a two stage process, first compiling flex using ``scan.c`` (from
``scan.l``) created by old flex, then recompile ``scan.c`` using the new
version of flex to remove any buggy artifacts from the old flex.

bison 3.4.1
===========

GNU ``bison`` is a parser generator. With ``m4`` and ``flex`` we can now
bootstrap it following https://gitlab.com/giomasce/bison-bootstrap. It’s
a 3 stage process:

1. Build bison using a handwritten grammar parser in C.
2. Use bison from previous stage on a simplified bison grammar file.
3. Build bison using original grammar file.

Finally we have a fully functional ``bison`` executable.

grep 2.4
========

GNU ``grep`` is a pattern matching utility. Is is not immediately needed
but will be useful later for autotools.

diffutils 2.7
=============

``diffutils`` is useful for comparing two files. It is not immediately
needed but is required later for autotools.

coreutils 5.0
=============

``coreutils`` is rebuilt against musl. Additional utilities are built
including ``comm``, ``expr``, ``dd``, ``sort``, ``sync``, ``uname`` and
``uniq``. This fixes a variety of issues with existing ``coreutils``.

coreutils 6.10
==============
We build ``date``, ``mktemp`` and ``sha256sum`` from coreutils 6.10 which are
either missing or don't build correctly in 5.0. Other utils are not built at
this stage.

gawk 3.0.4
==========

``gawk`` is the GNU implementation of ``awk``, yet another pattern
matching and data extraction utility. It is also required for autotools.

perl 5.000
==========

Perl is a general purpose programming language that is especially
suitable for text processing. It is essential for autotools build system
because automake and some other tools are written in Perl.

Perl itself is written in C but ships with some pre-generated files that
need perl for processing, namely ``embed.h`` and ``keywords.h``. To
bootstrap Perl we will start with the oldest Perl 5 version which has
the fewest number of pregenerated files. We reimplement two remaining
perl scripts in awk and use our custom makefile instead of Perl’s
pre-generated Configure script.

At this first step we build ``miniperl`` which is ``perl`` without
support for loading modules.

perl 5.003
==========

We now use ``perl`` from the previous stage to recreate pre-generated
files that are shipped in perl 5.003. But for now we still need to use
handwritten makefile instead of ``./Configure`` script.

perl 5.004_05
=============

Yet another version of perl; the last version buildable with 5.003.

perl 5.005_03
=============

More perl! This is the last version buildable with 5.004. It also
introduces the new pregenerated files ``regnodes.h`` and
``byterun.{h,c}``.

perl 5.6.2
==========

Even more perl. 5.6.2 is the last version buildable with 5.005.

autoconf 2.52
=============

GNU Autoconf is a tool for producing ``configure`` scripts for building, installing and
packaging software on computer systems where a Bourne shell is available.

At this stage we still do not have a working autotools system, so we manually install
``autoconf`` script and replace a few placeholder variables with ``sed``.

Autoconf 2.52 is the newest version of ``autoconf`` that does not need ``perl``, and hence
a bit easier to install.

This is not a full featured autoconf install, it is missing other programs such as ``autoheader``
but is sufficient to build autoconf 2.53.

automake 1.6.3
==============

GNU Automake is a tool for automatically generating Makefile.in files. It is another major
part of GNU Autotools build system and consists of ``aclocal`` and ``automake`` scripts.

We bootstrap it using a 2 stage process:

1. Use ``sed`` to replace a few placeholder variables in ``aclocal.in`` script.
   Then we manually install ``aclocal`` script and its dependencies.
2. Patch ``configure.in`` to create ``automake`` file but skip ``Makefile.in`` processing.
   Again we manually install ``automake`` script and its dependencies.

autoconf 2.53
=============

We now start bootstrapping newer versions of autoconf. Version 2.53 now uses ``perl``.
In order to build it with ``autoconf-2.52`` we have to patch it a bit.

automake 1.7
============

Automake 1.7 and Autoconf 2.54 depend on each other, so we patch out two offending
autoconf macros to make it build with ``autoconf-2.53``.

autoconf 2.54
=============

Never version of ``autoconf``.

autoconf 2.55
=============

Even newer ``autoconf``. This is the last version of ``autoconf`` that is buildable
with ``automake-1.7``.

automake 1.7.8
==============

Newer ``automake``. This is the latest ``automake`` that is buildable with ``autoconf-2.55``.

autoconf 2.57
=============

Newer ``autoconf``. This time we were able to skip version 2.56.

autoconf 2.59
=============

Again, we managed to skip one version.

automake 1.8.5
==============

We need newer ``automake`` to proceed to newer ``autoconf`` versions. This is the latest
automake version from 1.8 release series.

help2man 1.36.4
===============

``help2man`` automatically generates manpages from programs ``--help`` and ``--version``
outputs. This is not strictly required for bootstrapping but will help us to avoid patching
build process to skip generation of manpages. This is the newest version of ``help2man`` that
does not require Perl 5.8.

autoconf 2.61
=============

Yet another version of ``autoconf``.

automake 1.9.6
==============

Latest GNU Automake from 1.9 series. Slightly annoyingly depends on itself but it is easy to patch
to make it buildable with 1.8.5.

automake 1.10.3
===============

GNU Automake from 1.10 series. ``aclocal`` is slightly patched to work
with our ``perl``.

autoconf 2.64
=============

Slightly newer version of GNU Autoconf. At this stage Autoconf is mostly
backwards compatible but newer versions need newer ``automake``.

automake 1.11.2
===============

GNU Automake from 1.11 series. This is not the latest point release as newer ones
need Autoconf 2.68. Newer major version of ``automake`` also depends on a newer ``bash``.

autoconf 2.69
=============

This is a much newer version of GNU Autoconf.

libtool 2.2.4
=============

GNU Libtool is the final part of GNU Autotools. It is a script used to hide away differences
when compiling shared libraries on different platforms.

bash 2.05b
==========

Up to this point, our build of ``bash`` could run scripts but could not be used
interactively. Rebuilding bash makes this functionality work.

automake 1.15.1
===============

GNU Automake from 1.15 series. This is the last version that runs on Perl 5.6.

binutils 2.30
=============

The GNU Binary Utilities, or binutils, are a set of programming tools for creating and
managing binary programs, object files, libraries, profile data, and assembly source code.

In particular we can now use full featured ``ar`` instead of ``tcc -ar``,
the GNU linker ``ld``, which allows us building shared libraries,
and the GNU assembler ``as``.

musl 1.1.24 (v3)
================

We rebuild musl for the third time. This time we use GNU ar rather than ``tcc -ar``,
so we can drop weak symbols patch. Also, we can use GNU as to build assembly source files,
so those assembly files that tcc failed to compile no longer have to be patched.

tcc 0.9.27 (musl v3)
====================

We rebuild tcc against new musl and without a patch to ignore duplicate symbols.

gcc 4.0.4
=========

The GNU Compiler Collection (GCC) is an optimizing compiler produced by the
GNU Project. GCC is a key component of the GNU toolchain and the standard
compiler for most projects related to GNU and the Linux kernel.

Only the C frontend is built at this stage.

At this stage we are not yet able to regenerate top-level ``Makefile.in``
which needs GNU Autogen and hence Guile. Luckily, building GCC without
top-level ``Makefile`` is fairly easy.

findutils 4.2.33
================

GNU Find Utilities can be used to search for files. We are mainly interested
in ``find`` and ``xargs`` that are often used in scripts.

musl 1.2.3
==========

GCC can build the latest as of the time of writing musl version.

We also don't need any of the TCC patches that we used before.
To accomodate Fiwix, there are patches to avoid syscalls set_thread_area and clone.

gcc 4.0.4
=========

Rebuild GCC with GCC and also against the latest musl.

util-linux 2.19.1
=================

``util-linux`` contains a number of general system administration utilities.
Most pressingly, we need these for being able to mount disks (for non-chroot
mode, but it is built it in chroot mode anyway because it will likely be useful
later). The latest version is not used because of autotools/GCC
incompatibilities.

kbd-1.15
========

``kbd`` contains ``loadkeys`` which is required for building the Linux kernel.
The 2.x series is not used because it requires particular features of autotools
that we do not have available.

make 3.82
=========

GNU ``make`` is now rebuilt properly using the build system and GCC, which means that
it does not randomly segfault while building the Linux kernel.

ed 1.4
======

``ed`` is a very basic line editor. This is the last version that is not distributed
in ``.tar.lz`` format. ``ed`` is used by ``bc`` build scripts.

bc 1.07.1
=========

``bc`` is a console based calculator that is sometime used in scripts. We need ``bc``
to rebuild some Linux kernel headers.

kexec-tools 2.0.22
==================

``kexec`` is a utility for the Linux kernel that allows the re-execution of the
Linux kernel without a manual restart from within a running system. It is a
kind of soft-restart. It is only built for non-chroot mode, as we only use it
in non-chroot mode. It is used to go into sysb/sysc.

create_sysb
===========

The next step is not a package, but the creation of the sysb rootfs, containing
all of the scripts for sysb (which merely move to sysc). Again, this is only
done in non-chroot mode, because sysb does not exist in chroot mode.

musl 1.2.3
==========
Prior to building and booting Linux, musl is rebuilt yet again with syscalls
``clone`` and ``set_thread_area`` enabled for Linux thread support.

Linux kernel 4.9.10
===================

A lot going on here. This is the first (and currently only) time the Linux kernel
is built. Firstly, Linux kernel version 4.9.x is used because newer versions
require much more stringent requirements on the make, GCC, binutils versions.
However, the docs are also wrong, as the latest of the 4.9.x series does not
work with our version of binutils. However, a much earlier 4.9.10 does
(selected arbitrarily, could go newer but did not test), with a small amount
of patching. This is also modern enough for most hardware and to cause few
problems with software built in sysc. Secondly, the linux-libre scripts are used
to deblob the kernel.  Every other pregenerated file is appended with ``_shipped``
so we use a ``find`` command to remove those, which are automatically regenerated.
The kernel config was originally taken from Void Linux, and was then modified
for the requirements of live-bootstrap, including compiler features, drivers,
and removing modules. Modules are unused. They are difficult to transfer to
subsequent systems, and we do not have ``modprobe``. Lastly,
the initramfs of sysb is generated in this stage, using ``gen_init_cpio`` within
the Linux kernel tree. This avoids the compilation of ``cpio`` as well.

go_sysb
=======

This is the last step of sysa, run for non-chroot mode. It uses kexec to load
the new Linux kernel into RAM and execute it, moving into sysb.

In chroot, sysb is skipped, and data is transferred directly to sysc and
chrooted into.

sysb
====

sysb is purely a transition to sysc, allowing binaries from sysa to get onto a
disk (as sysa does not necessarily have hard disk support in the kernel).
It populates device nodes, mounts sysc, copies over data, and executes sysc.

curl 7.88.1
===========

``curl`` is used to download files using various protocols including HTTP and HTTPS.
However, this first build does not support encrypted HTTPS yet.

bash 5.2.15
===========

This new version of ``bash`` compiles without any patches, provides new features,
and is built with GNU readline support so it can be used as a fully-featured
interactive shell. ``autoconf-2.69`` is used to regenerate the configure
script and ``bison`` is used to recreate some included generated files.

curl 7.83.0
===========

Curl is built in sysc because Linux must be running with support for threads.
Curl requires musl 1.2.3 with thread support which was built at the end of sysa.
Curl is built first in sysc so the rest of the packages can be downloaded.
Note that the tar file for curl itself was copied over from sysa because
curl is not yet available to download it.

xz 5.4.1
========

XZ Utils is a set of free software command-line lossless data compressors,
including lzma and xz. In most cases, xz achieves higher compression rates
than alternatives like gzip and bzip2.

file 5.44
=========

file is a utility that is used to get information about files based upon their
magic.

libtool 2.4.7
=============

A modern version of libtool with better compatibility with newer versions of GNU
Autotools.

tar 1.34
========

Newer tar has better support for decompressing .tar.bz2 and .tar.xz archives.
It also deals better with modern tar archives with extra metadata.

coreutils 8.32
==============

We build the latest available coreutils 8.32 which adds needed options to make
results of build metadata reproducible. For example, timestamps are changed with
``touch --no-dereference``.

pkg-config 0.29.2
=================

pkg-config is a helper tool that helps to insert compile and link time flags.

make 4.2.1
==========

A newer version of make built using autotools is much more reliable and is
compiled using a modern C compiler and C library. This removes a couple of
segfaults encountered later in the process and allows more modern make features
to be used. We do not go for the latest because of the use of automake 1.16
which we do not have yet.

gmp 6.2.1
=========

GNU Multiple Precision Arithmetic Library (GMP) is a free library for
arbitrary-precision arithmetic, operating on signed integers, rational numbers,
and floating-point numbers.

GMP is required by newer versions of GCC and Guile.

autoconf-archive 2021.02.19
===========================

The GNU Autoconf Archive is a collection of Autoconf macros that are used by
various projects and in particular GNU MPFR.

mpfr 4.1.0
==========

The GNU Multiple Precision Floating-Point Reliable Library (GNU MPFR) is a library
for arbitrary-precision binary floating-point computation with correct rounding,
based on GNU Multi-Precision Library.

mpc 3.2.1
=========

GNU MPC is a library for multiprecision complex arithmetic with exact rounding based
on GNU MPFR.

flex 2.5.33
===========

An older version of flex is required for bison 2.3. We cannot use 2.5.11 that
was compiled much earlier, as it does not produce reproducible output when
building bison 2.3.

bison 2.3
=========

This is an older version of bison required for the bison files in perl 5.10.1.
We backwards-bootstrap this from 3.4.1, using 3.4.1 to compile the bison files
in 2.3. This parser works sufficiently well for perl 5.10.5.

bison 3.4.2
===========

Bison 3.4.1 is buggy and segfaults when perl 5.32.1 is built. This is probably
because it was built with a hand-written makefile. We do not build the latest
bison because perl 5.32.1 requires bison <= 3.4.2.

perl 5.10.1
===========

Perl 5.10.1 is an intermediate version used before Perl 5.32. We require this
version as it adds a couple of modules into lib/ required to regenerate files in
Perl 5.32. We still use the Makefile instead of the metaconfig strategy, as
metaconfig history becomes poor more than a few years back.

dist 3.5-236
============

dist is perl's package used for generating Perl's Configure (which is written in
Perl itself). We 'compile' (aka generate) metaconfig and manifake only from dist.
We do not use dist's build system because it itself uses dist.

perl 5.32.1
===========

We finally compile a full version of Perl using Configure. This includes all base
extensions required and is the latest version of Perl. We are now basically able
to run any Perl application we want.

libarchive 3.5.2
================

``libarchive`` is a C library used to read and write archives.

openssl 1.1.1l
==============

OpenSSL is a C library for secure communications/cryptography.

curl 7.88.1
===========

We rebuild curl with support for OpenSSL.

zlib 1.2.13
===========

zlib is a software library used for data compression and implements an abstraction of
DEFLATE algorithm that is also used in ``gzip``.

automake 1.16.3
===============

GNU Automake from 1.16 series that required newer Perl.

autoconf 2.71
=============

GNU Autoconf 2.71 is even newer version of autoconf. It does not build with miniperl,
so we postponed it until full perl was built.

patch 2.7.6
===========

Our old patch was built with manual makefile and used mes libc.
This is a newer version which we need in order to import gnulib into gettext.

gettext 0.21
============

GNU Gettext is an internationalization and localization system used for writing
multilingual programs.

texinfo 6.7
===========

Texinfo is a typesetting syntax used for generating documentation. We can now use
``makeinfo`` script to convert ``.texi`` files into ``.info`` documentation format.

gcc 4.7.4
=========

GCC 4.7.4 is the last version written in C. This time we build both C and C++ backends.
The C++ backend has a dependency on ``gperf``, which is written in C++. Fortunately, it is
easy to patch it out; the resulting ``g++`` compiler is capable of building ``gperf``.
We also add in two patchsets to the compiler;

* one to add support for musl shared library support
* one providing a few compiler flags/features that are required later to build GCC 10

binutils 2.38
=============

This version of binutils provides a more comprehensive set of programming tools for
creating and managing binary programs. It also includes modern versions of the ``ld``
linker, the ``as`` assembler and the ``ar`` program.

gperf 3.1
=========

``gperf`` is a perfect hash function generator (hash function is injective).

libunistring 0.9.10
===================

Library for manipulating Unicode and C strings according to Unicode standard. This
is a dependency of GNU Guile.

libffi 3.3
==========

The libffi library provides a portable, high level programming interface to various
calling conventions.

libatomic_ops 7.6.10
====================

``libatomic_ops`` provides semi-portable access to hardware-provided atomic memory
update operations on a number of architectures.

boehm-gc 8.0.4
==============

The Boehm-Demers-Weiser conservative garbage collector can be used as a garbage
collecting replacement for C malloc or C++ new.

guile 3.0.7
===========

GNU Ubiquitous Intelligent Language for Extensions (GNU Guile) is the preferred
extension language system for the GNU Project and features an implementation
of the programming language Scheme.

We use ``guile-psyntax-bootstrapping`` project to bootstrap Guile's ``psyntax.pp``
without relying on pre-expanded code.

which 2.21
==========

``which`` shows the full path of (shell) commands. It mostly duplicates
bash built-in ``command -v`` but some scripts call ``which`` instead.
In particular, ``autogen`` scripts use it.

grep 3.7
========

Newer ``grep`` will be needed to bootstrap ``autogen``.

sed 4.8
=======

Earlier ``sed`` was built with manual makefile with most features compiled out.
Build a newer ``sed`` using GNU Autotools build system. In particular this will let
sed keep executable bit on after in place editing.

autogen 5.18.16
===============

GNU Autogen is a tool designed to simplify the creation and maintenance of
programs that contain large amounts of repetitious text. Unfortunately, the
source is full of pregenerated files that require ``autogen`` to rebuild.

We use the `gnu-autogen-bootstrapping <https://github.com/schierlm/gnu-autogen-bootstrapping>`_
project to rebuild those and create (slightly crippled) ``autogen`` that
is then able to build a full-featured version.

musl 1.2.3
==========

With GCC and binutils supporting a musl-based toolchain natively, musl itself is rebuilt
with support for dynamic linking.

python 2.0.1
============

Everything is in place to bootstrap the useful programming language/utility
Python. While Python is largely written in C, many parts of the codebase are
generated from Python scripts, which only increases as Python matured over time.

We begin with Python 2.0.1, which has minimal generated code, most of which can
be removed. Lib/{keyword,token,symbol} scripts are rewritten in C and used to
regenerate parts of the standard library. Unicode support and sre (regex)
support is stripped out. 

Using the stripped-down first version of Python 2.0.1, Python 2.0.1 is rebuilt,
including Unicode and regex support (required for future Python builds). The
first version is insufficient to run the Lib/{keyword,token,symbol} scripts, so
those continue to use the C versions.

Precompiled Python code at this point is highly unreproducible, so it is
deleted (JIT compiled instead). This makes Python itself slower, but this is of
little consequence.

python 2.3.7
============

Python 2.0.1 is sufficient to build Python 2.3.7.

Differences to 2.0.1:

* The new "ast" module, performing parsing of Python, is generated from a
  parsing specification using Python code.
* 2.0.1 is insufficient to run 2.3.7's unicode regeneration, so Unicode
  support is again stripped out.

Python 2.3.7 is then rebuilt to include Unicode support.

python 2.5.6
============

Python 2.3.7 is sufficient to build Python 2.5.6, with a few minimal changes to
language constructs in scripts. This is the last 2.x version we build.

Differences to 2.3.7 are very minimal.

python 3.1.5
============

Python 2.5.6 is new enough to be able to build Python 3.1.5, allowing us to move
into the modern 3.x series of Python. Various patching is required, as some
scripts in the tree are still Python 2 while others are Python 3. We have to
convert the Python 3 ones back to Python 2 to be able to use Python 2.5.6.

Differences to 2.5.6:

* An include cycle when a distributed file is removed arises, we have to jump
  through some hoops to make this work.
* At the second pass of building, various charset encodings can be regenerated &
  used in the standard library (required in future Python 3.x).
* The new ssl Python library is disabled due to our OpenSSL version being too
  new.

Python 3.1.5 is rebuilt, using Python 3 for the Python 3 scripts in the tree.

python 3.3.7
============

Python 3.1.5 is sufficient to build Python 3.3.7 (rapid language change = small
jumps).

Differences to 3.1.5:

* The ssl Python library can now be re-enabled, and ``_ssl_data.h`` regenerated.

python 3.4.10
=============

Python 3.3.7 is sufficient to build Python 3.4.10.

Differences to 3.3.7:

* The clinic tool has been introduced, which unifies documentation with code.
  Clinic creates many generated files. We run the clinic tool across all files
  using clinic.
* The ssl library breaks in much more ugly ways than before, but unlike previous
  versions, it passes over this error silently.

python 3.8.16
=============

Python 3.4.10 is sufficient to build Python 3.8.16.

Differences to 3.4.10:

* The build system has been significantly revamped (coming in line with modern
  standards).
* Many of our previous regenerations can be replaced with one ``make regen-all``
  invocation.
* The stringprep Python module, previously deleted, is now required, so it is
  regenerated.

python 3.11.1
=============

The newest version of Python, Python 3.11.1 can now be built.

Differences to 3.8.16:

* Unfortunately, the build system has regressed slightly. We must choose the
  order to perform regenerations in the Makefile ourselves, as some
  regenerations use other regenerations, but the Makefile does not include them
  as dependencies.
* The concept of "frozen" modules has been introduced, adding a layer of
  complexity to regeneration.
* ``stdlib_module_names.h`` is a new file that must be built using data from a
  current Python binary. To achieve this, a dummy ``stdlib_module_names.h`` is used
  for the build, then ``stdlib_module_names.h`` is created, and Python is
  rebuilt using the proper ``stdlib_module_names.h``. Unfortunately this
  greatly increases the time taken to build Python, but it is not trivial to
  work around.
* A new generated script ``Lib/re/_casefix.py`` is introduced.
* The ssl module, now unbroken, can be built again.
* Very recent Python versions allow for the use of ``SOURCE_DATE_EPOCH`` to
  remove determinism from precompiled Python libraries (``.pyc``). Finally, we
  can re-enable compiling of Python modules.

GCC 10.4.0
==========

GCC 10.x series is the last version of GCC that is able to be built with the
C/C++ standards available in GCC 4.7. Instead of manually configuring & compiling
every subdirectory, since we now have ``autogen`` available we are able to use
the top-level configure to build the project. We do not use GCC's bootstrap mode,
where GCC is recompiled with itself after being built, since we're just going
to use this GCC to compile GCC 12, it adds build time for little benefit.
