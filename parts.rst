.. sectnum::
.. SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
.. SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
.. SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

.. SPDX-License-Identifier: CC-BY-SA-4.0

mescc-tools-seed
================

This is where all the magic begins. We start with our hex0 and kaem
seeds and bootstrap our way up to M2-Planet, a subset of C, and mes-m2,
an independent port of GNU Mes to M2-Planet. The following steps are
taken here:

-  hex0 (seed)
-  hex0 compiles hex1
-  hex0 compiles catm
-  hex1 compiles hex2 (v1)
-  hex2 (v1) compiles M0
-  M0 compiles cc_x86
-  cc_x86 compiles M2-Planet (v1)
-  M2-Planet (v1) compiles blood-elf (v1)
-  M2-Planet (v1) compiles hex2 (final)
-  M2-Planet (v1) compiles M1
-  M2-Planet (v1) compiles kaem
-  M2-Planet (v1) compiles blood-elf (final)
-  M2-Planet (v1) compiles get_machine
-  M2-Planet (v1) compiles M2-Planet (final)

This seems very intimidating, but becomes clearer when reading the
source: https://github.com/oriansj/mescc-tools-seed/blob/master/x86/
(start at mescc-tools-seed-kaem.kaem).

From here, we can move on from the lowest level stuff.

mescc-tools-extra
=================

mescc-tools and mes-m2 are the projects bootstrapped by
mescc-tools-seed. However, we have some currently unmerged additions to
mescc-tools that we require for this project, namely filesystem
utilities ``cp`` and ``chown``. This allows us to have one unified
directory for our binaries. Futhermore, we also build ``fletcher16``, a
preliminary checksumming tool, that we use to ensure reproducibility and
authenticity of generated binaries.

``/after``
==========

We now move into the ``/after`` directory. As mescc-tools-seed has no
concept of ``chdir()`` (not added until very late in mescc-tools-seed),
we have to copy a lot of files into the root of the initramfs, making it
very messy. We get into the move ordered directory ``/after`` here,
copying over all of the required binaries from ``/``.

mes
===

``mes`` is a scheme interpreter. It runs the sister project ``mescc``,
which is a C compiler written in scheme, which links against the Mes C
Library. All 3 are included in this same repository. Note that we are
using the experimental ``wip-m2`` branch to jump over the gap between
``M2-Planet`` and ``mes``. There are two stages to this part:

1. Compiling an initial mes using ``M2-Planet``. Note that this is
   *only* the Mes interpreter, not the libc or anything else.
2. We then use this to recompile the Mes interpreter as well as building
   the libc. This second interpreter is faster and less buggy. We need
   the libc to compile all the programs until we get glibc.

tinycc 0.9.26
=============

``tinycc`` is a minimal C compiler that aims to be small and fast. It
complies with all C89 and most of C99 standards.

First, we compile janneke’s fork of tcc 0.9.26 using ``mescc``,
containing 27 patches to make it operate well in the bootstrap
environment and make it compilable using ``mescc``. This is a
non-trivial process and as seen within tcc.kaem has many different parts
within it: a. tcc 0.9.26 is first compiled using ``mescc``. b. The mes
libc is recompiled using tcc (``mescc`` has a non-standard ``.a``
format), including some additions for later programs. c. tcc 0.9.26 is
recompiled 5(!) times to add new features that are required for other
features, namely ``long long`` and ``float``. Each time, the libc is
also recompiled.

tinycc 0.9.27
=============

Now, we compile upstream tcc 0.9.27, the latest release of tinycc, using
the final version of tcc 0.9.26. We then recompile the libc once more.

From this point onwards, until further notice, all programs are compiled
using tinycc 0.9.27. Note that now we begin to delve into the realm of
old GNU software, using older versions compilable by tinycc. Prior to
this point, all tools have been adapted significantly for the bootstrap;
now, we will be using old tooling instead.

tar 1.12
========

GNU ``tar`` is the most common archive format used by software source
code, often compressed also. To avoid continuing using submodules, we
build GNU tar 1.12, the last version compilable with mes libc.

gzip 1.2.4
==========

``gzip`` is the most common compression format used for software source
code. It is luckily distributed as a barebones uncompressed ``.tar``,
which we extract and then build.

Going forward, we can now use ``.tar.gz`` for source code.

sed 4.0.9
=========

You are most likely aware of GNU ``sed``, a line editor.

patch 2.5.9
===========

``patch`` is a very useful tool at this stage, allowing us to make
significantly more complex edits, including just changes to lines.
Luckily, we are able to patch patch using sed only.

sha-2
=====

``sha-2`` is a standalone external ``sha256sum`` implementation,
originally as a library, but patched to have a command line interface.
It is mostly output-compatible with ``sha256sum`` from coreutils. We use
this in replacement of ``fletcher16``.

Redo checksums using ``sha256sum``
==================================

We have now just built ``sha256sum``, which has a significantly (many orders
of magnitude) lower collision rate than ``fletcher16``, so we recheck all of
the existing binaries using ``sha256sum``.

patched mes-libc
================

Since patch is available at this point, we can apply additional fixes to
mes-libc that are not included in the wip-m2 branch and recompile libc.

patched tinycc
==============

In Guix, tinycc is patched to force static linking. Prior to this step,
we have been forced to manually specify static linking for each tool.
Now that we have patch, we can patch tinycc to force static linking and
then recompile it.

Note that we have to do this using tinycc 0.9.26, as tinycc 0.9.27
cannot recompile itself for unknown reasons.

make 3.80
=========

GNU ``make`` is now built so we have a more robust building system.
``make`` allows us to do things like define rules for files rather than
writing complex kaem scripts.

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
manually modified so that it can be processed by lex for the Heirloom
project (the required modifications are mostly syntactical, plus a few
workarounds to avoid some flex advanced features).

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
build tcc-0.9.27 that itself links to Mes C library but produces
binaries linked to musl. Then we recompile newly produced tcc with
itself. Interestingly, tcc-0.9.27 linked against musl is self hosting.

musl 1.1.24 (tcc-musl)
======================

We now rebuild ``musl`` with ``tcc-musl`` of Part 22, which fixes a
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

flex 2.6.14
===========

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
=============
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

automake 1.6.3
==============

GNU Automake is a tool for automatically generating Makefile.in files. It is another major
part of GNU Autotools build system and consists of ``aclocal`` and ``automake`` scripts.

We bootstrap it using a 3 stage process:

1. Use ``sed`` to replace a few placeholder variables in ``aclocal.in`` script.
   Then we manually install ``aclocal`` script and its dependencies.
2. Patch ``configure.in`` to create ``automake`` file but skip ``Makefile.in`` processing.
   Again we manually install ``automake`` script and its dependencies.
3. We now use ``aclocal``, ``autoconf``, and ``automake`` to do a proper build and install.

automake 1.4-p6
===============

This is an older version of GNU Automake. Various versions of GNU Autotools are not fully
compatible, and we will need older ``automake`` to build some older software.

autoconf 2.52
=============

We now properly rebuild ``autoconf`` using ``automake-1.4`` and manually installed ``autoconf``.

autoconf 2.13
=============

An older ``autoconf`` will be necessary to build GNU Binutils.

autoconf 2.12
=============

Yet another old autoconf version that we will need for some parts of GNU Binutils.

libtool 1.4
===========

GNU Libtool is the final part of GNU Autotools. It is a script used to hide away differences
when compiling shared libraries on different platforms.

binutils 2.14
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

autoconf 2.53
=============

We now start bootstrapping newer versions of autoconf. Version 2.53 now uses ``perl``.
In order to build it with ``autoconf-2.52`` we have to patch it a bit and then do
a second unpatched build with newly built ``autoconf-2.53``.

automake 1.7
============

Automake 1.7 and Autoconf 2.54 depend on each other, so we patch out two offending
autoconf macros to make it build with ``autoconf-2.53``.

autoconf 2.54
=============

More ``autoconf``.

automake 1.7
============

Rebuild ``automake`` with ``autoconf-2.54``.

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

Latest GNU Automake from 1.9 series. Slightly annoyingly depends itself but it is easy to patch
to make it buildable with 1.8.5. Then we rebuild ``automake-1.9`` with itself.

findutils 4.2.33
================

GNU Find Utilities can be used to search for files. We are mainly interested
in ``find`` and ``xargs`` that are often used in scripts.

libtool 2.2.4
=============

Newer version of libtool which is more compatible with modern Autotools.

automake 1.10.3
===============

GNU Automake from 1.10 series. ``aclocal`` is slightly patched to work
with our ``perl``.

autoconf 2.65
=============

Slightly newer version of GNU Autoconf. At this stage Autoconf is mostly
backwards compatible but newer versions need newer ``automake``.

gcc 4.0.4
=========

The GNU Compiler Collection (GCC) is an optimizing compiler produced by the
GNU Project. GCC is a key component of the GNU toolchain and the standard
compiler for most projects related to GNU and the Linux kernel.

Only the C frontend is built at this stage.

At this stage we are not yet able to regenerate top-level ``Makefile.in``
which needs GNU Autogen and hence Guile. Luckily, building GCC without
top-level ``Makefile`` is fairly easy.

musl 1.2.2
==========

GCC can build the latest as of the time of writing musl version.

We also don't need any of the TCC patches that we used before.

gcc 4.0.4
=========

Rebuild GCC with GCC and also against the latest musl.

bash 5.1
========

Up to this point, our build of ``bash`` could run scripts but could not be used
interactively.  This new version of ``bash`` compiles without any patches,
provides new features, and is built with GNU readline support so it can be used
as an interactive shell. ``autoconf-2.61`` is used to regenerate the configure
script and ``bison`` is used to recreate some included generated files.
