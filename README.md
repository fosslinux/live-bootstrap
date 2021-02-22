<!--
SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# live-bootstrap

An attempt to provide a reproducible, automatic, complete end-to-end bootstrap
from a minimal number of binary seeds to a supported fully functioning operating
system.

## Get me started!

1. `git clone https://github.com/fosslinux/live-bootstrap`
2. `git submodule update --init --recursive`
3. Provide a kernel (vmlinuz file) as the name kernel in the root of the repository.
4. `./rootfs.sh` - ensure your account has kvm privileges and qemu installed.
   a. Alternatively, run `./rootfs.sh chroot` to run it in a chroot.
   b. Alternatively, run `./rootfs.sh` but don't run the actual virtualization
   and instead copy sysa/tmp/initramfs.igz to a USB or some other device and 
   boot from bare metal.
6. Wait.
7. Currently, live-bootstrap doesn't provide anything to you, as it is incomplete.

## Background

This project is a part of the bootstrappable project, a project that aims to be
able to build complete computing platforms through the use of source code. When
you build a compiler like GCC, you need another C compiler to compile the
compiler - turtles all the way down. Even the first GCC compiler was written in
C. There has to be a way to break the chain...

There has been significant work on this over the last 5 years, from Jeremiah
Orians' stage0, hex2 and M2-Planet to janneke's Mes. We have a currently,
fully-functioning chain of bootstrapping from the 357-byte hex0 seed to a
complete GCC compiler and hence a full Linux operating system. From there, it is
trivial to move to other UNIXes. However, there is only currently one vector
through which this can be automatically done, GNU Guix.

While the primary author of this project does not believe Guix is a bad project,
the great reliance on Guile, the complexity of many of the scripts and the
rather steep learning curve to install and run Guix make it a very non
plug-and-play solution. Furthermore, there is currently (Jan 2021) no possible
way to run the bootstrap from outside of a pre-existing Linux environment.
Additionally, Guix uses many scripts and distributed files that cannot be
considered source code.

(NOTE: Guix is working on a Full Source Bootstrap, but I'm not completely sure
what that entails).

Furthermore, having an alternative bootstrap automation tool allows people to
have greater trust in the bootstrap procedure.

## Comparison between GNU Guix and live-bootstrap

| Item                    | Guix                                  | live-bootstrap              |
| --                      | --                                    | --                          |
| Total size of seeds [1] | ~30MB (Reduced Source Bootstrap) [2]  | ~1KB                        |
| Use of kernel           | Linux-Libre Kernel                    | Any Linux Kernel (2.6+) [3] |
| Implementation complete | Yes                                   | No (in development)         |
| Automation              | Almost fully automatic                | Optional user customization |

[1]: Excluding kernel.
[2]: Reiterating that Guix is working on a full source bootstrap, although that still uses guile (~12 MB).
[3]: Work is ongoing to use other, smaller POSIX kernels.

## Why would I want bootstrapping?

That is outside of the scope of this README. Here's a few things you can look
at:

- https://bootstrappable.org
- Trusting Trust Attack (as described by Ken Thompson)
- https://guix.gnu.org/manual/en/html_node/Bootstrapping.html
- Collapse of the Internet (eg CollapseOS)

## Specific things to be bootstrapped

GNU Guix is currently the furthest along project to automate bootstrapping.
However, there are a number of non-auditable files used in many of their
packages. Here is a list of file types that we deem unsuitable for
bootstrapping.

1. Binaries (apart from seed hex0, kaem, kernel).
2. Any pre-generated configure scripts, or Makefile.in's from autotools.
3. Pre-generated bison/flex parsers (identifiable through a `.y` file).
4. Any source code/binaries downloaded within a software's build system that is
   outside of our control to verify before use in the build system.
5. Any non-free software. [1]

[1]: We only use software licensed under a FSF-approved free software license.

## How does this work?

### sysa

sysa is the first 'system' used in live-bootstrap. We move to a new system after
a reboot, which often occurs after the movement to a new kernel. It is run by
the seed Linux kernel provided by the user, and has 16 parts.

#### Part 1: mescc-tools-seed

This is where all the magic begins. We start with our hex0 and kaem seeds and
bootstrap our way up to M2-Planet, a subset of C, and mes-m2, an independent
port of GNU Mes to M2-Planet. The following steps are taken here:

- hex0 (seed)
- hex0 compiles hex1
- hex0 compiles catm
- hex1 compiles hex2 (v1)
- hex2 (v1) compiles M0
- M0 compiles cc_x86
- cc_x86 compiles M2-Planet (v1)
- M2-Planet (v1) compiles blood-elf (v1)
- M2-Planet (v1) compiles hex2 (final)
- M2-Planet (v1) compiles M1
- M2-Planet (v1) compiles kaem
- M2-Planet (v1) compiles blood-elf (final)
- M2-Planet (v1) compiles get_machine
- M2-Planet (v1) compiles M2-Planet (final)

This seems very intimidating, but becomes clearer when reading the source:
https://github.com/oriansj/mescc-tools-seed/blob/master/x86/ (start at
mescc-tools-seed-kaem.kaem).

From here, we can move on from the lowest level stuff.

#### Part 2: mescc-tools-extra

mescc-tools and mes-m2 are the projects bootstrapped by mescc-tools-seed.
However, we have some currently unmerged additions to mescc-tools that we
require for this project, namely filesystem utilities `cp` and `chown`. This
allows us to have one unified directory for our binaries. Futhermore, we also
build `fletcher16`, a preliminary checksumming tool, that we use to ensure
reproducibility and authenticity of generated binaries.

#### Part 3: `/after`

We now move into the `/after` directory. As mescc-tools-seed has no concept of
`chdir()` (not added until very late in mescc-tools-seed), we have to copy a lot
of files into the root of the initramfs, making it very messy. We get into the
move ordered directory `/after` here, copying over all of the required binaries
from `/`.

#### Part 4: blynn-compiler

`blynn-compiler` is a project on top of mescc-tools-seed to bootstrap a minimal
haskell compiler from M2-Planet. While we don't currently use this for anything,
it is planned to be eventually used to bootstrap the next part.

#### Part 5: mes

`mes` is a scheme interpreter. It runs the sister project `mescc`, which is a C
compiler written in scheme, which links against the Mes C Library. All 3 are
included in this same repository. Note that we are using the experimental
`wip-m2` branch to jump over the gap between `M2-Planet` and `mes`. There are
two stages to this part:

1. Compiling an initial mes using `M2-Planet`. Note that this is *only* the Mes
   interpreter, not the libc or anything else.
2. We then use this to recompile the Mes interpreter as well as building the
   libc. This second interpreter is faster and less buggy. We need the libc to
   compile all the programs until we get glibc.

#### Part 6, 7: tinycc

`tinycc` is a minimal C compiler that aims to be small and fast. It complies
with all C89 and most of C99 standards. This is also a two-tiered process:

1. First, we compile janneke's fork of tcc 0.9.26 using `mescc`, containing 27
   patches to make it operate well in the bootstrap environment and make it
   compilable using `mescc`. This is a non-trivial process and as seen within
   tcc.kaem has many different parts within it: a. tcc 0.9.26 is first compiled
   using `mescc`.  b. The mes libc is recompiled using tcc (`mescc` has a
   non-standard `.a` format), including some additions for later programs.  c.
   tcc 0.9.26 is recompiled 5(!) times to add new features that are required for
   other features, namely `long long` and `float`. Each time, the libc is also
   recompiled.
2. Then we compile upstream tcc 0.9.27, the latest release of tinycc, using the
   final version of tcc 0.9.26. We then recompile the libc once more.
   
From this point onwards, until further notice, all programs are compiled using
tinycc 0.9.27. Note that now we begin to delve into the realm of old GNU
software, using older versions compilable by tinycc. Prior to this point, all
tools have been adapted significantly for the bootstrap; now, we will be using
old tooling instead.

#### Part 8: sed 4.0.7

You are most likely aware of GNU `sed`, a line editor.

#### Part 9: tar 1.12

GNU `tar` is the most common archive format used by software source code, often
compressed also. To avoid continuing using submodules, we build GNU tar 1.12,
the last version compilable by tinycc without significant patching.

#### Part 10: gzip 1.2.4

`gzip` is the most common compression format used for software source code. It
is luckily distributed as a barebones uncompressed `.tar`, which we extract and
then build. We do require deletion of a few lines unsupported by mes libc.

Going forward, we can now use `.tar.gz` for source code.

#### Part 11: patch 2.5.9

`patch` is a very useful tool at this stage, allowing us to make significantly
more complex edits, including just changes to lines. Luckily, we are able to
patch patch using sed only.

#### Part 12: sha-2

`sha-2` is a standalone external `sha256sum` implementation, originally as a
library, but patched to have a command line interface. It is mostly
output-compatible with `sha256sum` from coreutils. We use this in replacement of
`fletcher16`.

#### Part 12a: Redo checksums using `sha256sum`

We have now just built `sha256sum`, which has a 16x lower collision rate than
`fletcher16`, so we recheck all of the existing binaries using `sha256sum`.

#### Part 13: patched mes-libc

Since patch is available at this point, we can apply additional fixes to
mes-libc that are not included in the wip-m2 branch and recompile libc.

#### Part 14: patched tinycc

In Guix, tinycc is patched to force static linking. Prior to this step, we have
been forced to manually specify static linking for each tool. Now that we have
patch, we can patch tinycc to force static linking and then recompile it.

Note that we have to do this using tinycc 0.9.26, as tinycc 0.9.27 cannot
recompile itself for unknown reasons.

#### Part 15: make 3.80

GNU `make` is now built so we have a more robust building system. `make` allows
us to do things like define rules for files rather than writing complex kaem
scripts.

#### Part 16: bzip2 1.0.8

`bzip2` is a compression format that compresses more than `gzip`. It is
preferred where we can use it, and makes source code sizes smaller.

#### Part 17: coreutils 5.0.0

GNU Coreutils is a collection of widely used utilities such as `cat`, `chmod`,
`chown`, `cp`, `install`, `ln`, `ls`, `mkdir`, `mknod`, `mv`, `rm`, `rmdir`,
`tee`, `test`, `true`, and many others.

A few of the utilities cannot be easily compiled with Mes C library, so we skip
them.

The `cp` in this stage replaces the `mescc-tools-extra` `cp`.

#### Part 18: heirloom devtools

`lex` and `yacc` from the Heirloom project. The Heirloom project is a collection
of standard UNIX utilities derived from code by Caldera and Sun. Differently
from the analogous utilities from the GNU project, they can be compiled with a
simple `Makefile`.

#### Part 19: bash 2.05b

GNU `bash` is the most well known shell and the most complex piece of software
so far. However, it comes with a number of great benefits over kaem, including
proper POSIX sh support, globbing, etc.

Bash ships with a bison pre-generated file here which we delete. Unfortunately,
we have not bootstrapped bison but fortunately for us, heirloom yacc is able to
cope here.

#### Part 20: flex 2.5.11

`flex` is a tool for generating lexers or scanners: programs that recognize
lexical patters.

Unfortunately `flex` also depends on itself for compiling its own scanner, so
first flex 2.5.11 is compiled, with its scanner definition manually modified so
that it can be processed by lex for the Heirloom project (the required
modifications are mostly syntactical, plus a few workarounds to avoid some flex
advanced features).

#### Part 21: musl 1.1.24

`musl` is a C standard library that is lightweight, fast, simple, free, and
strives to be correct in the sense of standards-conformance and safety. `musl`
is used by some distributions of GNU/Linux as their C library. Our previous Mes
C library was incomplete which prevented us from building many newer or more
complex programs.

`tcc` has slight problems when building and linking `musl`, so we apply a few
patches. In particular, we replace all weak symbols with strong symbols and will
patch `tcc` in the next step to ignore duplicate symbols.

#### Part 22: tcc 0.9.27 (musl)

We recompile `tcc` against musl. This is a two stage process. First we build
tcc-0.9.27 that itself links to Mes C library but produces binaries linked to
musl. Then we recompile newly produced tcc with itself. Interestingly,
tcc-0.9.27 linked against musl is self hosting.

#### Part 23: musl 1.1.24 (tcc-musl)

We now rebuild `musl` with `tcc-musl` of Part 22, which fixes a number of bugs,
particularly regarding floats, in the first `musl`.

#### Part 24: tcc 0.9.27 (musl v2)

Now that we have a 'fixed' `musl`, we now recompile `tcc` as `tcc` uses floats
extensively.

#### Part 25: m4 1.4.7

`m4` is the first piece of software we need in the autotools suite, flex 2.6.4
and bison.  It allows macros to be defined and files to be generated from those
macros.

#### Part 26: flex 2.6.14

We recompile unpatched GNU `flex` using older flex 2.5.11. This is again a two
stage process, first compiling flex using `scan.c` (from `scan.l`) created by
old flex, then recompile `scan.c` using the new version of flex to remove any
buggy artifacts from the old flex.

#### Part 27: bison 3.4.1

GNU `bison` is a parser generator. With `m4` and `flex` we can now bootstrap it
following https://gitlab.com/giomasce/bison-bootstrap. It's a 3 stage process:

1. Build bison using a handwritten grammar parser in C.
2. Use bison from previous stage on a simplified bison grammar file.
3. Build bison using original grammar file.

Finally we have a fully functional `bison` executable.

#### Part 28: grep 2.4

GNU `grep` is a pattern matching utility. Is is not immediately needed but will
be useful later for autotools.

#### Part 29: diffutils 2.7

`diffutils` is useful for comparing two files. It is not immediately needed but
is required later for autotools.

#### Part 30: coreutils 5.0

`coreutils` is rebuilt against musl.  Additional utilities are built including
`comm`, `expr`, `date`, `dd`, `sort`, `uname` and `uniq`. This fixes a variety
of issues with existing `coreutils`.

#### Part 31: gawk 3.0.4

`gawk` is the GNU implementation of `awk`, yet another pattern matching and data
extraction utility. It is also required for autotools.

#### Part 32: perl 5.000

Perl is a general purpose programming language that is especially suitable for
text processing. It is essential for autotools build system because automake and
some other tools are written in Perl.

Perl itself is written in C but ships with some pre-generated files that need
perl for processing, namely `embed.h` and `keywords.h`. To bootstrap Perl we
will start with the oldest Perl 5 version which has the fewest number of
pregenerated files. We reimplement two remaining perl scripts in awk and use our
custom makefile instead of Perl's pre-generated Configure script.

At this first step we build `miniperl` which is `perl` without support for
loading modules.

#### Part 33: perl 5.003

We now use `perl` from the previous stage to recreate pre-generated files that
are shipped in perl 5.003. But for now we still need to use handwritten makefile
instead of `./Configure` script.

#### Part 34: perl 5.004_05

Yet another version of perl; the last version buildable with 5.003.

#### Part 35: perl 5.005_03

More perl! This is the last version buildable with 5.004. It also introduces the
new pregenerated files `regnodes.h` and `byterun.{h,c}`.
