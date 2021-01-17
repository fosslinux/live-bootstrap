# live-bootstrap

An attempt to provide a reproducible, automatic, complete end-to-end bootstrap
from a minimal number of binary seeds to a supported fully functioning operating
system.

## Get me started!

1. `git clone https://github.com/fosslinux/live-bootstrap`
2. `git submodule update --init --recursive`
3. Provide a kernel (vmlinuz file) as the name kernel in the root of the repository.
4. `./rootfs.sh` - ensure your account has kvm priviliges and qemu installed.
   a. Alternatively, run `./rootfs.sh chroot` to run it in a chroot.
   b. Alternatively, run `./rootfs.sh` but don't run the actual virtualization
   and instead copy sysa/tmp/initramfs.igz to a USB or some other device and 
   boot from bare metal.
6. Wait.
7. Currently, live-bootstrap dosen't provide anything to you, as it is incomplete.

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
| Total size of seeds [1] | ~120MB (Reduced Source Bootstrap) [2] | ~1KB                        |
| Use of kernel           | Linux-Libre Kernel                    | Any Linux Kernel (2.6+) [3] |
| Implementation complete | Yes                                   | No (in development)         |
| Automation              | Almost fully automatic                | Optional user customization |

[1]: Excluding kernel.
[2]: Reiterating that Guix is working on a full source bootstrap.
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
2. Any pregenerated configure scripts, or Makefile.in's from autotools.
3. Pregenerated bison/flex parsers (identifiable through a `.y` file).
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
allows us to have one unified directory for our binaries.

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

1. Compiling an initial mes using `M2-Planet`. Note that this is *only* the
   Mes interpreter, not the libc or anything else.
2. We then use this to recompile the Mes interpreter as well as building the
   libc. This second interpreter is faster and less buggy. We need the libc to
   compile all the programs until we get glibc.

#### Part 6: tinycc

`tinycc` is a minimal C compiler that aims to be small and fast. It complies
with all C89 and most of C99 standards. This is also a two-tiered process:

1. First, we compile janneke's fork of tcc 0.9.26 using `mescc`, containing
   27 patches to make it operate well in the bootstrap environment and make 
   it compilable using `mescc`. This is a non-trivial process and as seen
   within tcc.kaem has many different parts within it:
   a. tcc 0.9.26 is first compiled using `mescc`.
   b. The mes libc is recompiled using tcc (`mescc` has a non-standard `.a`
   format), including some additions for later programs.
   c. tcc 0.9.26 is recompiled 5(!) times to add new features that are required
   for other features, namely `long long` and `float`. Each time, the libc is
   also recompiled.
2. Then we compile upstream tcc 0.9.27, the latest release of tinycc, using the
   final version of tcc 0.9.26. We then recompile the libc once more.
   
From this point onwards, until further notice, all programs are compiled using
tinycc 0.9.27. Note that now we begin to delve into the realm of old GNU
software, using older versions compilable by tinycc. Prior to this point, all
tools have been adapted significantly for the bootstrap; now, we will be using
old tooling instead.

#### Part 7: sed 4.0.7

You are most likely aware of GNU `sed`, a line editor. Here, we had to make a
compromise between two versions:

- 1.18: fully functional `s/a/b`, but no `-i`.
- 4.0.7: fully functional `-i`, but broken `s/a/b`.

We opted for the latter, as otherwise there would be no patching of files for
quite some time. We can delete lines from files now!

#### Part 8: tar 1.12

GNU `tar` is the most common archive format used by software source code, often
compressed also. To avoid continuing using submodules, we build GNU tar 1.12,
the last version compilable by tinycc without significant patching.

#### Part 9: gzip 1.2.4

`gzip` is the most common compression format used for software source code. It
is luckily distributed as a barebones uncompressed `.tar`, which we extract and
then build. We do require deletion of a few lines unsupported by mes libc.

Going forward, we can now use `.tar.gz` for source code.

#### Part 10: diffutils 2.7

`diffutils` is useful for comparing two files. It is not immediately needed but
is required later for autotools.

#### Part 11: patch 2.5.9

`patch` is a very useful tool at this stage, allowing us to make sigificantly
more complex edits, including just changes to lines. Luckily, we are able to
patch patch using sed only.

#### Part 12: patched tinycc

In Guix, tinycc is patched to force static linking. Prior to this step, we have
been forced to manually specify static linking for each tool. Now that we have
patch, we can patch tinycc to force static linking and then recompile it.

Note that we have to do this using tinycc 0.9.26, as tinycc 0.9.27 cannot
recompile itself for unknown reasonsn.

#### Part 13: make 3.80

GNU `make` is now built so we have a more robust building system. `make` allows
us to do things like define rules for files rather than writing complex kaem
scripts.

#### Part 14: bzip2 1.0.8

`bzip2` is a compression format that compresses more than `gzip`. It is
preferred where we can use it, and makes source code sizes smaller.

#### Part 15: bash 2.05b

GNU `bash` is the most well known shell and the most complex piece of software
so far. However, it comes with a number of great benefits over kaem, including
proper POSIX sh support, globbing, etc.

NOTE: Currently, there is a bison pregenerated file here, which we are working
to remove.

#### Part 16: m4 1.4

`m4` is the first piece of software we need in the autotools suite. It allows
macros to be defined and files to be generated from those macros.
