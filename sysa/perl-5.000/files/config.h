// SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
// SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

// SPDX-License-Identifier: GPL-3.0-or-later

#define MEM_ALIGNBYTES 8
#define BIN "/usr/bin"
#define BYTEORDER 0x4321
#define CPPSTDIN "tcc -E"
#define CPPMINUS "-"
#define HAS_ALARM
#define HAS_FORK
#define HAS_MKDIR
#define HAS_PAUSE
#define HAS_RMDIR
#define HAS_STRERROR
#define HAS_SYS_ERRLIST
#define Strerror(e) strerror(e)

#define HAS_SYSCALL
#define HAS_TIMES

#define HAS_VPRINTF
#define Gid_t gid_t

#define I_DIRENT
#define Direntry_t struct dirent

#define I_DLFCN
#define I_FCNTL
#define I_LIMITS
#define I_MATH
#define I_NETINET_IN
#define I_STDDEF
#define I_STDLIB
#define I_STRING
#define I_SYS_DIR
#define I_SYS_IOCTL
#define I_SYS_PARAM
#define I_SYS_SELECT
#define I_SYS_TIMES
#define I_TERMIOS
#define I_TIME
#define I_UNISTD
#define I_UTIME
#define I_STDARG

#define INTSIZE 4
#define Off_t off_t

#define PRIVLIB "/usr/lib/perl5"

#define _(args) args

#define RANDBITS 31

#define SCRIPTDIR "/usr/bin"

#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","IOT","BUS","FPE","KILL","USR1","SEGV","USR2","PIPE","ALRM","TERM","STKFLT","CHLD","CONT","STOP","TSTP","TTIN","TTOU","URG","XCPU","XFSZ","VTALRM","PROF","WINCH", "IO", "POLL", "PWR", "SYS", "UNUSED"

#define Size_t size_t

#define STDCHAR char
#define Uid_t uid_t

#define LOC_SED "/usr/bin/sed"
#define ARCHLIB "/usr/lib/perl5/"

#define CAT2(a,b) a##b
#define Gconvert(x,n,t,b) gcvt((x),(n),(b))

#define Time_t time_t

#define SSize_t ssize_t
