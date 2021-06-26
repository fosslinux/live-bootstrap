// SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
// SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

// SPDX-License-Identifier: GPL-3.0-or-later

#define MEM_ALIGNBYTES 8
#define BIN "/image/bin"
#define BYTEORDER 0x4321
#define CPPSTDIN "gcc -E"
#define CPPMINUS "-"
#define HAS_ALARM
#define HAS_DIRENT
#define HAS_FORK
#define HAS_FCNTL
#define HAS_MEMCHR
#define HAS_MEMSET
#define HAS_MKDIR
#define HAS_PAUSE
#define HAS_READDIR
#define HAS_RMDIR
#define HAS_SETLOCALE
#define HAS_STRERROR
#define HAS_STRTOUL
#define HAS_STRTOULL
#define HAS_STRXFRM
#define HAS_SYS_ERRLIST
#define Strerror(e) strerror(e)

#define HAS_SYSCALL
#define HAS_SYSCALL_PROTO
#define HAS_TIMES
#define HAS_FLOCK
#define HAS_TRUNCATE

#define HAS_VPRINTF
#define Gid_t gid_t

#define I_DIRENT
#define Direntry_t struct dirent

#define I_DLFCN
#define I_FCNTL
#define I_GRP
#define I_LIMITS
#define I_LOCALE
#define I_MATH
#define I_NETINET_IN
#define I_PWD
#define I_STDDEF
#define I_STDLIB
#define I_STRING
#define I_SYS_DIR
#define I_SYS_IOCTL
#define I_SYS_PARAM
#define I_SYS_SELECT
#define I_SYS_STAT
#define I_SYS_TIMES
#define I_TERMIOS
#define I_TIME
#define I_UNISTD
#define I_UTIME
#define I_STDARG

#define INTSIZE 4
#define Off_t off_t

#define PRIVLIB "/image/lib/perl5"

#define _(args) args

#define SCRIPTDIR "/image/bin"

#define SIG_NAME "ZERO", "HUP", "INT", "QUIT", "ILL", "TRAP", "ABRT", "BUS", "FPE", "KILL", "USR1", "SEGV", "USR2", "PIPE", "ALRM", "TERM", "STKFLT", "CHLD", "CONT", "STOP", "TSTP", "TTIN", "TTOU", "URG", "XCPU", "XFSZ", "VTALRM", "PROF", "WINCH", "IO", "PWR", "SYS", "NUM32", "NUM33", "NUM34", "RTMIN", "NUM36", "NUM37", "NUM38", "NUM39", "NUM40", "NUM41", "NUM42", "NUM43", "NUM44", "NUM45", "NUM46", "NUM47", "NUM48", "NUM49", "NUM50", "NUM51", "NUM52", "NUM53", "NUM54", "NUM55", "NUM56", "NUM57", "NUM58", "NUM59", "NUM60", "NUM61", "NUM62", "NUM63", "RTMAX", "IOT", "POLL", "UNUSED", 0		/**/
#define SIG_NUM  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 6, 29, 31, 0		/**/
#define SIG_SIZE 68			/**/

#define Size_t size_t

#define STDCHAR char
#define Uid_t uid_t

#define LOC_SED "/image/bin/sed"
#define ARCHLIB "/image/lib/perl5/"

#define CAT2(a,b) a##b
#define STRINGIFY(a) "a"
#define Gconvert(x,n,t,b) gcvt((x),(n),(b))

#define Time_t time_t

#define SSize_t ssize_t

#define Sigjmp_buf sigjmp_buf

#define Sigsetjmp(buf,save_mask) setjmp(buf)
#define Siglongjmp(buf,retval) longjmp(buf,retval)

#define Signal_t void

#define OSNAME "linux"

#undef __DATE__ /* want reproducible build */

#define Malloc_t void *
#define Free_t void

#define SH_PATH "/bin/sh"
#define ARCHNAME "i386-linux"
#define BIN_EXP "/image/bin"

#define NVSIZE 8 /* sizeof(double) */
#define UVSIZE 4 /* sizeof(long) on i386 */
#define IVSIZE 4
#define PTRSIZE 4

#define IVTYPE long
#define UVTYPE unsigned long
#define NVTYPE double
#define I8TYPE char
#define U8TYPE unsigned char
#define I16TYPE short
#define U16TYPE unsigned short
#define I32TYPE int
#define U32TYPE unsigned int
#define STDCHAR char

#define IVdf "ld"
#define UVuf "lu"
#define UVof "lo"
#define UVxf "lx"
#define UVXf "lX"
#define NVef "e"
#define NVff "f"
#define NVgf "g"

#define Mode_t mode_t
#define Off_t off_t
#define Pid_t pid_t
#define Rand_seed_t long
#define Size_t_size 4
#define Sock_size_t unsigned

#define RANDBITS 48
#define seedDrand01(x) srand48((long)x)
#define Drand01() drand48()

#define	NVSIZE 8
#define	NV_PRESERVES_UV
#define	NV_PRESERVES_UV_BITS 32
#define	NV_OVERFLOWS_INTEGERS_AT 256.0*256.0*256.0*256.0*256.0*256.0*2.0*2.0*2.0*2.0*2.0
#define	NV_ZERO_IS_ALLBITS_ZERO

#define L_R_TZSET
#define GMTIME_MAX 2147483647
#define GMTIME_MIN -2147483648
#define LOCALTIME_MAX 2147483647
#define LOCALTIME_MIN -2147483648
#define LOCALTIME_R_PROTO 0

#define PERL_PATCHLEVEL_H_IMPLICIT

#define MULTIPLICITY

#define OSNAME "linux"
#define OSVERS "3.18.0"
