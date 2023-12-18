# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

CC      = tcc
LD      = tcc
AR      = tcc -ar

COMMON_CFLAGS  = \
	-DHAVE_DIRENT_H \
	-DSTRUCT_DIRENT_HAS_D_INO \
	-DHAVE_STDINT_H \
	-DHAVE_LIMITS_H \
	-DHAVE_STRING_H \
	-DHAVE_INTTYPES_H \
	-DRETSIGTYPE=void \
	-DHUGE_VAL=10000000000.0 \
	-DTERMIO_TTY_DRIVER \
	-DPREFER_STDARG \
	-DHAVE_DECL_STRTOL \
	-DHAVE_DECL_STRTOLL \
	-DHAVE_DECL_STRTOUL \
	-DHAVE_DECL_STRTOULL \
	-DHAVE_TZNAME \
	-DPIPESIZE=4096 \
	-DDEFAULT_PATH_VALUE=\"$(prefix)/bin\" \
	-DSTANDARD_UTILS_PATH=\"$(prefix)/bin\" \
	-DPPROMPT=\"$ \" \
	-DSPROMPT=\"$ \" \
	-DCONF_MACHTYPE=\"bootstrap\" \
	-DGETGROUPS_T=int \
	-DCOND_COMMAND \
	-DCONF_HOSTTYPE=\"i386\" \
	-DCONF_OSTYPE=\"linux\" \
	-DDEFAULT_MAIL_DIRECTORY=\"/fake-mail\" \
	-DVOID_SIGHANDLER \
	-DDISTVERSION=\"2.05b\" \
	-DBUILDVERSION=\"0\" \
	-DSCCSVERSION=\"2.05b\" \
	-DLC_ALL=\"C\" \
	-DHAVE_STRERROR \
	-DHAVE_MEMSET \
	-DHAVE_DUP2 \
	-DHAVE_STRTOUL \
	-DHAVE_STRTOULL \
	-DHAVE_STRCHR \
	-DHAVE_BCOPY \
	-DHAVE_BZERO \
	-DHAVE_POSIX_SIGNALS \
	-DHAVE_GETCWD \
	-DHAVE_RENAME \
	-DHAVE_SYS_SIGLIST \
	-Dendpwent\(x\)=0 \
	-Denable_hostname_completion\(on_or_off\)=0

BUILTINS_DEF_FILES = alias bind break builtin cd colon command complete declare \
	echo enable eval exec exit fc fg_bg hash history jobs kill let read return \
	set setattr shift source suspend test times trap type ulimit umask wait \
	getopts pushd shopt printf
