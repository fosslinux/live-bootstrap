/*
 * SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
 * SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <sys/stat.h>
#include <linux/syscall.h>
#include <linux/x86/syscall.h>

int _lstat(const char *path, struct stat *buf) {
	int rc = lstat(path, buf);
	if (rc == 0) {
		buf->st_atime = 0;
		buf->st_mtime = 0;
	}
	return rc;
}

/* stat is deliberately hacked to be lstat.
   In src/system.h tar already defines lstat to be stat
   since S_ISLNK is not defined in mes C library
   Hence, we can't use something like #define lstat(a,b) _lstat(a,b)
   to have separate stat and lstat functions.
   Thus here we break tar with --dereference option but we don't use
   this option in live-bootstrap.
 */
#define stat(a,b) _lstat(a,b)
