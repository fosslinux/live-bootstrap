/*
 * SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <sys/stat.h>
#include <linux/syscall.h>
#include <linux/x86/syscall.h>

int _stat(const char *path, struct stat *buf) {
	int rc = stat(path, buf);
	if (rc == 0) {
		buf->st_atime = 0;
		buf->st_mtime = 0;
	}
	return rc;
}

int _lstat(const char *path, struct stat *buf) {
	int rc = lstat(path, buf);
	if (rc == 0) {
		buf->st_atime = 0;
		buf->st_mtime = 0;
	}
	return rc;
}

#define stat(a,b) _stat(a,b)
#define lstat(a,b) _lstat(a,b)
