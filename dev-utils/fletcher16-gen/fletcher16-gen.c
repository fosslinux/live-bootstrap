/*
 * SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdlib.h>
#include <stdio.h>

#define BUF_SIZE 1024
// CONSTANT BUF_SIZE 1024
#define PATH_MAX 512
// CONSTANT PATH_MAX 512

int fletch16(FILE* fp)
{
	long sum1 = 0;
	long sum2 = 0;
	int index;
	int c = fgetc(fp);
	while(c != EOF)
	{
		sum1 = (sum1 + c) % 255;
		sum2 = (sum2 + sum1) % 255;
		c = fgetc(fp);
	}

	return (sum2 << 8) | sum1;
}

int main(int argc, char** argv)
{
	/* Open the file containing the checksums */
	FILE* fp = fopen(argv[1], "r");
	require(fp != NULL, prepend_string(
			prepend_string("Error opening checksum file ", argv[1]), "\n"));
	int csum = fletch16(fp);
	file_print(prepend_string(prepend_string(prepend_string(
				numerate_number(csum), " "), argv[1]), "\n"), stdout);
}
