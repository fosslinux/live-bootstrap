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
	FILE* check_fp = fopen(argv[1], "r");
	require(check_fp != NULL, prepend_string(
			prepend_string("Error opening checksum file ", argv[1]), "\n"));
	int rc = 0;

	/* Now loop over each one */
	FILE* fp;
	char c;
	char* filename;
	char* s_rchecksum;
	char* file;
	int rchecksum;
	int i;
	int csum;
	/* It's much easier to just break; out of the loop */
	while(1)
	{
		filename = calloc(PATH_MAX, sizeof(char));
		require(filename != NULL, "Failed to allocate filename\n");
		s_rchecksum = calloc(16, sizeof(char));
		require(s_rchecksum != NULL, "Failed to allocate filename\n");
		/* Read the checksum */
		c = fgetc(check_fp);
		i = 0;
		while (c != ' ' && c != EOF)
		{
			require(in_set(c, "0123456789"), "Invalid checksum file\n");
			s_rchecksum[i] = c;
			c = fgetc(check_fp);
			i = i + 1;
		}
		if(c == EOF) break;
		/* Skip the space */
		c = fgetc(check_fp);
		/* Read the filename */
		i = 0;
		while(c != '\n' && c != EOF)
		{
			filename[i] = c;
			c = fgetc(check_fp);
			i = i + 1;
		}
		/* If we got here and it is EOF, we probably have all the needed data */
		/* Convert s_rchecksum -> int */
		rchecksum = numerate_string(s_rchecksum);

		/* Now let's actually calculate the real checksum */
		/* Read the file into a char* array */
		fp = fopen(filename, "r");
		/* Calculate the checksum! */
		csum = fletch16(fp);

		/* Compare */
		if(csum == rchecksum)
		{
			file_print(prepend_string(filename, ": OK\n"), stdout);
		}
		else
		{
			/* We failed.. we should fail at the end */
			rc = 1;
			file_print(prepend_string(filename, ": FAILED\n"), stdout);
		}

		/* Now if we finished the last one, leave */
		if(c == EOF) break;
	}

	return rc;
}
