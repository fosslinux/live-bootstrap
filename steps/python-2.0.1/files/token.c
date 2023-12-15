/*
 * SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
 *
 * SPDX-License-Identifier: Python-2.0.1
 *
 * Reimplmentation of token.py main() in C, to break bootstrapping loop
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 128

int main(int argc, char** argv) {
	char *filename = argv[1];
	FILE *orig = fopen(filename, "r");
	/* Read-write until starter line */
	char *line = malloc(MAX_LINE);
	do {
		fgets(line, MAX_LINE, orig);
		puts(line);
	} while (strcmp(line, "#--start constants--\n") != 0);
	/* Perform the actual transformation */
	while (fgets(line, MAX_LINE, stdin) != NULL) {
		/* Transform input into output */
		char *tokena = line + 8;
		char *tokenb = strstr(tokena, "\t");
		if (tokenb == 0) tokenb = strstr(tokena, " ");
		*tokenb = '\0';
		tokenb++;
		while (*tokenb == '\t' || *tokenb == ' ') tokenb++;
		/* Write output line to stdout */
		printf("%s = %s", tokena, tokenb);
		/* For each line also advance orig pointer */
		fgets(line, MAX_LINE, orig);
		/* Cleanup */
		free(line);
		line = malloc(MAX_LINE);
	}
	/* Read-write until end */
	while (fgets(line, MAX_LINE, orig) != NULL) {
		puts(line);
		fflush(stdout);
	}
}
