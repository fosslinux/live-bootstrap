/*
 * SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
 *
 * SPDX-License-Identifier: Python-2.0.1
 *
 * Reimplmentation of keyword.py main() in C, to break bootstrapping loop
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 128

int main() {
	char filename[] = "Lib/keyword.py";
	FILE *orig = fopen(filename, "r");
	/* Read-write until starter line */
	char *line = malloc(MAX_LINE);
	do {
		fgets(line, MAX_LINE, orig);
		puts(line);
	} while (strcmp(line, "#--start keywords--\n") != 0);
	/* Perform the actual transformation */
	while (fgets(line, MAX_LINE, stdin) != NULL) {
		char *token = line;
		while (*token != '"') token++;
		token++;
		/* Now at beginning of keyword */
		char *end = token;
		while (*end != '"') end++;
		*end = '\0';
		/* Write output line to stdout */
		printf("'%s',\n", token);
		/* For each line also advance orig pointer */
		fgets(line, MAX_LINE, orig);
		/* Cleanup */
		free(line);
		line = malloc(MAX_LINE);
	}
	/* Read-write until end */
	while (fgets(line, MAX_LINE, orig) != NULL) {
		puts(line);
	}
}
