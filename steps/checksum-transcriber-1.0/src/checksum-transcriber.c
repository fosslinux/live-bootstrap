/*
 * SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <bootstrappable.h>
#include <unistd.h>

#define MAX_STRING 4096
#define MAX_TOKENS 8

char *get_distfiles(char **envp) {
	char *envvar = "DISTFILES=";
	int i = 0;
	while (envp[i] != NULL && strncmp(envp[i], envvar, strlen(envvar)) != 0) i += 1;
	// Now we have distfiles= - get just the part we want.
	require(envp[i] != NULL, "Unable to find distfiles environment variable");
	return envp[i] + strlen(envvar);
}

int main(int argc, char **argv, char **envp) {
	// Random file things
	require(argc == 2, "Usage: checksum-transcriber FILENAME");
	char *input = argv[1];
	FILE *in = fopen(input, "r");
	require(in != NULL, "File does not exist");
	char *output = calloc(MAX_STRING, sizeof(char));
	require(strcpy(output, input) != NULL, "Failed copying string");
	require(strcat(output, ".SHA256SUM") != NULL, "Failed concating string");
	FILE *out = fopen(output, "w+");
	require(out != NULL, "Failed opening output file");

	char *orig_line;
	char *line = calloc(MAX_STRING, sizeof(char));
	require(line != NULL, "Failed allocating string");
	char **tokens;
	char *new_line;
	char *checksum;
	char *filename;
	int i;
	fgets(line, MAX_STRING, in);
	while (strlen(line) != 0) {
		// Split each line into tokens
		orig_line = line;
		tokens = calloc(MAX_TOKENS, sizeof(char*));
		i = 0;
		while (i < MAX_TOKENS) {
			tokens[i] = line;
			new_line = strchr(line, ' ');
			// Occurs when there are only two tokens
			if (new_line == NULL) break;
			line = new_line;
			line[0] = '\0';
			line += 1;
			i += 1;
		}
		line = strchr(line, '\n');
		line[0] = '\0';
		// Only "file" type of distfile supported at this point
		require(strcmp(tokens[0], "f") == 0 || strcmp(tokens[0], "file") == 0,
		        "Only support file distfile type at this point");
		// Get checksum and filename
		checksum = tokens[2];
		if (tokens[3] != NULL) {
			filename = tokens[3];
		} else {
			filename = strrchr(tokens[1], '/');
			filename += 1;
		}
		// Put it all together
		fputs(checksum, out);
		fputs("  ", out);
		fputs(get_distfiles(envp), out);
		fputc('/', out);
		fputs(filename, out);
		fputc('\n', out);
		// Cleanup
		i = 0;
		free(orig_line);
		free(tokens);
		line = calloc(MAX_STRING, sizeof(char));
		require(line != NULL, "Failed allocating string");
		fgets(line, MAX_STRING, in);
	}

	// Clean up
	fclose(in);
	fclose(out);
}
