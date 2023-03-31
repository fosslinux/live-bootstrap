#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "M2libc/bootstrappable.h"

/*
SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com>
SPDX-License-Identifier: MIT

Simple Patch program.

This program is written in a subset of C called M2, which is from the
stage0-posix bootstrap project.

Example usage:
./simple-patch file_to_patch before_pattern_file after_pattern_file

*/

// function prototypes
void read_file_or_die(char *file_name, char **buffer, int *file_size);
void patch_buffer_or_die(char *patch_file_before_buffer, int patch_file_before_size,
                 char *before_pattern_buffer, int before_pattern_size,
                 char *after_pattern_buffer, int after_pattern_size,
                 char *patch_file_after_buffer);
void writestr_fd(int fd, char *str);
int memsame(char *search_buffer, int search_size,
            char *pattern_buffer, int pattern_size);


int main(int argc, char **argv)
{
    char *patch_file_before_buffer;
    int patch_file_before_size;

    char *before_pattern_buffer;
    int before_pattern_size;

    char *after_pattern_buffer;
    int after_pattern_size;

    int patch_file_after_size;
    char *patch_file_after_buffer;

    int patch_file_fd;

    read_file_or_die(argv[1], &patch_file_before_buffer, &patch_file_before_size);
    read_file_or_die(argv[2], &before_pattern_buffer, &before_pattern_size);
    read_file_or_die(argv[3], &after_pattern_buffer, &after_pattern_size);

    patch_file_after_size = patch_file_before_size - before_pattern_size + after_pattern_size;
    patch_file_after_buffer = calloc(patch_file_after_size, sizeof(char));

    patch_buffer_or_die(patch_file_before_buffer, patch_file_before_size,
                 before_pattern_buffer, before_pattern_size,
                 after_pattern_buffer, after_pattern_size,
                 patch_file_after_buffer);
    
    patch_file_fd = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC, 0);
    write(patch_file_fd, patch_file_after_buffer, patch_file_after_size);
    close(patch_file_fd);

    return EXIT_SUCCESS;
}


void read_file_or_die(char *file_name, char **buffer, int *file_size) {
    int file_fd;
    int num_bytes_read;

    file_fd = open(file_name, O_RDONLY, 0);
    if (file_fd == -1) {
        writestr_fd(2, "Could not open file: ");
        writestr_fd(2, file_name);
        writestr_fd(2, "\n");
        exit(1);
    }
    // determine file size
    *file_size = lseek(file_fd, 0, SEEK_END);
    // go back to beginning of file
    lseek(file_fd, 0, SEEK_SET);
    // alloc a buffer to read the entire file
    *buffer = calloc(*file_size, sizeof(char));

    // read the entire patch file
    num_bytes_read = read(file_fd, *buffer, *file_size);
    if (num_bytes_read != *file_size) {
        writestr_fd(2, "Could not read file: ");
        writestr_fd(2, file_name);
        writestr_fd(2, "\n");
        exit(1);
    }
    close(file_fd);
}

void patch_buffer_or_die(char *patch_file_before_buffer, int patch_file_before_size,
                 char *before_pattern_buffer, int before_pattern_size,
                 char *after_pattern_buffer, int after_pattern_size,
                 char *patch_file_after_buffer) {

    char *pos = patch_file_before_buffer;
    int prefix_len = 0;

    // look for the pattern at every offset
    while (prefix_len < patch_file_before_size) {
        // if we find the pattern, replace it and return
        if (memsame(pos, patch_file_before_size - prefix_len, before_pattern_buffer, before_pattern_size)) {
           memcpy(patch_file_after_buffer, patch_file_before_buffer, prefix_len);
           memcpy(patch_file_after_buffer + prefix_len, after_pattern_buffer, after_pattern_size);
           memcpy(patch_file_after_buffer + prefix_len + after_pattern_size, 
                  patch_file_before_buffer + prefix_len + before_pattern_size, 
                  patch_file_before_size - (prefix_len + before_pattern_size));
           return;
        }
        pos = pos + 1;
        prefix_len = prefix_len + 1;
    }

    /* if we don't find the pattern, something is wrong, so exit with error */
    exit(1);
}

/*
    Write the string to the given file descriptor.
*/
void writestr_fd(int fd, char *str) {
    write(fd, str, strlen(str));
}

/*
    Is the pattern located at the start of the search buffer
    (and not exceeding the length of the search buffer)?
*/

int memsame(char *search_buffer, int search_size,
            char *pattern_buffer, int pattern_size) {
    int check_offset = 0;

    if (pattern_size > search_size) {
        return FALSE;
    }
    while (check_offset < pattern_size) {
        if (search_buffer[check_offset] != pattern_buffer[check_offset]) {
             return FALSE;
        }
        check_offset = check_offset + 1;
    }
    return TRUE;
}
