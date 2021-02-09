/*
 * SPDX-FileCopyrightText: 2016 Jeremiah Orians
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include<stdlib.h>
#include<stdio.h>
#define MAX_STRING 4096
//CONSTANT MAX_STRING 4096
// void* calloc(int count, int size);
void file_print(char* s, FILE* f);

char* copy_string(char* target, char* source)
{
	while(0 != source[0])
	{
		target[0] = source[0];
		target = target + 1;
		source = source + 1;
	}
	return target;
}

char* postpend_char(char* s, char a)
{
	char* ret = calloc(MAX_STRING, sizeof(char));
	if(NULL == ret) return NULL;

	char* hold = copy_string(ret, s);
	hold[0] = a;
	return ret;
}

char* prepend_char(char a, char* s)
{
	char* ret = calloc(MAX_STRING, sizeof(char));
	if(NULL == ret) return NULL;

	ret[0] = a;
	copy_string((ret+1), s);
	return ret;
}

char* prepend_string(char* add, char* base)
{
	char* ret = calloc(MAX_STRING, sizeof(char));
	if(NULL == ret) return NULL;

	copy_string(copy_string(ret, add), base);
	return ret;
}

int string_length(char* a)
{
	int i = 0;
	while(0 != a[i]) i = i + 1;
	return i;
}
