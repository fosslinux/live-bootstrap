/*
 * SPDX-FileCopyrightText: 2016 Jeremiah Orians
 * SPDX-FileCopyrightText: 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include<stdio.h>
#include<stdlib.h>

void file_print(char* s, FILE* f);

void require(int bool, char* error)
{
	if(!bool)
	{
		file_print(error, stderr);
		exit(EXIT_FAILURE);
	}
}
