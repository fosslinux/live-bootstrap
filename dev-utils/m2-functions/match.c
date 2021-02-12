/*
 * SPDX-FileCopyrightText: 2016 Jeremiah Orians
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#define FALSE 0
// CONSTANT FALSE 0
#define TRUE 1
// CONSTANT TRUE 1

int match(char* a, char* b)
{
	int i = -1;
	do
	{
		i = i + 1;
		if(a[i] != b[i])
		{
			return FALSE;
		}
	} while((0 != a[i]) && (0 !=b[i]));
	return TRUE;
}
