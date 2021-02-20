/*
 * SPDX-FileCopyrightText: 2016 Jeremiah Orians
 * SPDX-FileCopyrightText: 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#define FALSE 0
// CONSTANT FALSE 0
#define TRUE 1
// CONSTANT TRUE 1

int in_set(int c, char* s)
{
	while(0 != s[0])
	{
		if(c == s[0]) return TRUE;
		s = s + 1;
	}
	return FALSE;
}
