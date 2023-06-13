/*
 * SPDX-FileCopyrightText: 2020 Timo Teräs <timo.teras@iki.fi>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

extern void __stack_chk_fail(void);
void __attribute__((visibility ("hidden"))) __stack_chk_fail_local(void) { __stack_chk_fail(); }
