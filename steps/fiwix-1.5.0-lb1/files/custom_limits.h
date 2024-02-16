/* SPDX-FileCopyrightText: 2024 Richard Masters <grick23@gmail.com> */
/* SPDX-License-Identifier: MIT */
#undef CHILD_MAX
#define CHILD_MAX	4096
#undef OPEN_MAX
#define OPEN_MAX	4096
#undef FD_SETSIZE
#define FD_SETSIZE	OPEN_MAX 
