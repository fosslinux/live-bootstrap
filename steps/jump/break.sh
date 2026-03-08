#!/bin/bash
#
# SPDX-FileCopyrightText: 2023 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

resume_next_path="/steps/.lb-resume-next"

if [ -n "${LB_RESUME_NEXT_SCOPE:-}" ] && [ -n "${LB_RESUME_NEXT_PACKAGE:-}" ]; then
    mkdir -p "$(dirname "${resume_next_path}")"
    cat > "${resume_next_path}" <<EOF
scope=${LB_RESUME_NEXT_SCOPE}
package=${LB_RESUME_NEXT_PACKAGE}
EOF
fi

exit 0
