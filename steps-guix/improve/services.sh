#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

mkdir -p /etc

cat > /etc/services <<'EOF'
http            80/tcp
http            80/udp
https           443/tcp
https           443/udp
EOF
