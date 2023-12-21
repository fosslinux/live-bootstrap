# SPDX-FileCopyrightText: 2023 Eduardo Sánchez Muñoz <eduardosm-dev@e64.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later

pushd /external/repo
sha256sum -- * | tee "${SRCDIR}/SHA256SUMS.pkgs"
popd
