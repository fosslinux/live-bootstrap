# SPDX-FileCopyrightText: 2024 Lance Vick <lance@vick.house>
# SPDX-FileCopyrightText: 2025 Kevin Nause <kevin@nause.engineering>
#
# SPDX-License-Identifier: GPL-3.0-or-later

ARG ARCH=x86
ARG USER=user
ARG UID=1000
ARG GID=1000
ARG HOME=/home/${USER}

FROM scratch AS build
ARG TARGET
ARG INIT
COPY ${TARGET} /

FROM build AS build-aarch64
RUN ["/bootstrap-seeds/POSIX/AArch64/kaem-optional-seed"]

FROM build AS build-amd64
RUN ["/bootstrap-seeds/POSIX/AMD64/kaem-optional-seed"]

FROM build AS build-riscv32
RUN ["/bootstrap-seeds/POSIX/riscv32/kaem-optional-seed"]

FROM build AS build-riscv64
RUN ["/bootstrap-seeds/POSIX/riscv64/kaem-optional-seed"]

FROM build AS build-x86
RUN ["/bootstrap-seeds/POSIX/x86/kaem-optional-seed"]

FROM build-${ARCH} AS install
ARG USER
ARG UID
ARG GID
ARG HOME
ENV PATH=/bin:/usr/sbin:/usr/bin
RUN set -eu; \
    rm -rf /usr/lib/python*/__pycache__; \
    mkdir -p /rootfs/etc /rootfs/${HOME}; \
    cp -R $(ls -d /etc/* | grep -v '\(resolv.conf\|hosts\)') /rootfs/etc/; \
    cp -R lib usr bin var /rootfs/; \
    echo "${USER}:x:${GID}:" > /rootfs/etc/group; \
    echo "${USER}:x:${UID}:${GID}::${HOME}:/bin/bash" > /rootfs/etc/passwd; \
    find /rootfs -exec touch -hcd "@0" "{}" +

FROM scratch AS package
ARG UID
ARG GID
COPY --from=install /rootfs /
USER ${UID}:${GID}
ENTRYPOINT ["/bin/bash"]
ENV TZ=UTC
ENV LANG=C.UTF-8
ENV SOURCE_DATE_EPOCH=1
ENV KCONFIG_NOTIMESTAMP=1
ENV PS1="bootstrap$ "
