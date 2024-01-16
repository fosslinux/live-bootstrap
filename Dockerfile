FROM scratch as build
ADD target/ /
RUN ["/bootstrap-seeds/POSIX/x86/kaem-optional-seed"]

FROM build as install
ENV PATH=/bin:/usr/sbin:/usr/bin
RUN set -eux; \
    rm -rf /usr/lib/python*/__pycache__; \
    mkdir -p /rootfs/etc /rootfs/home/user; \
    cp -R $(ls -d /etc/* | grep -v '\(resolv.conf\|hosts\)') /rootfs/etc/; \
    cp -R lib usr bin var /rootfs/; \
    echo "user:x:1000:" > /rootfs/etc/group; \
    echo "user:x:1000:1000::/home/user:/bin/bash" > /rootfs/etc/passwd; \
    find /rootfs -exec touch -hcd "@0" "{}" +

FROM scratch as package
COPY --from=install /rootfs /
USER 1000:1000
ENTRYPOINT ["/bin/bash"]
ENV TZ=UTC
ENV LANG=C.UTF-8
ENV SOURCE_DATE_EPOCH=1
ENV KCONFIG_NOTIMESTAMP=1
ENV PS1="bootstrap$ "
