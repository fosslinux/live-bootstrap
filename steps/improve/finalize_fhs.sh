#!/bin/sh
# Add the rest of the FHS that we will use and is not created pre-boot
rm -rf /sbin /usr/sbin
for d in bin lib sbin; do
    ln -s "usr/${d}" "/${d}"
done
mkdir -p /etc /run /var
test -d /proc || (mkdir /proc && mount -t proc proc /proc)
test -d /sys || (mkdir /sys && mount -t sysfs sysfs /sys)
# Make /tmp a ramdisk (speeds up configure etc significantly)
test -d /tmp || (mkdir /tmp && mount -t tmpfs tmpfs /tmp)
# Add /etc/resolv.conf
echo 'nameserver 1.1.1.1' > /etc/resolv.conf
