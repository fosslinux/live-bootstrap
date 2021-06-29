#!/bin/sh

# SPDX-FileCopyrightText: 2006 Brian Brazil
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-2.0-or-later

function clean_kconfig {
    perl -i -ne 'BEGIN{$p=1} if(/^config ('$2')$/){$p=0}elsif(/^(config|endmenu|source)/){$p=1}; print if $p' $1
}

#FORE uses pca200e.data, pca200e_ecd.data and sba200e_ecd.data
clean_kconfig ./drivers/atm/Kconfig 'ATM_(AMBASSADOR|FORE).*'
sed -i '/CONFIG_ATM_AMBASSADOR/d' ./drivers/atm/Makefile
sed -i '/CONFIG_ATM_FORE200E)/d' ./drivers/atm/Makefile

clean_kconfig ./drivers/char/drm/Kconfig 'DRM_(MGA|R128|RADEON)'
sed -i '/\(mga\|r128\|radeon\)(\.o\|-objs)/d' ./drivers/char/drm/Makefile

#No kconfig for dsp56
clean_kconfig ./drivers/char/Kconfig 'COMPUTONE'
sed -i '/ATARI_DSP56K\|COMPUTONE/d' ./drivers/char/drm/Makefile

clean_kconfig ./drivers/media/dvb/frontends/Kconfig 'DVB_(TDA10021|TDA8083|TDA80XX|VES1820|VES1X93)'
sed -i '/CONFIG_DVB_\(TDA10021\|TDA8083\|TDA80XX\|VES1820\|VES1X93\)/d' ./drivers/media/dvb/frontends/Makefile

clean_kconfig ./drivers/media/dvb/ttpci/Kconfig 'DVB_(AV7110.*|BUDGET_CI|BUDGET_AV)'
sed -i '/CONFIG_\(AV7110\|BUDGET_CI\|BUDGET_AV\)/d' ./drivers/media/dvb/ttpci/Makefile

echo > ./drivers/media/dvb/ttusb-budget/Kconfig
echo > ./drivers/media/dvb/ttusb-budget/Makefile
sed -i 's#ttusb-budget/##' ./drivers/media/dvb/Makefile

#STRADIS uses cs8420.h
clean_kconfig ./drivers/media/video/Kconfig 'VIDEO_STRADIS'
sed -i '/CONFIG_VIDEO_STRADIS/d' ./drivers/media/video/Makefile

clean_kconfig ./drivers/net/Kconfig 'ACENIC|ACENIC_OMIT_TIGON_I|BNX2|CASSINI|DGRS|E100|ADAPTEC_STARFIRE|ADAPTEC_STARFIRE_NAPI|TYPHOON|TIHON3'
sed -i '/\(acenic\|bnx2\|cassini\|dgrs\|e100\|starfire\|typhoon\|tg3\)\.o/d' ./drivers/net/Makefile

clean_kconfig ./drivers/net/appletalk/Kconfig 'COPS.*'
sed -i '/CONFIG_COPS/d' ./drivers/net/appletalk/Makefile

#YAM uses yam1200.h and yam9600.h
clean_kconfig ./drivers/net/hamradio/Kconfig 'YAM'
sed -i '/CONFIG_YAM/d' ./drivers/net/hamradio/Makefile

clean_kconfig ./drivers/net/pcmcia/Kconfig 'PCMCIA_SMC91C92'
sed -i '/CONFIG_PCMCIA_SMC91C92/d' ./drivers/net/pcmcia/Makefile

clean_kconfig ./drivers/net/tokenring/Kconfig 'SMCTR|3C359'
sed -i '/CONFIG_\(SMCTR\|3C359\)/d' ./drivers/net/tokenring/Makefile

#CYCLADES_SYNC uses sdladrv.c
#WANXL uses wanxlfw.inc_shipped
clean_kconfig ./drivers/net/wan/Kconfig 'CYCLADES_SYNC|WANXL.*'
sed -i '/CONFIG_\(CYCLADES_SYNC\|WANXL)\)/d' ./drivers/net/wan/Makefile

#QETH uses qeth_mpc.c
clean_kconfig ./drivers/s390/net/Kconfig 'QETH.*'
sed -i '/^qeth-/d' ./drivers/s390/net/Makefile

#AIC79XX uses aic79xx_seq.h_shipped
echo > ./drivers/scsi/aic7xxx/Kconfig.aic79xx
sed -i '/aic79xx/d' ./drivers/scsi/aic7xxx/Makefile

#QLOGIC_1280 uses ql1040_fw.h, ql12160_fw.h and ql1280_fw.h
clean_kconfig ./drivers/scsi/Kconfig 'SCSI_QLOGIC(_1280|_FC.*|PTI)'
sed -i '/qla1280\.o/d; /CONFIG_SCSI_QLOGIC\(_1280\|_FC\|PTI\)/d' ./drivers/scsi/Makefile

echo > ./drivers/scsi/qla2xxx/Kconfig
echo > ./drivers/scsi/qla2xxx/Makefile
sed -i '/CONFIG_SCSI_QLA2XXX/d' ./drivers/scsi/Makefile

clean_kconfig ./drivers/serial/Kconfig 'SERIAL_JSM'
echo > ./drivers/serial/jsm/Makefile
sed -i '/CONFIG_SERIAL_JSM/d' ./drivers/pci/Makefile

#DABUSB uses dabfirmware.h
#PWC uses pwc-kiara.c and pwc-timon.c
clean_kconfig ./drivers/usb/media/Kconfig 'USB_(DABUSB|IBMCAM|VICAM|PWC|SPCA5XX)'
sed -i '/CONFIG_USB_\(DABUSB\|IBMCAM\|VICAM\|PWC\|\SPCA5XX\)/d' ./drivers/usb/media/Makefile

#EMI26 uses emi26_fw.h
#EMI62 uses emi62_fw_m.h and emi62_fw_s.h
clean_kconfig ./drivers/usb/misc/Kconfig 'USB_EMI(26|62)'
sed -i '/CONFIG_USB_EMI\(26\|62\)/d' ./drivers/usb/misc/Makefile

echo > ./drivers/usb/misc/sisusbvga/Kconfig
echo > ./drivers/usb/misc/sisusbvga/Makefile
sed -i '/CONFIG_USB_SISUSBVGA/d' ./drivers/usb/misc/Makefile

#KAWETH uses kawethfw.h
clean_kconfig ./drivers/usb/net/Kconfig 'USB_(ATMEL|EAGLE|KAWETH|ZD1211)'
sed -i '/CONFIG_USB_\(ATMEL\|EAGLE\|KAWETH\|ZD1211\)/d' ./drivers/usb/net/Makefile

#EDGEPORT uses io_fw_boot.h, io_fw_boot2.h, io_fw_down.h and io_fw_down2.h
#EDGEPORT_TI uses io_fw_down3.h
#TI uses ti_fw_3410.h, ti_fw_5052.h
#WHITEHEAT uses whiteheat_fw.h
#XIRCOM uses xircom_pgs_fw.h
#KEYSPAN/KEYSPAN_PDA uses the rest *and* xircom_pgs_fw.h
clean_kconfig ./drivers/usb/serial/Kconfig 'USB_SERIAL_(EDGEPORT(_TI)?|KEYSPAN.*|TI||WHITEHEAT|XIRCOM)'
sed -i '/CONFIG_USB_SERIAL_\(EDGEPORT\(_TI\)\?\|KEYSPAN\|TI\|WHITEHEAT\|XIRCOM\)/d' ./drivers/usb/serial/Makefile

#chipsfb.c is unused
clean_kconfig ./drivers/video/Kconfig 'FB_ASILIANT'
sed -i '/CONFIG_FB_ASILIANT/d' ./drivers/video/Makefile

clean_kconfig ./sound/pci/Kconfig 'SND_(KORG1212|MAESTRO3|YMFPCI|CS46XX.*)'
echo > ./sound/pci/cs46xx/Makefile
echo > ./sound/pci/korg1212/Makefile
echo > ./sound/pci/ymfpci/Makefile
sed -i '/cs46xx\//d' ./sound/pci/Makefile
sed -i '/korg1212\//d' ./sound/pci/Makefile
sed -i '/ymfpci\//d' ./sound/pci/Makefile
sed -i '/SND_MAESTRO3/d' ./sound/pci/Makefile

#FUSION uses  cs461x_image.h
clean_kconfig ./sound/oss/Kconfig 'SOUND_(FUSION|MAESTRO3|YMFPCI)'
sed -i '/SOUND_FUSION/d' ./sound/pci/Makefile
sed -i '/SOUND_MAESTRO3/d' ./sound/pci/Makefile
sed -i '/ymfpci.o/d' ./sound/pci/Makefile
