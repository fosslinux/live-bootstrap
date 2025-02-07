// SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
// SPDX-License-Identifier: GPL-3.0-or-later

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

/* 
 * Creates decDPD.h.
 * Based upon the algorithm given on;
 * https://web.archive.org/web/20080308073422/http://www2.hursley.ibm.com/decimal/DPDecimal.html
 * originally written in the (obsolete) language Rexx.
 *
 * Is not bit-for-bit identical to the original decDPD.h, as we don't bother
 * to follow the same formatting.
 *
 * The original Rexx code follows;
 */

// /* dpdGenerate.rex -- display Densely Packed Decimal table    */
// /* mfc 2000.10.03; Rexx version with new equations 2007.02.01 */
// 
// do i=0 to 999
//   bcd=right(i, 3, 0)      -- make three-digit hexadecimal string
//   bit10=bcd2dpd(x2b(bcd)) -- compress
//   bit12=dpd2bcd(bit10)    -- expand
//   say bcd bit10 bit12     -- display
//   end i
// exit
// 
// /* bcd2dpd -- Compress BCD to Densely Packed Decimal
//    argument is a string of 12 characters, each 0 or 1, being 3 digits
//             of 4 bits, each being a valid BCD digit (0000-1001)
//             (for example, 923 is 100100100011)
//    result   is a string of 10 characters, each 0 or 1
//             (for the example, this would be 1001010011)
//    */
// bcd2dpd: procedure
//   -- assign each bit to a variable, named as in the description
//   parse arg a +1 b +1 c +1 d +1 e +1 f +1 g +1 h +1 i +1 j +1 k +1 m +1
// 
//   -- derive the result bits, using boolean expressions only
//   -- [the operators are: '&'=AND, '|'=OR, '\'=NOT.]
//   p=b | (a & j) | (a & f & i)
//   q=c | (a & k) | (a & g & i)
//   r=d
//   s=(f & (\a | \i)) | (\a & e & j) | (e & i)
//   t=g  | (\a & e &k;) | (a & i)
//   u=h
//   v=a | e | i
//   w=a | (e & i) | (\e & j)
//   x=e | (a & i) | (\a & k)
//   y=m
//   -- concatenate the bits and return
//   return p||q||r||s||t||u||v||w||x||y
// 
// /* dpd2bcd -- Expand Densely Packed Decimal to BCD
//    argument is a string of 10 characters, each 0 or 1; all 1024
//             possibilities are accepted (non-canonicals -> 999)
//             (for example, 1001010011)
//    result   is a string of 12 characters, each 0 or 1
//             (for the example, 100100100011 -- 923)
//    */
// dpd2bcd: procedure
//   -- assign each bit to a variable, named as in the description
//   parse arg p +1 q +1 r +1 s +1 t +1 u +1 v +1 w +1 x +1 y +1
// 
//   -- derive the result bits, using boolean expressions only
//   a= (v & w) & (\s | t | \x)
//   b=p & (\v | \w | (s & \t & x))
//   c=q & (\v | \w | (s & \t & x))
//   d=r
//   e=v & ((\w & x) | (\t & x) | (s & x))
//   f=(s & (\v | \x)) | (p & \s & t & v & w & x)
//   g=(t & (\v | \x)) | (q & \s & t & w)
//   h=u
//   i=v & ((\w & \x) | (w & x & (s | t)))
//   j=(\v & w) | (s & v & \w & x) | (p & w & (\x | (\s & \t)))
//   k=(\v & x) | (t & \w & x) | (q & v & w & (\x | (\s & \t)))
//   m=y
//   -- concatenate the bits and return
//   return a||b||c||d||e||f||g||h||i||j||k||m

void int2boolarr(uint32_t num, bool *arr, int bits) {
	int j = 0;
	for (int i = bits - 1; i >= 0; i--) {
		arr[j++] = (num >> i) & 0x1;
	}
}

uint32_t boolarr2int(bool *dpd, int bits) {
	uint32_t num = 0;
	int j = 0;
	for (int i = bits - 1; i >= 0; i--) {
		num |= dpd[j++] << i;
	}
	return num;
}

uint32_t bcd2dpd(uint16_t ibcd) {
	bool bcd[12];
	int2boolarr(ibcd, bcd, 12);

	bool dpd[10];
	dpd[0] = bcd[1] | (bcd[0] & bcd[9]) | (bcd[0] & bcd[5] & bcd[8]);
	dpd[1] = bcd[2] | (bcd[0] & bcd[10]) | (bcd[0] & bcd[6] & bcd[8]);
	dpd[2] = bcd[3];
	dpd[3] = (bcd[5] & (~bcd[0] | ~bcd[8])) | (~bcd[0] & bcd[4] & bcd[9]) | (bcd[4] & bcd[8]);
	dpd[4] = bcd[6] | (~bcd[0] & bcd[4] & bcd[10]) | (bcd[0] & bcd[8]);
	dpd[5] = bcd[7];
	dpd[6] = bcd[0] | bcd[4] | bcd[8];
	dpd[7] = bcd[0] | (bcd[4] & bcd[8]) | (~bcd[4] & bcd[9]);
	dpd[8] = bcd[4] | (bcd[0] & bcd[8]) | (~bcd[0] & bcd[10]);
	dpd[9] = bcd[11];

	return boolarr2int(dpd, 10);
}

uint32_t dpd2bcd(uint32_t idpd) {
	bool dpd[10];
	int2boolarr(idpd, dpd, 10);

	bool bcd[12];
	bcd[0] = (dpd[6] & dpd[7]) & (~dpd[3] | dpd[4] | ~dpd[8]);
	bcd[1] = dpd[0] & (~dpd[6] | ~dpd[7] | (dpd[3] & ~dpd[4] & dpd[8]));
	bcd[2] = dpd[1] & (~dpd[6] | ~dpd[7] | (dpd[3] & ~dpd[4] & dpd[8]));
	bcd[3] = dpd[2];
	bcd[4] = dpd[6] & ((~dpd[7] & dpd[8]) | (~dpd[4] & dpd[8]) | (dpd[3] & dpd[8]));
	bcd[5] = (dpd[3] & (~dpd[6] | ~dpd[8])) | (dpd[0] & ~dpd[3] & dpd[4] & dpd[6] & dpd[7] & dpd[8]);
	bcd[6] = (dpd[4] & (~dpd[6] | ~dpd[8])) | (dpd[1] & ~dpd[3] & dpd[4] & dpd[7]);
	bcd[7] = dpd[5];
	bcd[8] = dpd[6] & ((~dpd[7] & ~dpd[8]) | (dpd[7] & dpd[8] & (dpd[3] | dpd[4])));
	bcd[9] = (~dpd[6] & dpd[7]) | (dpd[3] & dpd[6] & ~dpd[7] & dpd[8]) | (dpd[0] & dpd[7] & (~dpd[8] | (~dpd[3] & ~dpd[4])));
	bcd[10] = (~dpd[6] & dpd[8]) | (dpd[4] & ~dpd[7] & dpd[8]) | (dpd[1] & dpd[6] & dpd[7] & (~dpd[8] | (~dpd[3] & ~dpd[4])));
	bcd[11] = dpd[9];

	return boolarr2int(bcd, 12);
}

uint8_t get_cntrl_char(uint8_t num) {
	if (num == 0) {
		return 0;
	} else if (num < 10) {
		return 1;
	} else if (num < 100) {
		return 2;
	} else {
		return 3;
	}
}

void bin2char(uint8_t num, uint32_t str[4]) {
	str[0] = get_cntrl_char(num);
	str[1] = num / 100 + '0';
	str[2] = (num % 100) / 10 + '0';
	str[3] = num % 10 + '0';
}

void bin2bcd8(uint8_t num, char digit[4], uint32_t arr[4]) {
	for (int i = 0; i < 3; i++) {
		arr[i] = digit[i] - '0';
	}
	arr[3] = get_cntrl_char(num);
}

#define TABLES_COUNT 9

int main(void) {
	uint32_t BCD2DPD[2458] = {0};
	uint32_t BIN2DPD[1000] = {0};
	uint32_t BIN2BCD8[4000];
	uint32_t BIN2CHAR[4001];
	uint32_t DPD2BCD[1024] = {0};
	uint32_t DPD2BIN[1024] = {0};
	uint32_t DPD2BINK[1024] = {0};
	uint32_t DPD2BINM[1024] = {0};
	uint32_t DPD2BCD8[4096];

	for (int i = 0; i < 1000; i++) {
		char digit[4];
		snprintf(digit, 4, "%03d", i);
		uint32_t bcd = 0;
		for (int j = 0; j < 3; j++) {
			bcd |= (digit[j] - '0') << (4 * (2 - j));
		}

		uint32_t dpd = bcd2dpd(bcd);
		BCD2DPD[bcd] = dpd;
		DPD2BCD[dpd] = bcd;
		BIN2DPD[i] = dpd;
		DPD2BIN[dpd] = i;
		DPD2BINK[dpd] = i * 1000;
		DPD2BINM[dpd] = i * 1E+6;

		bin2char(i, BIN2CHAR + (4 * i));
		bin2bcd8(i, digit, BIN2BCD8 + (4 * i));
		bin2bcd8(i, digit, DPD2BCD8 + (4 * dpd));
	}
	BIN2CHAR[4000] = '\0';

	char *names[] = {
		"BCD2DPD", "BIN2DPD", "BIN2CHAR", "BIN2BCD8", "DPD2BCD", "DPD2BIN",
		"DPD2BINK", "DPD2BINM", "DPD2BCD8",
	};
	char *types[] = {
		"uint16_t", "uint16_t", "uint8_t", "uint8_t", "uint16_t", "uint16_t",
		"uint32_t", "uint32_t", "uint8_t",
	};
	uint32_t *data[] = {
		BCD2DPD, BIN2DPD, BIN2CHAR, BIN2BCD8, DPD2BCD, DPD2BIN,
		DPD2BINK, DPD2BINM, DPD2BCD8,
	};
	int lengths[] = {2458, 1000, 4001, 4000, 1024, 1024, 1024, 1024, 4096};

	for (int i = 0; i < TABLES_COUNT; i++) {
		printf("#if defined(DEC_%s) && DEC_%s==1 && !defined(DEC%s)\n", names[i], names[i], names[i]);
		printf("#define DEC%s\n", names[i]);
		printf("const %s %s[%d] = {\n", types[i], names[i], lengths[i]);
		for (int j = 0; j < lengths[i] / 16; j++) {
			for (int k = j * 16; k < (j + 1) * 16 && k < lengths[i]; k++) {
				printf("%s%d,", k == j * 16 ? "" : " ", data[i][k]);
			}
			printf("\n");
		}
		printf("};\n");
		printf("#endif\n\n");
	}
}
