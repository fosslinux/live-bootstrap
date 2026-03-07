# SPDX-FileCopyrightText: 2026 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later
 
### Creation of gnulib's uninames.h from the UnicodeData.txt and NameAliases.txt
### tables.

### Written by Bruno Haible <bruno@clisp.org>, 2000-12-28.
### Translated into Python by Samuel Tyler, 2026-01-31.
###
### This program is free software.
### It is dual-licensed under "the GNU LGPLv3+ or the GNU GPLv2+".
### You can redistribute it and/or modify it under either
###   - the terms of the GNU Lesser General Public License as published
###     by the Free Software Foundation, either version 3, or (at your
###     option) any later version, or
###   - the terms of the GNU General Public License as published by the
###     Free Software Foundation; either version 2, or (at your option)
###     any later version, or
###   - the same dual license "the GNU LGPLv3+ or the GNU GPLv2+".
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### Lesser General Public License and the GNU General Public License
### for more details.
###
### You should have received a copy of the GNU Lesser General Public
### License and of the GNU General Public License along with this
### program.  If not, see <https://www.gnu.org/licenses/>.

import sys

add_comments = False

class UnicodeChar:
    def __init__(self, index, name):
        self.index = index
        self.name = name
        self.word_indices = []
        self.word_indices_index = 0

class Range:
    def __init__(self, index, start_code, end_code):
        self.index = index
        self.start_code = start_code
        self.end_code = end_code

class WordList:
    def __init__(self):
        self.hashed = {}
        self.sorted = []
        self.size = 0
        self.length = 0

def main(inputfile, aliasfile, outputfile):
    # Local variable initialization
    all_chars = []
    all_chars_hashed = {}
    all_aliases = []
    all_chars_and_aliases = []
    all_ranges = []
    name_index = 0
    current_range = None

    # Read all characters and names from the input file.
    with open(inputfile, "r", encoding="utf-8") as istream:
        for line in istream:
            line = line.strip("\n")
            if not line:
                continue

            code_string, name_string = line.split(";", 1)
            code = int(code_string, 16)

            # Ignore characters whose name starts with "<".
            if name_string.startswith('<'):
                continue

            # Also ignore Hangul syllables; they are treated specially.
            if 0xAC00 <= code <= 0xD7A3:
                continue

            # Also ignore CJK compatibility ideographs; they are treated
            # specially as well.
            if (0xF900 <= code <= 0xFA2D) or (0xFA30 <= code <= 0xFA6A) or \
               (0xFA70 <= code <= 0xFAD9) or (0x2F800 <= code <= 0x2FA1D):
                continue

            # Also ignore variationselectors; they are treated
            # specially as well.
            if (0xFE00 <= code <= 0xFE0F) or (0xE0100 <= code <= 0xE01EF):
                continue

            uc = UnicodeChar(name_index, name_string)
            all_chars.insert(0, uc)
            all_chars_hashed[code] = uc

            # Update the contiguous range, or start a new range.
            if current_range and (current_range.end_code + 1 == code):
                current_range.end_code = code
            else:
                if current_range:
                    all_ranges.insert(0, current_range)
                current_range = Range(name_index, code, code)
            name_index += 1

    all_chars.reverse()
    if current_range:
        all_ranges.insert(0, current_range)
    all_ranges.reverse()

    if aliasfile:
        # Read all characters and names from the alias file.
        with open(aliasfile, "r", encoding="utf-8") as istream:
            for line in istream:
                line = line.strip("\n")
                if not line or line == "" or line.startswith('#'):
                    continue

                code_string, name_string = line.split(";", 1)
                code = int(code_string, 16)

                if code in all_chars_hashed:
                    uc_alias = UnicodeChar(all_chars_hashed[code].index, name_string)
                    all_aliases.insert(0, uc_alias)

    all_aliases.reverse()
    all_chars_and_aliases = all_chars + all_aliases

    # Split into words.
    words_by_length = []
    for name in ["HANGUL SYLLABLE", "CJK COMPATIBILITY", "VARIATION"] + \
        [c.name for c in all_chars_and_aliases]:
        i1 = 0
        while i1 < len(name):
            i2 = name.find(' ', i1)
            if i2 == -1:
                i2 = len(name)

            word = name[i1:i2]
            while len(word) >= len(words_by_length):
                words_by_length.append(WordList())

            word_list = words_by_length[len(word)]
            if word not in word_list.hashed:
                word_list.hashed[word] = True
                word_list.sorted.append(word)

            i1 = i2 + 1

    # Sort the word lists.
    for length in range(len(words_by_length)):
        if not words_by_length[length]:
            words_by_length[length] = WordList()

        word_list = words_by_length[length]
        word_list.sorted.sort()
        word_list.size = sum(len(w) for w in word_list.sorted)
        word_list.length = len(word_list.sorted)

    # Output the tables.
    with open(outputfile, 'w', encoding='ascii') as ostream:
        ostream.write("/* DO NOT EDIT! GENERATED AUTOMATICALLY! */\n")
        ostream.write("/*\n")
        ostream.write(f" * {outputfile}\n")
        ostream.write(" *\n")
        ostream.write(" * Unicode character name table.\n")
        ostream.write(" * Generated automatically by the gen-uninames utility.\n")
        ostream.write(" */\n")
        ostream.write("/* Copyright (C) 2000-2024 Free Software Foundation, Inc.\n")
        ostream.write("\n")
        ostream.write("   This file is free software.\n")
        ostream.write('   It is dual-licensed under "the GNU LGPLv3+ or the GNU GPLv2+".\n')
        ostream.write("   You can redistribute it and/or modify it under either\n")
        ostream.write("     - the terms of the GNU Lesser General Public License as published\n")
        ostream.write("       by the Free Software Foundation, either version 3, or (at your\n")
        ostream.write("       option) any later version, or\n")
        ostream.write("     - the terms of the GNU General Public License as published by the\n")
        ostream.write("       Free Software Foundation; either version 2, or (at your option)\n")
        ostream.write("       any later version, or\n")
        ostream.write('     - the same dual license "the GNU LGPLv3+ or the GNU GPLv2+".\n')
        ostream.write("\n")
        ostream.write("   This file is distributed in the hope that it will be useful,\n")
        ostream.write("   but WITHOUT ANY WARRANTY; without even the implied warranty of\n")
        ostream.write("   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n")
        ostream.write("   Lesser General Public License and the GNU General Public License\n")
        ostream.write("   for more details.\n")
        ostream.write("\n")
        ostream.write("   You should have received a copy of the GNU Lesser General Public\n")
        ostream.write("   License and of the GNU General Public License along with this\n")
        ostream.write("   program.  If not, see <https://www.gnu.org/licenses/>.  */\n")
        ostream.write("\n")

        total_size = sum(wl.size for wl in words_by_length)
        ostream.write(f"static const char unicode_name_words[{total_size}] = {{\n")

        for wl in words_by_length:
            for word in wl.sorted:
                # format " ~{ '~C',~}"
                # space before loop, print each char in single quotes followed by comma
                chars = "".join(f"'{c}'," for c in word)
                ostream.write(f" {chars}\n")
        ostream.write("};\n")

        total_num_words = sum(wl.length for wl in words_by_length)
        ostream.write(f"#define UNICODE_CHARNAME_NUM_WORDS {total_num_words}\n")

        # unicode_name_by_length
        ostream.write(
            "static const struct { uint32_t extra_offset; uint16_t ind_offset; } "
            f"unicode_name_by_length[{len(words_by_length) + 1}] = {{\n"
        )

        extra_offset = 0
        ind_offset = 0
        for wl in words_by_length:
            ostream.write(f"  {{ {extra_offset}, {ind_offset} }},\n")
            extra_offset += wl.size
            ind_offset += wl.length
        ostream.write(f"  {{ {extra_offset}, {ind_offset} }}\n")
        ostream.write("};\n")

        # Assign indices to hashed words
        current_idx = 0
        for wl in words_by_length:
            for word in wl.sorted:
                wl.hashed[word] = current_idx
                current_idx += 1

        # Defines specific words
        for word in ["HANGUL", "SYLLABLE", "CJK", "COMPATIBILITY", "VARIATION"]:
            wlen = len(word)
            idx = words_by_length[wlen].hashed.get(word)
            ostream.write(f"#define UNICODE_CHARNAME_WORD_{word} {idx}\n")

        # Compute word-indices for every unicode-char
        for uc in all_chars_and_aliases:
            indices = []
            i1 = 0
            name = uc.name
            while i1 < len(name):
                i2 = name.find(' ', i1)
                if i2 == -1:
                    i2 = len(name)
                word = name[i1:i2]
                wlen = len(word)
                idx = words_by_length[wlen].hashed[word]
                indices.append(idx)
                i1 = i2 + 1
            uc.word_indices = list(reversed(indices))

        # Sort the list of unicode-chars by word-indices
        all_chars_and_aliases.sort(key=lambda x: x.word_indices)

        # Output the word-indices
        total_indices = sum(len(uc.word_indices) for uc in all_chars_and_aliases)
        ostream.write(f"static const uint16_t unicode_names[{total_indices}] = {{\n")

        i = 0
        for uc in all_chars_and_aliases:
            packed_indices = []
            wi = uc.word_indices
            for k, val in enumerate(wi):
                is_last = (k == len(wi) - 1)
                packed_indices.append(val * 2 + is_last)

            ostream.write(" " + " ".join(f"{val}," for val in packed_indices))

            if add_comments:
                ostream.write(f"{' ' * (40 - len(indices_str))}/* {uc.name} */")
            ostream.write("\n")

            uc.word_indices_index = i
            i += len(uc.word_indices)
        ostream.write("};\n")

        ostream.write(
            "static const struct { uint16_t index; uint32_t name:24; } ATTRIBUTE_PACKED "
            f"unicode_name_to_index[{len(all_chars_and_aliases)}] = {{\n"
        )
        for uc in all_chars_and_aliases:
            content = f"  {{ 0x{uc.index:04X}, {uc.word_indices_index} }},"
            ostream.write(content)
            if add_comments:
                ostream.write(f"{' ' * (21 - len(content))}/* {uc.name} */")
            ostream.write("\n")
        ostream.write("};\n")

        ostream.write(
            f"static const struct {{ uint16_t index; uint32_t name:24; }} ATTRIBUTE_PACKED "
            f"unicode_index_to_name[{len(all_chars)}] = {{\n"
        )
        for uc in sorted(all_chars, key=lambda c: c.index):
            content = f"  {{ 0x{uc.index:04X}, {uc.word_indices_index} }},"
            ostream.write(content)
            if add_comments:
                ostream.write(f"{' ' * (21 - len(content))}/* {uc.name} */")
            ostream.write("\n")
        ostream.write("};\n")

        # Max counts
        max_len = max(len(uc.name) for uc in all_chars_and_aliases)
        ostream.write(f"#define UNICODE_CHARNAME_MAX_LENGTH {max_len}\n")

        max_words = max(len(uc.word_indices) for uc in all_chars_and_aliases)
        ostream.write(f"#define UNICODE_CHARNAME_MAX_WORDS {max_words}\n")

        # Ranges
        ostream.write(
            "static const struct { uint16_t index; uint32_t gap; uint16_t length; } "
            f"unicode_ranges[{len(all_ranges)}] = {{\n"
        )
        for r in all_ranges:
            ostream.write(
                f"  {{ {r.index}, {r.start_code - r.index}, {1 + r.end_code - r.start_code} }},\n"
            )
        ostream.write("};\n")


if __name__ == "__main__":
    if len(sys.argv) >= 4:
        main(sys.argv[1], sys.argv[2], sys.argv[3])
    else:
        print("Usage: script.py <inputfile> <aliasfile> <outputfile>", file=sys.stderr)
