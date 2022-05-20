#!/bin/bash

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: MIT

# Replace this hook if you wish to do more

exec env - PATH="${PREFIX}/bin" PS1="\w # " bash -i
