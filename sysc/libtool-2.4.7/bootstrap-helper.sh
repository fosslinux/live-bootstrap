#!/bin/sh

# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2010-2019, 2021 Bootstrap Authors
# SPDX-FileCopyrightText: 2010-2019, 2021-2022 Free Software Foundation, Inc.
#
# SPDX-License-Identifier: GPL-2.0-or-later

EXIT_FALURE=1

#######################################
#  Extracted from the output of:      #
#      ./bootstrap --verbose --force  #
#######################################

MAKE='make'
SED='/usr/bin/sed'

build_aux='build-aux'
ltdl_dir='libltdl'
macro_dir='m4'

package_name='GNU Libtool'
package='libtool'
package_bugreport='bug-libtool@gnu.org'
package_url='http://www.gnu.org/s/libtool/'
package_version='2.4.7'

#############################
#  Inspired by "bootstrap"  #
#############################

func_show_eval ()
{
    eval "$1"
    _G_status=$?
    if test 0 -ne "$_G_status"; then
      exit $_G_status
    fi
}

##################################
#  Copied from "bootstrap.conf"  #
##################################

# libtool_build_prerequisites
# ---------------------------
# Libtool generates some files that are required before any autotools
# can be run successfully.
libtool_build_prerequisites ()
{
    $debug_cmd

    $require_build_aux
    $require_ltdl_dir
    $require_macro_dir
    $require_package
    $require_package_bugreport
    $require_package_name
    $require_package_url
    $require_package_version

    # Whip up a dirty Makefile:
    makes='Makefile.am libltdl/ltdl.mk'
    rm -f Makefile
    {
      echo "aux_dir = $build_aux"
      echo "ltdl_dir = $ltdl_dir"
      echo "macro_dir = $macro_dir"

      # The following allow us to tie bootstrap-deps output verbosity
      # into the bootstrap --verbose option:
      echo 'AM_V_GEN = $(am__v_GEN_$(V))'
      echo 'am__v_GEN_ = $(am__v_GEN_$(AM_DEFAULT_VERBOSITY))'
      echo 'am__v_GEN_0 = @echo "  GEN     " $@;'
      echo 'AM_V_at = $(am__v_at_$(V))'
      echo 'am__v_at_ = $(am__v_at_$(AM_DEFAULT_VERBOSITY))'
      echo 'am__v_at_0 = @'

      $SED '/^if /,/^endif$/d;/^else$/,/^endif$/d;/^include /d' $makes
    } > Makefile

    # Building distributed files from configure is bad for automake, so we
    # generate them here, and have Makefile rules to keep them up to date.
    func_show_eval "$MAKE V=1 bootstrap-deps \
        AM_DEFAULT_VERBOSITY=0 `$opt_verbose && echo V=1` \
        PACKAGE='$package' PACKAGE_BUGREPORT='$package_bugreport' \
        PACKAGE_NAME='$package_name' PACKAGE_URL='$package_url' \
        SED='$SED' srcdir=. VERSION='$package_version'"
    status=$?

    rm -f Makefile
    test 0 -eq "$status" ||exit $EXIT_FAILURE
}

###################
#  Run functions  #
###################

libtool_build_prerequisites
