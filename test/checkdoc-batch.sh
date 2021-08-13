#!/bin/sh

# emacs-checkdoc-batch -- run checkdoc-batch.el on lisp files
#
# Copyright 2010 Kevin Ryde
#
# emacs-checkdoc-batch is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# emacs-checkdoc-batch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You can get a copy of the GNU General Public License online at
# <http://www.gnu.org/licenses/>.

# set -x

# Normally in -batch mode .emacs is not read, make that happen by passing
# "-u" by default.
#
lispfile=checkdoc-batch.el
args=""
user="-u `whoami`"

while [ $# -gt 0 ]
do
  case $1 in
    --lispdir=*)
      lispfile="`echo $1 | sed s/^--lispdir=//`"
      lispfile="$lispfile/checkdoc-batch.el"
      shift ;;
    --lispdir)
      lispfile="$1/checkdoc-batch.el"
      shift ;;
    -q|-Q)
      user=""
      args="$args $1"
      shift ;;
    -u)
      user="$1 $2"
      shift 2 ;;
    --user=*)
      user="$1"
      shift ;;
    --)
      shift
      break ;;
    -*)
      args="$args $1"
      shift ;;
    *)
      break ;;
  esac
done

if [ $# -eq 0 ]
then
  cat 1>&2 <<'HERE'
Usage: emacs-checkdoc-batch [-options] [--] filename...

Run checkdoc-batch.el on each given filename.
By default ~/.emacs and site-start.el is loaded.
The usual Emacs options can control this,
   -q   	don't load .emacs
   -Q   	don't load .emacs or site-start.el
   -u username  load this user's .emacs
   --           end options, only filenames follow
other options are passed through to Emacs too.

Read more in the emacs-checkdoc-batch(1) man page.

The checkdoc-batch home page is
http://user42.tuxfamily.org/checkdoc-batch/index.html
HERE
  exit 1
fi


echo "Running checkdoc.."
error_lines=`emacs -Q -batch $user $args -l $lispfile -f checkdoc-batch-commandline "$@" | tee /dev/fd/2 | wc -l`

if [ $error_lines -gt 1 ]; then
	echo $OUTPUT
	echo "There were errors processing things."
	exit 1
fi

