#!/bin/bash


################################################################################
#
# run_tests.sh -- Bash script for checking the table2itol.R script.
#
# (C) since 2017 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License. See
# http://www.gnu.org/licenses/gpl.html for further information.
#
################################################################################


set -eu


################################################################################


# Expects textual input files with name of directory of expected output files
# in 1st field, name of directory of resulting output files in 2nd fields, and
# program call to be tested in the rest of the fields. First argument must be
# example output file that will list the successful calls to table2itol.R.
#
function check_outdir
{

  [ $# -gt 0 ] || return

  local examples=$1
  shift

  [ $# -gt 0 ] || return

  local expdir outdir result empty infile
  declare -a words
  declare -i i

  for infile; do
    if [ "$infile" = "$examples" ]; then
      echo "ERROR: example file '$examples' used as input file" >&2
      return 1
    fi
  done

  rm -f "$examples"

  while read -a words; do

    [ "${#words[@]}" -gt 2 ] || continue
    expdir=${words[0]}
    [ -d "$expdir" ] && empty='' || empty=yes

    outdir=${words[1]}
    if [ -d "$outdir" ]; then
      echo "ERROR: directory '$outdir' already exists" >&2
      return 1
    fi

    unset words[0] words[1]
    for ((i = 2; i <= ${#words[@]} + 1; i++)); do
      [ "${words[$i]}" = __OUTDIR__ ] && words[$i]=$outdir
    done

    echo "running test $expdir <=> $outdir ..." >&2
    if eval "${words[@]// /\\ }"; then
      if [ "$empty" ]; then
        if [ -d "$outdir" ]; then
          result=FAILURE
        else
          result=SUCCESS
        fi
      else
        if diff -rq "$outdir" "$expdir"; then
          result=SUCCESS
        else
          result=FAILURE
        fi
      fi
    else
      result=ERROR
    fi

    echo -e "TEST $expdir <=> $outdir\t$result"

    if [ "$result" = SUCCESS ]; then
      if [ -z "$empty" ]; then
        rm -f "$outdir"/* && rmdir "$outdir"
      fi
      echo "${words[@]// /\\ }" >> "$examples"
    fi

    echo >&2

  done < "$@"

}


################################################################################


[ -z "${0%/*}" ] || cd "${0%/*}"


[ $# -eq 0 ] && [ -s tests.def ] && set -- tests.def


[ $# -gt 0 ] && check_outdir examples.txt "$@"


