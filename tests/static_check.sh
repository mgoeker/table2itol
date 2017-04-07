#!/bin/bash


################################################################################
#
# static_check.sh -- Bash script for checking the table2itol.R script.
#
# (C) since 2017 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License. See
# http://www.gnu.org/licenses/gpl.html for further information.
#
################################################################################


set -eu


################################################################################


function static_check
{
  local infile tmpfile
  declare -i errs=0
  tmpfile=$(mktemp --suffix ."$FUNCNAME")
  for infile; do
    if R --interactive --slave > /dev/null <<-______EOF
customised_check <- function(env = globalenv()) {
  problems <- character()
  codetools::checkUsageEnv(env = env, all = TRUE, report = function(s)
    problems <<- c(problems, s), suppressParamAssigns = TRUE)
  # filter out known, irrelevant issues detected in table2itol.R
  ok <- paste0("\\\\b(emit_\\\\w+|GENERATED_FILES)\\\\W+(assigned but|",
    "parameter\\\\W+(ids|outdir|x))\\\\W+may not be used\\\\b")
  cat(problems[!grepl(ok, problems, FALSE, TRUE)], file = "$tmpfile")
}
source("$infile")
customised_check()
quit("no", 0L)
______EOF
    then
      true
    else
      let errs+=1
      echo 'ERROR: call of R did not result' >&2
    fi
    if [ -s "$tmpfile" ]; then
      let errs+=1
      echo "PROBLEMS in file $infile detected by static check:"
      cat "$tmpfile"
    else
      echo "File $infile seems alright."
    fi
    echo
  done
  rm -f "$tmpfile"
  [ $errs -gt 0 ] && return 1 || return 0
}


################################################################################


[ -z "${0%/*}" ] || cd "${0%/*}"


[ $# -gt 0 ] || set -- ../table2itol.R


static_check "$@"


