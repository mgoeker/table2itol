---
output:
  html_document: default
  word_document: default
---
# table2itol

## About

Interactive Tree of Life ([iTOL](http://itol.embl.de/)) is a popular tool for
displaying phylogenetic trees and associated information. The `table2itol.R`
script makes it easy to generate iTOL annotations from spreadsheet files.

## Features

* Works with [CSV](https://en.wikipedia.org/wiki/Delimiter-separated_values),
  OpenOffice, LibreOffice and Microsoft Excel files.
* Supports iTOL domains, colour strips, simple bars, gradients, binary data,
  and texts.
* Partially supports iTOL branch annotation (currently work in progress).
* By default selects the appropriate visualisation from the data type of each
  input column but this can be modified by the user.
* Provides carefully chosen colour vectors for up to 40 levels and optionally
  combines them with symbols for maximizing contrast.
* The default colour vectors can be replaced by user-defined colour vectors.
* Can be used either interactively on any operating system on which R is
  running, or non-interactively using the command line of a UNIX-like system.

## Prerequisites

* A recent version of [R](https://cran.r-project.org/).
* The [optparse](https://CRAN.R-project.org/package=optparse) package for R.
* The [plotrix](https://CRAN.R-project.org/package=plotrix) package for R if
  you want to generate branch annotations from continuous numeric data.
* The [readxl](https://CRAN.R-project.org/package=readxl) package for R if
  you want to apply the script to Microsoft Excel files.
* The [readODS](https://CRAN.R-project.org/package=readODS) package for R if
  you want to apply the script to Libreoffice or Openoffice
  [ods](https://en.wikipedia.org/wiki/OpenDocument) files.
* The [yaml](https://CRAN.R-project.org/package=yaml) package for R if
  you want to define colour vectors yourself.

*Please note that explaining how to correctly install R or R packages is beyond
the scope of this manual, and please do not contact the `table2itol.R` authors
about this issue. There is plenty of online material available elsewhere.*

## Installation

First, obtain the script as indicated on its GitHub page.

### Command-line use

The following explanations are for *non-experts*; there is nothing special with
running this script. If necessary make the script executable:

`chmod +x table2itol.R`

Then call:

`./table2itol.R`

to obtain the help message. If this yields an error, see the **troubleshooting**
chapter.

Optionally place the script in a folder that is contained in the `$PATH`
variable, e.g.

`install table2itol.R ~/bin`

or even

`sudo install table2itol.R /usr/local/bin`

if you have sudo permissions. Then you can call the script by just entering

`table2itol.R`

### Interactive use

Open R or [RStudio](https://www.rstudio.com/) or whatever interface to R you
are using, then enter at the console:

`source("table2itol.R")`

provided the script is located in the current working directory as given by
`getwd()`. Alternatively, first use `setwd()` to move to the directory in which
`table2itol.R` resides or enter the full path to the location of the script.

When loading the script it shows the usual help message and an indication that
you are running it in interactive mode. When doing so, you might need to modify
the `options` variable much like command-line users might need to apply certain
command-line options. For instance, in analogy to entering:

`./table2itol.R --identifier Tip --label Name annotation.tsv`

on the command line of a UNIX-like system, you would enter within R the
following:

```R
source("table2itol.R")
options$identifier <- "Tip"
options$label <- "Name"
args <- "annotation.tsv"
create_itol_files(args, options)
```

The analogy should be obvious, hence for details on the values of `options` just
see the help message. With some basic knowledge of R is easy to set up
customized scripts that modify `options` for your input files and generate the
intended output.

## Examples

Exemplars for input table files are found within the `tests/INPUT` folder. A
list of examples for calling `table2itol.R` is found in `tests/examples.txt`.

*Experts only*: On a UNIX-like system you can run these examples by calling
`tests/run_tests.sh` provided a modern Bash is installed.

## Troubleshooting

Some commonly encountered error messages are mentioned in the following. Note
that you might actually get these error messages in a language other than
English (e.g., your own language) or with other minor modifications.

### Command-line use

#### Bad interpreter

`/usr/local/bin/Rscript: bad interpreter: No such file or directory`

Solution: Enter

`locate Rscript`

and watch the output. If it is empty, you must install
[R](https://cran.r-project.org/) first. If you instead obtained a location such
as `/usr/bin/Rscript` you could do the following:

`sudo ln -s /usr/bin/Rscript /usr/local/bin/Rscript`

if you had sudo permissions. Alternatively, within the first line of the script
replace `/usr/local/bin/Rscript` by `/usr/bin/Rscript` or wherever your
`Rscript` executable is located. A third option is to leave the script as-is and
enter `Rscript table2itol.R` instead of `./table2itol.R` or whatever location of
the script you are using. But this is less convenient in the long run.

*Please note that explaining how to correctly install R is beyond the scope of
this manual, and please do not contact the `table2itol.R` authors about this
issue. There is plenty of online material available elsewhere.*

### Command-line or interactive use

#### Missing R package

`there is no package called 'optparse'`

Solution: Install the [optparse](https://CRAN.R-project.org/package=optparse)
package for R. (It is not an absolute requirement in interactive mode but
without it you would need to compile the `options` variable by hand.)

`there is no package called 'plotrix'`

Solution: Install the [plotrix](https://CRAN.R-project.org/package=plotrix)
package for R. (It is only needed if you want to create branch annotations
from continuous numeric data.)

`there is no package called 'readODS'`

Solution: Install the [readODS](https://CRAN.R-project.org/package=readODS)
package for R. (It is only needed if you want to apply the script to
[ods](https://en.wikipedia.org/wiki/OpenDocument) files.)

`there is no package called 'readxl'`

Solution: Install the [readxl](https://CRAN.R-project.org/package=readxl)
package for R. (It is only needed if you want to apply the script to Microsoft
Excel files.)

`there is no package called 'yaml'`

Solution: Install the [yaml](https://CRAN.R-project.org/package=yaml)
package for R. (It is only needed if you want to use the script in conjunction
with your own colour vectors.)

*Please note that explaining how to correctly install R packages is beyond the
scope of this manual, and please do not contact the `table2itol.R` authors about
this issue. There is plenty of online material available elsewhere.*

#### Outdated R version

`need a newer version of R, 3.2.0 or higher`

Solution: Install a newer version of [R](https://cran.r-project.org/).

*Please note that explaining how to correctly install R is beyond the scope of
this manual, and please do not contact the `table2itol.R` authors about this
issue. There is plenty of online material available elsewhere.*

#### The script generates not enough output files

Solution: Watch the warnings and error messages generated by the script. Without
any input files, the script *should* not generate any output. The script would
also skip input files or single tables if they failed to contain columns you
have requested. You can use the `--abort` option to let the script immediately
stop in such cases, then look up the last error message in this manual. But even
without `--abort` the script generates warnings when data sets get skipped.

#### The script generates too many output files

Solution: Accept as a design decision that the scripts generates one file for
each input column (except for the tip identifier column). Since you can still
decide to not upload (some of) the generated files to iTOL and also deselect
data sets within iTOL, we believe it would not make much sense to also include a
selection mechanism within the `table2itol.R` script. As last resort you could
also reduce the number of input columns.

#### A column is requested but missing

`selected column 'ID' does not exist`

Solution: Use the `--identifier` option to set the name of the tip identifier
column.

`selected column 'Label' does not exist`

Solution: Use the `--label` option to set the name of the tip label column.

#### A name clash of output file names occurs

`name clash: file [...] has already been generated`

Solution: Name the columns distinctly in distinct tables within the same file
and/or call the `table2itol.R` script individually for each input file, maybe
best with distinct values of `--directory`.

Since the name of each output file is generated from the name of the respective
input column and the resulting kind of visualisation, columns from distinct
input tables but with the same name and the same resulting kind of visualisation
would yield only a single output file. Instead of silently overwriting the
earlier ones, the script stops with an informative error message.

## Frequently asked questions

#### How can I generate other kinds of visualisation from integer columns?

For generating distinct kinds of visualisation from distinct integer columns,
run the script several times with distinct values of `--conversion`. Since the
name of each output file is generated from the name of the respective input
column *and* the resulting kind of visualisation, nothing of importance will be
overwritten (but see the section on name clashes between distinct spreadsheets).

#### How can I define my own colour vectors?

For replacing the default colour vectors by other colour vectors, use the
`--colour-file` option. Its argument must be the name of a file in
[YAML](http://yaml.org/) format. Names of the colour vectors are optional in
such files but increase readability. Not all colour vectors need to be defined,
only those that should replace default colour vectors. Assignment is solely by
vector length.
