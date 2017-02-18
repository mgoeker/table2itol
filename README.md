# table2itol

## About

Interactive Tree of Life ([iTOL](http://itol.embl.de/)) is a popular tool
for displaying phylogenetic trees and associated information.
The `table2itol.R` script makes it easy to generate iTOL annotations from 
spreadsheet files.

## Features

* Works with [CSV](https://en.wikipedia.org/wiki/Delimiter-separated_values),
  OpenOffice, LibreOffice and Microsoft Excel files.
* Supports iTOL domains, colour strips, simple bars, gradients, and texts.
* By default selects the appropriate visualisation from the data type of each
  input column but this can be modified by the user.
* Provides carefully chosen colour vectors for up to 40 levels but combines
  them with symbols for maximizing contrast.
* Can be used either interactively on any operating system on which R is
  running, or non-interactively using the command line of a UNIX-like system.

## Prerequisites

* A recent version of [R](https://cran.r-project.org/).
* The [optparse](https://CRAN.R-project.org/package=optparse) package for R.
* The [readxl](https://CRAN.R-project.org/package=readxl) package for R if
  you want to apply the script to Microsoft Excel files.
* The [readODS](https://CRAN.R-project.org/package=readODS) package for R if
  you want to apply the script to Libreoffice or Openoffice 
  [ods](https://en.wikipedia.org/wiki/OpenDocument) files.

*Please note that explaining how to correctly install R or R packages is beyond
the scope of this manual.*

## Installation

First, obtain the script as indicated on its GitHub page.

### Command-line use

If necessary make the script executable:

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
`getwd()`. Alternatively, first move to the directory in which `table2itol.R`
resides or enter the full path to its location.

When loading the script it shows the usual help message and an indication that
you are running it in interactive mode. When doing so, you might need to modify
the `opt` variable much like command-line users might need to apply certain 
command-line options. For instance, in analogy to entering:

`./table2itol.R --identifier Tip --label Name annotation.tsv`

on the command line, you would enter within R the following:

```
source("table2itol.R")
opt$identifier <- "Tip"
opt$label <- "Name"
infiles <- "annotation.tsv"
create_itol_files(infiles, opt)
```

The analogy should be obvious, hence for details on the values of `opt` just 
see the help message.

## Examples

Exemplars for input table files are found within the `tests/INPUT` folder. A
list of examples for calling `table2itol.R` is found in `tests/examples.txt`.

*Experts only*: On a UNIX-like system you can run these examples by calling
`tests/run_tests.sh` provided a modern Bash is installed.

## Troubleshooting

Some commonly encountered error messages are discussed in the following. Note
that you might actually get these error messages in a language other than
English (e.g., your own language) or with other minor modifications.

### Command-line use

#### Bad interpreter

`/usr/local/bin/Rscript: bad interpreter: No such file or directory`

Solution: Enter

`locate Rscript`

and watch the output. If it is empty, you must first install 
[R](https://cran.r-project.org/). If you instead obtain a location such as
`/usr/bin/Rscript` you could do the following:

`sudo ln -s /usr/bin/Rscript /usr/local/bin/Rscript`

if you have sudo permissions. Alternatively, within the first line of the script
replace `/usr/local/bin/Rscript` by `/usr/bin/Rscript` or wherever your
`Rscript` executable is located. A third option is to leave the script as-is and
enter `Rscript table2itol.R` instead of `./table2itol.R` or whatever location of
the script you are using. But this is not particularly convenient in the long
run.

*Please note that explaining how to correctly install R is beyond the scope of
this manual.*

### Command-line or interactive use

#### Missing R package

`there is no package called ‘optparse’`

Solution: Install the [optparse](https://CRAN.R-project.org/package=optparse)
package for R.

`there is no package called ‘readODS’`

Solution: Install the [readODS](https://CRAN.R-project.org/package=readODS)
package for R.

`there is no package called ‘readxl’`

Solution: Install the [readxl](https://CRAN.R-project.org/package=readxl)
package for R.

*Please note that explaining how to correctly install R packages is beyond the
scope of this manual.*

#### Outdated R version

`need a newer version of R, 3.1.0 or higher`

Solution: Install a newer version of [R](https://cran.r-project.org/).

*Please note that explaining how to correctly install R is beyond the scope of
this manual.*

#### The script generates no output

Without any input files, the script *should* not generate any output. It might
also skip input files or single tables if they fail to contain columns you have
requested. You can use the `--abort` option to let the script immediately stop
in such cases, then look up the last error message in this manual. But even
without `--abort` the script generates warnings when data sets get skipped.

#### A column is requested but missing

`selected column 'ID' does not exist`

Solution: Use the `--identifier` argument to set the name of the tip identifier
column.

`selected column 'Label' does not exist`

Solution: Use the `--label` argument to set the name of the tip label column.

