#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# csv2itol -- Rscript script for generating input files for iToL.
#
# (C) since 2016 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
# You must install R and optparse (https://cran.r-project.org/package=optparse)
# to run this script. For working with Libreoffice/Openoffice ods files, the
# readODS package is needed, and readxl for working with Excel files.
#
# This script was written for the command line but can also be used in
# interactive mode.
#
################################################################################


options(warn = 1L)


################################################################################


# Main function, does everything given a vector of file names 'infiles' and a
# list of options 'opt'.
#
create_itol_files <- function(infiles, opt) {


  ## Constants

  # EL  ellipse
  # RE  rectangle
  # TL  left pointing triangle
  # TR  right pointing triangle
  # DI  rhombus (diamond)
  # HH  horizontal hexagon
  # HV  vertical hexagon
  # PL  left pointing pentagram
  # PR  right pointing pentagram
  # PU  up pointing pentagram
  # PD  down pointing pentagram
  # OC  octagon
  # GP  rectangle (gap; black filled rectangle with 1/3 normal height)
  #
  SYMBOLS <- c("EL", "RE", "TL", "TR", "DI", "HH", "HV",
    "PL", "PR", "PU", "PD", "OC", "GP")


  BLACK <- "#000000"

  WHITE <- "#FFFFFF"


  # Colour vectors collected by Jan P. Meier-Kolthoff.
  #
  COLORS <- list(
    JMK01 = "#1b9e77",
    JMK02 = c("#1b9e77", "#d95f02"),
    JMK03 = c("#1b9e77", "#d95f02", "#7570b3"),
    JMK04 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"),
    JMK05 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99"),
    JMK06 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c"),
    JMK07 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f"),
    JMK08 = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
      "#66a61e", "#e6ab02", "#a6761d", "#666666"),
    JMK09 = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
      "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"),
    JMK10 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
    JMK11 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
      "#ffff99"),
    JMK12 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
      "#ffff99", "#b15928"),
    JMK13 = c("#8393c7", "#8ad256", "#6a49c5", "#d2b351",
      "#cb55c3", "#4d4040", "#c4527c", "#57743d", "#d85439", "#7accb1",
      "#925136", "#ceb2ab", "#512f67"),
    JMK14 = c("#a2d1cd", "#5d39a8", "#71d14c", "#cb56c7",
      "#7ed094", "#4d4040", "#7077b8", "#c28b4c", "#cd9dae", "#c64a34",
      "#55868c", "#cccb51", "#b2436e", "#567137"),
    JMK15 = c("#92d4ad", "#6842c1", "#6ecf58", "#cb4ec2",
      "#55733d", "#4d4040", "#c99447", "#9083cb", "#c9d14f", "#4d2c63",
      "#cea4a2", "#d54f38", "#71a6bd", "#ca507f", "#823f33"),
    JMK16 = c("#76a5bd", "#bfdf44", "#cf4bab", "#66c95b",
      "#7c42c5", "#4d4040", "#7279ca", "#c27837", "#4b2a62", "#c7b956",
      "#cc8cb5", "#536e3b", "#d74746", "#84d3ae", "#893b42", "#cdb19a"),
    JMK17 = c("#823f35", "#77d952", "#6d44c4", "#78d5a1",
      "#cf4a70", "#4d4040", "#ca53bd", "#69923c", "#6d7fc4", "#d1d04e",
      "#532b63", "#d64d31", "#4b623d", "#ca96b7", "#78b5c2", "#ccbf9b",
      "#c58741"),
    JMK18 = c("#697bc5", "#5e9742", "#6641c0", "#7bdc57",
      "#c954c9", "#4d4040", "#4d2b62", "#73d6ac", "#d6493d", "#75adbe",
      "#c54883", "#526339", "#caca9b", "#7b332e", "#cfcf49", "#c89dc8",
      "#c58738", "#c78980"),
    JMK19 = c("#9e693f", "#9147d5", "#c9d747", "#9482d3",
      "#61913d", "#4d4040", "#6dd85e", "#d049a4", "#76d0b6", "#d5493c",
      "#6897bb", "#d7993d", "#553291", "#c7cb8a", "#472f5b", "#cd7993",
      "#496340", "#ccb8bc", "#7f2c3a"),
    JMK20 = c("#7295c1", "#d44b38", "#6ad14f", "#6a3bc0",
      "#cedb44", "#4d4040", "#77d192", "#cb4fc3", "#b1b85f", "#7772cc",
      "#d9973b", "#4f2b62", "#79d1cf", "#cc497b", "#4a6c2e", "#c990b5",
      "#752e30", "#d1c5ac", "#a26f47", "#537e71"),
    JMK21 = c("#90b5d9", "#d6532d", "#c84ccc", "#74d147",
      "#512d79", "#4d4040", "#6740c8", "#cace49", "#6b79d1", "#6ccc84",
      "#c8478c", "#74c4b8", "#cc4458", "#4f6781", "#cb9142", "#552443",
      "#c6cb97", "#82442d", "#c489c5", "#546d37", "#cb9896"),
    JMK22 = c("#392c51", "#4d4040", "#642c79", "#792d3b",
      "#6a3ec6", "#875b30", "#4f7231", "#547f72", "#d24637", "#6d71ce",
      "#d2497e", "#cd4fc8", "#6a8fbc", "#d88742", "#c78dc6", "#cc9795",
      "#c7af40", "#68cd55", "#72d4a6", "#9ecfd6", "#c9cb8f", "#c3de48"),
    JMK23 = c("#8ad93f", "#c749c4", "#5e8f3d", "#6639be",
      "#73d979", "#4d4040", "#d4ca4a", "#6c6ccc", "#d78c3b", "#6485b9",
      "#d24635", "#70d4ae", "#cc4279", "#cbcb99", "#4c295f", "#ce867e",
      "#793130", "#84cbd7", "#896c35", "#c27bbb", "#364e27", "#cab2cb",
      "#5b837b"),
    JMK24 = c("#ccc79a", "#6a42c7", "#d0a540", "#cc49c9",
      "#6dd755", "#4d4040", "#de5a26", "#7cc7d0", "#cc3f47", "#78d8a5",
      "#5e2d78", "#c9da51", "#6679d0", "#bf7348", "#c6b7d8", "#5f903c",
      "#c47ec5", "#6a5b29", "#ce4684", "#497359", "#772d38", "#c3858c",
      "#352444", "#5b7a9e"),
    JMK25 = c("#6ba43c", "#c74ace", "#cbe14b", "#6847cd",
      "#6ede53", "#4d4040", "#cbb248", "#592e82", "#d6842f", "#5e78c1",
      "#76dd99", "#c6438e", "#4b8047", "#cf4c67", "#7acdc4", "#d2472f",
      "#7ba5c4", "#79322f", "#c388cf", "#78662f", "#45294d", "#c8cd9d",
      "#3e5d4a", "#d08c6c", "#c698a9"),
    JMK26 = c("#73d991", "#b44adb", "#71d94d", "#cf4cb4",
      "#ccde4d", "#4d4040", "#ceae44", "#5a41c2", "#cdd09c", "#652e7a",
      "#83d7ce", "#dc4338", "#536e83", "#d34a79", "#5d9073", "#c68dc7",
      "#619339", "#85b1d7", "#da8340", "#6978cb", "#9d4533", "#34284e",
      "#d09e9e", "#732d41", "#364e25", "#866a38"),
    JMK27 = c("#363258", "#6ed853", "#5b3fc7", "#c9de43",
      "#b54ad9", "#4d4040", "#5c2c7e", "#b7d17b", "#cf4a83", "#6ed9a4",
      "#cd4450", "#8fd3d5", "#d74527", "#769ac1", "#d27d3f", "#6d75cf",
      "#d4af42", "#4f8c3b", "#d14eba", "#568778", "#c692c8", "#344625",
      "#d4c7a6", "#722e4c", "#c88988", "#7a3a25", "#86783a"),
    JMK28 = c("#7f3a27", "#71da53", "#c14bd4", "#55933d",
      "#626ad0", "#4d4040", "#623ac4", "#cbd943", "#542c79", "#c1d483",
      "#bc7fd0", "#6ad7a3", "#d84330", "#71bec7", "#ce7537", "#6f99d8",
      "#d5aa43", "#546586", "#7c7233", "#ce429f", "#3e6344", "#ce7d9f",
      "#2d1d38", "#c6b3ce", "#793151", "#bfcbae", "#d24566", "#c8927d"),
    JMK29 = c("#cdc2c2", "#663dc8", "#76dd51", "#c64ece",
      "#cfda49", "#4d4040", "#549e3f", "#7577da", "#d3522e", "#7cd6ce",
      "#d4425b", "#77de9a", "#542a7e", "#d1d395", "#321e3d", "#d74a98",
      "#95963d", "#586095", "#db9a3e", "#77abd9", "#8b3c67", "#639575",
      "#d08982", "#456129", "#ca92cc", "#896134", "#597984", "#742c28",
      "#283a28"),
    JMK30 = c("#31223c", "#bbe141", "#c94edb", "#65d559",
      "#8b3899", "#4d4040", "#613ec8", "#df9b36", "#6e75d5", "#c16c39",
      "#402a74", "#cfc248", "#da47a4", "#63d6ad", "#d94330", "#6abccd",
      "#c58181", "#617fae", "#7f2f2c", "#b5cfb8", "#833b65", "#b5d888",
      "#cc88cb", "#4e8a3b", "#d6466a", "#476d58", "#d2b284", "#544320",
      "#c9b6d0", "#867c36"),
    JMK31 = c("#913d83", "#ced242", "#6643d0", "#79d949",
      "#c249d4", "#4d4040", "#db45a4", "#68dc88", "#3a1f4f", "#c3d483",
      "#532e8e", "#da983e", "#6d79d5", "#9b4b29", "#d085d5", "#8b7d3b",
      "#c9a0c0", "#54913d", "#dc4b32", "#72d4b1", "#8f3e58", "#90d0d8",
      "#592720", "#d2c7a9", "#21262c", "#d64769", "#3b4f25", "#6ea2cf",
      "#cd887a", "#5c6089", "#568477"),
    JMK32 = c("#8f8b38", "#663cc8", "#6bd546", "#c74cce",
      "#b1d773", "#4d4040", "#c6e03a", "#59287c", "#5edb86", "#d14592",
      "#7ad9b1", "#da4627", "#719cd8", "#dc973a", "#6e71d7", "#dbc348",
      "#ca84c8", "#4c8b3a", "#d5445a", "#84ccd6", "#7f3353", "#d3c99f",
      "#2e1c38", "#ca7442", "#5a558b", "#803325", "#537286", "#cc8585",
      "#314826", "#cab3cc", "#7e6136", "#618d75"),
    JMK33 = c("#d64e9e", "#6cd54c", "#dd49d1", "#c8dd41",
      "#a152dd", "#4d4040", "#5139c2", "#ceaa3b", "#432d7c", "#c6d179",
      "#8f379a", "#70d68c", "#d9432f", "#6ad5be", "#d5416a", "#76c2d7",
      "#d87a71", "#6a75d5", "#836834", "#c988d1", "#598939", "#7a3260",
      "#bed3b3", "#8f372e", "#6082b3", "#d47c35", "#312749", "#d4ac8b",
      "#314825", "#cab9d7", "#4b211f", "#ad788b", "#568275"),
    JMK34 = c("#d8436c", "#653cc7", "#b4dc41", "#d143d0",
      "#5fd857", "#4d4040", "#a4db84", "#c64496", "#6adcad", "#de4830",
      "#6aa3d9", "#d98731", "#6271d1", "#dec841", "#b062cd", "#528e36",
      "#c28acd", "#675b2c", "#cbb7d3", "#a53332", "#528089", "#532878",
      "#d9d393", "#2a1e3c", "#8ed4d3", "#834629", "#5e5e8a", "#a08e3c",
      "#2b482a", "#d78763", "#619470", "#c87b8d", "#702944", "#c3a994"),
    JMK35 = c("#72d4cf", "#ccdf3e", "#5533c1", "#70d951",
      "#ac42d6", "#4d4040", "#6d66dc", "#b9c866", "#562a84", "#71da99",
      "#db43c7", "#518f39", "#d04497", "#314826", "#bc6cc9", "#5d8b74",
      "#d2416d", "#72abd3", "#dd461f", "#6078c6", "#d7ab3b", "#c49ad6",
      "#7d6b2f", "#cab8c4", "#3c1a20", "#c8ddb6", "#312652", "#cfb182",
      "#7c3463", "#c98271", "#576782", "#d24243", "#cb7a99", "#82372d",
      "#cf7734"),
    JMK36 = c("#6ade4b", "#6344d3", "#7bdc86", "#b746d4",
      "#65a234", "#4d4040", "#dbc941", "#552c93", "#bee148", "#dc3fb4",
      "#62d7b4", "#903a7e", "#4a8245", "#cf74d0", "#da993a", "#3e255f",
      "#c0d3b2", "#291d2d", "#cdce7e", "#752c41", "#7dcbd6", "#c43c44",
      "#669bcf", "#de4e28", "#5b5e83", "#c97449", "#bd92d0", "#847933",
      "#d7417a", "#558279", "#d07d92", "#364525", "#ceb9d0", "#763d23",
      "#6872d2", "#be9880"),
    JMK37 = c("#645b8e", "#80dc40", "#4f2ea4", "#69dc7b",
      "#d848cd", "#4d4040", "#8548da", "#c7d84e", "#96368e", "#afd995",
      "#d54227", "#61d9b9", "#db4187", "#4a9339", "#cd83d6", "#7a8431",
      "#6870d5", "#e3bc3b", "#6b9bd7", "#d87935", "#6fbfcf", "#cd3e50",
      "#c3d8c8", "#772e29", "#dbc38b", "#3f2267", "#bf9340", "#cab1d6",
      "#304726", "#b2918d", "#2a1f35", "#d5816f", "#5e8c6b", "#c77192",
      "#497080", "#7d592d", "#732d52"),
    JMK38 = c("#cf8ad0", "#74e042", "#b946da", "#5be080",
      "#5834c1", "#4d4040", "#d248bb", "#59a434", "#8064d4", "#b4dc4e",
      "#893876", "#96db99", "#d9478a", "#499052", "#627bcf", "#dfd238",
      "#47277a", "#908f39", "#79a2d8", "#d79234", "#4c7788", "#df502c",
      "#625984", "#d7d27b", "#2e1d3b", "#6bdac4", "#d34557", "#6a8b73",
      "#9e4427", "#cfb5cd", "#78562e", "#7cc6d5", "#26392b", "#cdcfb2",
      "#702735", "#bd7984", "#405924", "#d59571"),
    JMK39 = c("#8b308f", "#74dd41", "#6939ca", "#cce346",
      "#d545d2", "#4d4040", "#b271dd", "#e39b39", "#5050bc", "#cabc46",
      "#3a1f64", "#5cde7e", "#d9428e", "#57a56d", "#d63949", "#76dfc2",
      "#7e3052", "#b7e28f", "#d286c6", "#66a234", "#6d83d8", "#d65629",
      "#76c3d2", "#843326", "#6aa0d5", "#9c762c", "#5f5488", "#d48e70",
      "#4a6a81", "#d36778", "#466b2c", "#b28491", "#273825", "#c1b47a",
      "#301b31", "#d0d2bd", "#6c552d", "#c9b8d8", "#5f8675"),
    JMK40 = c("#3c2b5d", "#dee032", "#ab48d5", "#5bd749",
      "#db49c6", "#4d4040", "#5c42d0", "#a4e040", "#462687", "#d8b136",
      "#8d3989", "#60d076", "#d7468f", "#63d8b5", "#de4528", "#77c7d6",
      "#d13a55", "#5f8c7b", "#ce88d5", "#759b31", "#696ecd", "#de8739",
      "#6f9ad6", "#b75738", "#aadc90", "#946d89", "#d0dc6a", "#2c1a25",
      "#c6d8bc", "#782849", "#ceb977", "#283f27", "#d9798c", "#447c3d",
      "#ceb8d4", "#635b2d", "#c79783", "#733426", "#476682", "#98762e")
  )


  ## Helper functions


  # E.g. anyNA() is only available from 3.1.0 on.
  #
  assert_R_version <- function(wanted = numeric_version("3.1.0")) {
    if (getRversion() < wanted)
      stop(sprintf("need a newer version of R, %s or higher", wanted))
    invisible(TRUE)
  }


  # Input method dispatch is based on file extension. Depends on extra library
  # for Excel and Libreoffice/Openoffice files, respectively.  Must ensure
  # character vectors are converted to factors.
  #
  read_file <- function(file, opt) {
    read_xl <- function(sheet, path, na) {
      tryCatch(expr = readxl::read_excel(path = path, na = na,
        col_names = TRUE, col_types = NULL, skip = 0L), error = function(e) {
          warning(e) # a typical error is to encounter an empty sheet
          data.frame() # now we can treat this later on ourselves
        })
    }
    rescue_factors <- function(x) { # not necessary for CSV input
      for (i in which(vapply(x, is.character, NA)))
        x[, i] <- factor(x[, i])
      x
    }
    switch(EXPR = tolower(tools::file_ext(file)),
      ods = lapply(lapply(X = readODS::ods_sheets(file), path = file,
        FUN = readODS::read_ods, na = opt$`na-strings`[[1L]], col_names = TRUE,
        col_types = NULL, formula_as_formula = FALSE, skip = 0L, range = NULL),
        rescue_factors),
      xls =,
      xlsx = lapply(lapply(readxl::excel_sheets(file), read_xl, file,
        opt$`na-strings`[[1L]]), rescue_factors),
      list(read.table(file = file, header = TRUE, sep = opt$separator,
        quote = "\"", dec = ".", fill = FALSE, stringsAsFactors = TRUE,
        na.strings = opt$`na-strings`, check.names = FALSE, comment.char = ""))
    )
  }


  # We add white at the end, assuming this represents NA, when NA values occur.
  #
  select_colours <- function(size, has.na) {
    if (has.na)
      c(COLORS[[size - 1L]], WHITE)
    else
      COLORS[[size]]
  }


  # Used for generating the output filename.
  #
  itol_filename <- function(colname, kind, directory) {
    message(sprintf("Generating %s file for column '%s' ...", kind, colname))
    file.path(directory,
      sprintf("iToL_%s-%s.txt", kind, gsub("\\W", "_", colname, FALSE, TRUE)))
  }


  # Used for generating legend titles.
  #
  nice_str <- function(x) {
    chartr("_.", "  ", x)
  }


  # Always called before print_itol_data.
  #
  print_itol_header <- function(file, title, annotation) {
    join <- function(x) {
      if (!length(x))
        return(NULL)
      stopifnot(!is.null(names(x)))
      sizes <- lengths(x, FALSE)
      for (i in which(sizes > 1L))
        x[[i]] <- paste0(x[[i]], collapse = "\t")
      for (i in which(!sizes))
        x[[i]] <- ""
      paste(names(x), unlist(x, FALSE, FALSE), sep = "\t")
    }
    cat(title, "SEPARATOR TAB", join(annotation), "DATA", file = file,
      labels = NULL, sep = "\n", fill = FALSE, append = FALSE)
  }


  # Always called after print_itol_header.
  #
  print_itol_data <- function(file, ...) {
    cat(paste(..., sep = "\t", collapse = NULL), file = file,
      labels = NULL, sep = "\n", fill = FALSE, append = TRUE)
  }


  ## Functions for special columns


  # For labelling the leaves.
  #
  emit_itol_labeltexts <- function(x, ids, name, outdir, ...) {
    outfile <- itol_filename(name, "labelling", outdir)
    print_itol_header(outfile, "LABELS", NULL)
    print_itol_data(outfile, ids, x)
  }


  # For colouring the leaves.
  #
  emit_itol_labelcolors <- function(x, ids, name, outdir, ...) {
    size <- length(levels(x))
    if (size > length(COLORS)) {
      warning(sprintf("skipping column '%s', which yields > %i levels",
        name, length(COLORS)))
      return()
    }
    outfile <- itol_filename(name, "treecolors", outdir)
    colors <- select_colours(size, FALSE)
    annotation <- list(
      DATASET_LABEL = name,
      LEGEND_TITLE = nice_str(name),
      LEGEND_COLORS = colors,
      LEGEND_SHAPES = rep.int(1L, size),
      LEGEND_LABELS = levels(x),
      COLOR = "#a6cee3"
    )
    print_itol_header(outfile, "TREE_COLORS", annotation)
    print_itol_data(outfile, ids, "range", colors[x], x)
  }


  ## Functions for columns according to data type (class)


  # Output varies depending on the number of colours and symbols chosen and/or
  # available.
  #
  emit_itol_factor <- function(x, ids, name, outdir, symbols, max.colors,
      favour, ...) {

    product <- function(x, y) {
      cbind(rep(x = x, each = length(y)), rep.int(y, length(x)))
      #cbind(rep.int(x, length(y)), rep(x = y, each = length(x)))
    }

    x <- addNA(x, TRUE)
    base.annotation <- list(DATASET_LABEL = name, MARGIN = 5, COLOR = "#bebada")
    size <- length(levels(x))

    if (size > max.colors * length(SYMBOLS)) {

      outfile <- itol_filename(name, "text", outdir)
      print_itol_header(outfile, "DATASET_TEXT", base.annotation)
      # additional columns: position, color, style, size_factor, rotation
      print_itol_data(outfile, ids, x, -1, BLACK, "normal", 0.75, 0)

    } else if (length(symbols) || size > max.colors) {

      outfile <- itol_filename(name, "domains", outdir)

      if (length(symbols)) {
        symbols <- vapply(split.default(symbols, x), `[[`, "", 1L)
        colors <- select_colours(size, anyNA(levels(x)))
      } else {
        nsym <- ncls <- ceiling(sqrt(size))
        nsym <- round(nsym / favour, 0L)
        ncls <- round(ncls * favour, 0L)
        if (nsym > length(SYMBOLS) || ncls > max.colors) {
          msg <- sprintf(
            "Column '%s': # symbols (%i) or # colours (%i) inacceptable",
            name, nsym, ncls)
          if (favour >= 1) {
            ncls <- max.colors
            nsym <- ceiling(size / ncls)
          } else {
            nsym <- length(SYMBOLS)
            ncls <- ceiling(size / nsym)
          }
          message(msg, sprintf(", trying %i/%i instead.", nsym, ncls))
        }
        colors <- select_colours(ncls, FALSE) # NA treated below
        symbols <- product(SYMBOLS, colors)
        colors <- symbols[, 2L]
        if (anyNA(levels(x))) # ensure last selected position is white
          colors[[size]] <- WHITE
        symbols <- symbols[, 1L]
      }

      annotation <- c(base.annotation, list(
        LEGEND_TITLE = nice_str(name),
        BACKBONE_HEIGHT = 0, # controls the height of the midline
        BACKBONE_COLOR = WHITE, # controls the color of the midline
        # we are hiding it by drawing it white
        SHOW_DOMAIN_LABELS = 0,
        WIDTH = 25,
        HEIGHT_FACTOR = 1,
        LEGEND_SHAPES = symbols[seq_len(size)],
        LEGEND_COLORS = colors[seq_len(size)],
        LEGEND_LABELS = levels(x)
      ))
      print_itol_header(outfile, "DATASET_DOMAINS", annotation)
      joint <- paste(symbols[x], 0L, 10L, colors[x], as.character(x), sep = "|")
      print_itol_data(outfile, ids, 10L, joint)

    } else {

      outfile <- itol_filename(name, "colorstrip", outdir)
      colors <- select_colours(size, anyNA(levels(x)))
      annotation <- c(base.annotation, list(
        LEGEND_TITLE = nice_str(name),
        LEGEND_COLORS = colors,
        LEGEND_SHAPES = rep.int(1L, size),
        LEGEND_LABELS = levels(x),
        STRIP_WIDTH = 25
      ))
      print_itol_header(outfile, "DATASET_COLORSTRIP", annotation)
      print_itol_data(outfile, ids, colors[x], x)

    }
  }


  # Should not normally occur in input.
  #
  emit_itol_list <- function(x, ids, name, outdir, ...) {
    message(sprintf("Skipping column '%s' of mode 'list' ...", name))
  }


  # Integer vectors yield a bar chart.
  #
  emit_itol_integer <- function(x, ids, name, outdir, ...) {
    outfile <- itol_filename(name, "simplebar", outdir)
    annotation <- list(
      DATASET_LABEL = name,
      LEGEND_TITLE = nice_str(name),
      LEGEND_COLORS = BLACK,
      LEGEND_LABELS = paste0(sprintf("%s (%i)", c("Min.", "Max."),
        range(x, na.rm = TRUE)), collapse = " "),
      LEGEND_SHAPES = 1,
      WIDTH = 200,
      MARGIN = 5,
      COLOR = BLACK
    )
    print_itol_header(outfile, "DATASET_SIMPLEBAR", annotation)
    print_itol_data(outfile, ids, x)
  }


  # Vectors of mode 'double' (of class 'numeric' in R) yield a colour gradient.
  #
  emit_itol_numeric <- function(x, ids, name, outdir, end.color,
      precision, ...) {
    outfile <- itol_filename(name, "gradient", outdir)
    annotation <- list(
      DATASET_LABEL = name,
      LEGEND_TITLE = nice_str(name),
      LEGEND_COLORS = c(WHITE, end.color),
      LEGEND_LABELS = sprintf(sprintf("%%s (%%.%if)", precision),
        c("Min.", "Max."), range(x)),
      LEGEND_SHAPES = c(1L, 1L),
      STRIP_WIDTH = 50,
      MARGIN = 5,
      COLOR = "#fb9a99",
      COLOR_MIN = WHITE,
      COLOR_MAX = end.color
    )
    print_itol_header(outfile, "DATASET_GRADIENT", annotation)
    print_itol_data(outfile, ids, x)
  }


  ## Main


  # The main function, taking care of all columns of data frame 'x'.
  #
  itol_files <- function(x, lcol, bcol, icol, scol, id.pat, precision,
      max.colors, favour, strict, convert.int, outdir) {

    assort <- function(x, f) {
      idx <- split.default(seq_along(f), f)
      result <- vector(typeof(x), length(f))
      for (i in idx)
        result[i] <- rep_len(x, length(i))
      result
    }

    get_col <- function(name, x, strict) {
      if (length(name) != 1L)
        stop("need a single column name for identifying special column")
      result <- match(name, names(x), 0L)
      if (!result)
        if (strict)
          stop(sprintf(
            "selected column '%s' does not exist -- must select one of %s",
            name, paste0(sprintf("'%s'", names(x)), collapse = ", ")))
        else
          warning(sprintf("cannot find column '%s', skipping data", name))
      result
    }

    # identifier column, step 1
    idpos <- get_col(icol, x, strict)
    if (!idpos)
      return(invisible(FALSE))

    if (anyNA(x[, idpos]))
      x <- x[!is.na(x[, idpos]), , drop = FALSE]

    if (!all(dim(x))) {
      if (strict)
        stop("encountered empty data frame")
      warning("skipping empty data frame")
      return(invisible(FALSE))
    }

    # rescue 'logical' columns by conversion to factor
    for (i in which(vapply(x, is.logical, NA)))
      x[, i] <- factor(x[, i])

    # convert integers to other data types if requested
    switch(EXPR = convert.int,
      none = NULL,
      factor = for (i in which(vapply(x, is.integer, NA)))
        x[, i] <- factor(x[, i]),
      double = for (i in which(vapply(x, is.integer, NA)))
        storage.mode(x[, i]) <- "double",
      stop(sprintf("invalid integer conversion indicator '%s'", convert.int))
    )

    # identifier column, step 2
    icol <- x[, idpos]
    if (is.factor(icol))
      icol <- as.character(icol)
    icol <- sprintf(id.pat, icol)

    # must be done before the first use of 'outdir'
    if (!dir.exists(outdir))
      dir.create(outdir)

    # label column
    lpos <- get_col(lcol, x, strict)
    if (lpos)
      emit_itol_labeltexts(x = x[, lpos], ids = icol,
        name = names(x)[[lpos]], outdir = outdir)

    # background colour column
    if (length(bcol) && all(nzchar(bcol))) {
      cpos <- get_col(bcol, x, strict)
      if (cpos)
        emit_itol_labelcolors(x = x[, cpos], ids = icol,
          name = names(x)[[cpos]], outdir = outdir)
    } else {
      cpos <- 0L
    }

    # symbol-defining column
    if (length(scol) && all(nzchar(scol))) {
      spos <- get_col(scol, x, strict)
      if (spos) {
        symbols <- x[, spos]
        if (!is.factor(symbols) || length(levels(symbols)) > length(SYMBOLS)) {
          warning("column '", scol, "' is either not a factor or ",
            "has too many levels to be used for deriving symbols")
          symbols <- NULL
        } else {
          symbols <- SYMBOLS[symbols]
        }
      } else {
        symbols <- NULL
      }
    } else {
      symbols <- NULL
    }

    # normal columns, dispatch done according to data type (class)
    emit.fun <- sprintf("emit_itol_%s", vapply(x, class, ""))
    # this could be made specific for each column to yield distinct gradients
    end.color <- assort(c("#1f78b4", "#33a02c", "#e31a1c",
      "#ff7f00", "#6a3d9a", "#b15928"), emit.fun)
    key <- names(x)
    for (i in seq_along(x)[-c(idpos, lpos, cpos)])
      do.call(emit.fun[[i]], list(x = x[, i], ids = icol, name = key[[i]],
        end.color = end.color[[i]], precision = precision, outdir = outdir,
          symbols = symbols, max.colors = max.colors, favour = favour))

    invisible(TRUE)

  }

  assert_R_version()

  for (infile in infiles)
    # note that read_file() is supposed to return a list of data frames
    lapply(X = read_file(infile, opt), FUN = itol_files, bcol = opt$background,
      precision = opt$precision, lcol = opt$label, icol = opt$identifier,
      scol = opt$emblems, id.pat = opt$template, max.colors = opt$`max-colors`,
      favour = opt$favour, outdir = opt$directory, strict = opt$abort,
      convert.int = opt$conversion)

  invisible(NULL)

}


################################################################################
#
# Option processing
#


option.parser <- optparse::OptionParser(option_list = list(

  optparse::make_option(c("-a", "--abort"), action = "store_true",
    help = "Abort if a column cannot be found [default: %default]",
    default = FALSE),

  optparse::make_option(c("-b", "--background"), type = "character",
    help = "Name of the background colour column [default: %default]",
    metavar = "NAME", default = ""),

  optparse::make_option(c("-c", "--conversion"), type = "character",
    help = "Conversion of integers to other data types [default: %default]",
    metavar = "NAME", default = "none"),

  optparse::make_option(c("-d", "--directory"), type = "character",
    help = "Name of the output directory [default: %default]",
    metavar = "DIR", default = "."),

  optparse::make_option(c("-e", "--emblems"), type = "character",
    help = "Name of the symbol-defining column [default: %default]",
    metavar = "NAME", default = ""),

  optparse::make_option(c("-f", "--favour"), type = "numeric",
    help = "Factor for favouring colours over symbols [default: %default]",
    metavar = "NUMBER", default = 1),

  optparse::make_option(c("-h", "--help"), action = "store_true",
    default = FALSE,
    help = "Show this help message, then exit [default: %default]"),

  optparse::make_option(c("-i", "--identifier"), type = "character",
    help = "Name of the identifer column [default: %default]",
    metavar = "NAME", default = "ID"),

  optparse::make_option(c("-l", "--label"), type = "character",
    help = "Name of the label column [default: %default]",
    metavar = "NAME", default = "Label"),

  optparse::make_option(c("-m", "--max-colors"), type = "integer",
    help = "Cutoff for # levels to switch to symbols [default: %default]",
    metavar = "INTEGER", default = 20L),

  optparse::make_option(c("-n", "--na-strings"), type = "character",
    help = "Strings to interpret as non-available values [default: %default]",
    metavar = "TEXT", default = "\t(null)\tNA"),

  optparse::make_option(c("-p", "--precision"), type = "integer",
    help = "Number of decimal points for gradient legends [default: %default]",
    metavar = "INTEGER", default = 1L),

  optparse::make_option(c("-s", "--separator"), type = "character",
    help = "Input column separator [default: %default]",
    metavar = "CHARACTER", default = "\t"),

  optparse::make_option(c("-t", "--template"), type = "character",
    help = "Template for sprintf to convert ID column [default: %default]",
    metavar = "PATTERN", default = "%s")

), add_help_option = FALSE, description = "
%prog : script for converting spreadsheet files to iToL input.",
epilogue = "
You need the readxl and readODS packages for applying this script to Excel and
Libreoffice or Openoffice ods files, respectively.

IMPORTANT OPTIONS:

-b\tOptional column that defines the background colour of the labels.
-c\tConversion of integers to other data types: 'factor', 'double' or 'none'.
-i\tUnless name of identifier column happens to match the default.
-l\tUnless the name of the label column happens to match the default.
-n\tThe sentinel(s) used to indicate missing values in input. Several may be
  \tprovided, separated by the value of the '-s' option.
-s\tUnless the separator character happens to match the default.
-t\tTemplate for modifying the identifier column, e.g. prepending something.

USE OF DATA TYPES:

character, integer, logical -> factor -> iToL domains
integer -> double -> iToL gradient
integer -> iToL simplebar

EXAMPLES:

# set the most relevant relevant columns:
'%prog -i Genome_ID -l Strain -b Phylum annotation.tsv'

# prepend 'T' to ID column 'Genome_ID', which contains integers:
'%prog -i Genome_ID -l Strain -b Phylum -t T%i annotation.tsv'
"
)

opt <- optparse::parse_args(object = option.parser,
  positional_arguments = TRUE)
infiles <- opt$args
opt <- opt$options
opt$`na-strings` <- unlist(strsplit(opt$`na-strings`,
  opt$separator, TRUE), FALSE, FALSE)


################################################################################


if (length(infiles) > 0L) {
  create_itol_files(infiles, opt)
} else {
  optparse::print_help(option.parser)
  if (interactive())
    message("
********************************************************************************

Apparently this script is running in interactive mode. You could now modify the
'opt' variable by hand, set the 'infiles' variable to a vector of file names,
and then call:

create_itol_files(infiles, opt)

********************************************************************************
    ")
  else
    quit("no", 1L)
}


################################################################################

