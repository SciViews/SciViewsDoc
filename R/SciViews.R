# Base R language for SciViews::R
# to be sourced in 'SciViews:R'
# by Ph. Grosjean (2023)


# Functions for the SciViews::R Quarto book -------------------------------

# Read the csv file that contains correspondence of functions SciViews::R <-> R
fn_read <- function() {
  if (file.exists("fns.csv")) {
    read.csv("fns.csv")
  } else {
    data.frame(
      svr     = character(0),
      r       = character(0),
      cat     = character(0),
      comment = character(0)
    )
  }
}

# Write the same file
fn_write <- function(fns) {
  write.csv(fns, file = "fns.csv", row.names = FALSE)
}

# Clear the file (at the beginning of the book)
fn_clear <- function()
  unlink("fns.csv")

# List functions presented in the book and shows the correspondence between
# SciViews::R and R functions
# TODO: automatically add a link to the man page of the function
fn_list <- function(svr, r = svr, cat = "", comment = "Same syntax") {
  fns <- get0("fns")
  if (is.null(fns))
    fns <- fn_read()
  if (!nrow(fns)) {
    fns <- data.frame(svr = svr, r = r, cat = cat, comment = comment)
    fn_write(fns)
  } else if (!svr %in% fns$svr) {
    fns <- rbind(fns, data.frame(svr = svr, r = r, cat = cat,
      comment = comment))
    fn_write(fns)
  }
  fns <<- fns
  #idx <- sub("\\(\\)", "", svr)
  idx <- svr
  # Special cases
  idx <- switch(idx,
    "=" = "assignment",
    "<-" = "assignment",
    "->" = "assignment",
    "<<-" = "superassignment",
    "->>" = "superassignment",
    ":" = "sequence",
    "+" = "addition",
    "-" = "subtraction",
    "*" = "multiplication",
    "%*%" = "matrix multiplication",
    "/" = "division",
    "%/%" = "integer division",
    "%%" = "modulo",
    "^" = "exponentiation",
    "==" = "equality",
    "!=" = "difference",
    "<" = "smaller than",
    ">" = "greater than",
    "<=" = "smaller or equal",
    ">=" = "greater or equal",
    "%in%" = "inclusion",
    "!" = "negation",
    "|" = "or elementwise",
    "||" = "or",
    "&" = "and elementwise",
    "&&" = "and",
    "$" = "extract",
    idx
  )
  idx <- gsub("%", "\\\\%", idx)
  idx <- gsub("\\$", "\\\\$", idx)
  idx <- gsub("_", "\\\\_", idx)
  paste0("`", svr, "`\\index{", idx,"}")
}


# SciViews::R aka functions -----------------------------------------------

aka <- svMisc::aka

all_equal <- aka(base::all.equal)
all_names <- aka(base::all.names)
all_vars <- aka(base::all.vars)
arr_split <- aka(base::asplit)
mat_split <- aka(base::asplit)
attr_all_equal <- aka(base::attr.all.equal)
pmin_int <- aka(base::pmin.int)
pmax_int <- aka(base::pmax.int)
is_true <- aka(base::isTRUE)
is_false <- aka(base::isFALSE)
# No, should be vectorized... where is this defined?
#`%==%` <- function(x, y) {
#  all.equal(x, y)
#}
#`%!=%` <- function(x, y) {
#  !all.equal(x, y)
#}
get_s3_method <- aka(utils::getS3method)

library(cli)
ansi_num_colors <- aka(cli::num_ansi_colors)
ansi_combine_styles <- aka(cli::combine_ansi_styles)
ansi_make_style <- aka(cli::make_ansi_style)
ansi_truecolor <- aka(cli::truecolor)
