## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
has_tables <- requireNamespace("FSA", quietly = TRUE) &&
  requireNamespace("xtable", quietly = TRUE)

## -----------------------------------------------------------------------------
library(colleyRstats)

## -----------------------------------------------------------------------------
latex_preamble()

## -----------------------------------------------------------------------------
sty_path <- use_colleyrstats_sty(tempdir(), overwrite = TRUE)
sty_path

## -----------------------------------------------------------------------------
cat(expand_latex_macros("A significant effect (\\F{2}{57}{4.50}, \\p{0.012})."))

## -----------------------------------------------------------------------------
latex_escape("tlx_mental (%)")

## ----eval = has_tables--------------------------------------------------------
set.seed(1)
tbl_df <- data.frame(
  group = factor(rep(c("A", "B", "C"), each = 12)),
  score = c(rnorm(12, 50), rnorm(12, 54), rnorm(12, 58))
)

reportDunnTestTable(
  data = tbl_df,
  iv = "group",
  dv = "score",
  style = "booktabs"
)

## -----------------------------------------------------------------------------
define_result_macro("tlx_mental_omnibus", "F(2, 57) = 4.50, p = .02")

## -----------------------------------------------------------------------------
emit_name_macros(c("Video", "DriverPosition"))

## -----------------------------------------------------------------------------
study <- list(results = list(
  workload = list(
    sentences = "A significant main effect of \\Video on workload (\\F{2}{57}{4.50}, \\p{0.012}).",
    plot = NULL
  ),
  trust = list(
    sentences = "No significant effect on trust (\\p{0.45}).",
    plot = NULL
  )
))

out <- emit_overleaf(study, dir = file.path(tempdir(), "paper"), overwrite = TRUE)
list.files(out$dir, recursive = TRUE)

## -----------------------------------------------------------------------------
old <- options(colleyRstats.macros = FALSE)
plain <- emit_overleaf(study, dir = file.path(tempdir(), "paper-plain"), overwrite = TRUE)
options(old)

list.files(plain$dir, recursive = TRUE)

