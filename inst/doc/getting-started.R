## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
colleyRstats::colleyRstats_setup(
  set_options = FALSE,
  set_theme = FALSE,
  set_conflicts = FALSE,
  print_citation = FALSE,
  verbose = FALSE
)

## -----------------------------------------------------------------------------
set.seed(123)

main_df <- data.frame(
  Participant = factor(rep(1:20, each = 2)),
  ConditionID = factor(rep(c("Control", "Treatment"), times = 20)),
  score = rnorm(40, mean = rep(c(50, 55), times = 20), sd = 8)
)

## -----------------------------------------------------------------------------
colleyRstats::check_normality_by_group(main_df, "ConditionID", "score")
colleyRstats::check_homogeneity_by_group(main_df, "ConditionID", "score")

## -----------------------------------------------------------------------------
colleyRstats::generateEffectPlot(
  data = transform(main_df, Group = ConditionID),
  x = "ConditionID",
  y = "score",
  fillColourGroup = "Group",
  ytext = "Score",
  xtext = "Condition"
)

## -----------------------------------------------------------------------------
art_summary <- data.frame(
  Effect = "ConditionID",
  Df = 1,
  `F value` = 5.42,
  `Pr(>F)` = 0.027,
  Df.res = 19,
  check.names = FALSE
)

colleyRstats::reportART(art_summary, dv = "score")

