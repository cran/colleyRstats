## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

# The ART section needs ARTool and emmeans (both in Suggests). Guard those
# chunks so the vignette still builds without them.
has_art <- requireNamespace("ARTool", quietly = TRUE) &&
  requireNamespace("emmeans", quietly = TRUE)

## -----------------------------------------------------------------------------
library(colleyRstats)

## -----------------------------------------------------------------------------
set.seed(42)
n <- 24

main_df <- data.frame(
  Participant = factor(rep(seq_len(n), each = 3)),
  ConditionID = factor(rep(c("Baseline", "HUD", "LED"), times = n))
)

cond_effect <- c(Baseline = 55, HUD = 46, LED = 44)[as.character(main_df$ConditionID)]
main_df$tlx_mental <- pmin(100, pmax(0, round(cond_effect + rnorm(nrow(main_df), sd = 10))))
main_df$trust <- pmin(7, pmax(1, round(3.5 +
  (main_df$ConditionID != "Baseline") * 0.9 + rnorm(nrow(main_df), sd = 1))))

## -----------------------------------------------------------------------------
res <- analyze_and_report(
  main_df,
  dv = "tlx_mental", iv = "ConditionID",
  design = "within",
  ylab = "Mental Demand (TLX)"
)

## -----------------------------------------------------------------------------
res$plot

## -----------------------------------------------------------------------------
res$methods  # for the Methods section
res$text     # the omnibus result
res$posthoc  # significant pairwise comparisons (NULL for 2 groups)

## -----------------------------------------------------------------------------
battery <- report_all(
  main_df,
  dvs = c("tlx_mental", "trust"),
  iv = "ConditionID",
  design = "within",
  labels = c(tlx_mental = "Mental Demand", trust = "Trust")
)
battery$summary

## -----------------------------------------------------------------------------
check_normality_by_group(main_df, "ConditionID", "tlx_mental")

## -----------------------------------------------------------------------------
assumption_methods_text(main_df, x = "ConditionID", y = "tlx_mental")

## -----------------------------------------------------------------------------
plot_within_stats_asterisk(
  data = main_df,
  x = "ConditionID", y = "tlx_mental",
  ylab = "Mental Demand (TLX)",
  xlabels = c("Baseline", "HUD", "LED")
)

## ----eval = has_art-----------------------------------------------------------
m <- ARTool::art(
  tlx_mental ~ ConditionID + Error(Participant / ConditionID),
  data = main_df
)
reportART(anova(m), dv = "mental demand")

## ----eval = has_art-----------------------------------------------------------
ac <- ARTool::art.con(m, ~ ConditionID, adjust = "holm")
reportArtCon(
  ac,
  data = main_df, iv = "ConditionID", dv = "tlx_mental",
  paired = TRUE, id = "Participant"
)

## -----------------------------------------------------------------------------
reportMeanAndSD(main_df, iv = "ConditionID", dv = "tlx_mental")

## -----------------------------------------------------------------------------
fig_path <- file.path(tempdir(), "tlx-mental.pdf")
save_paper_figure(res$plot, fig_path, columns = 2)

