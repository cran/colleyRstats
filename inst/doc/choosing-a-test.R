## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# The mixed-model fits and their reporters need packages that live in
# Suggests. Guard the dependent chunks so the vignette still builds without
# them; the classify_outcome()/recommend_test() chunks need none of these.
has_mixed <- requireNamespace("lme4", quietly = TRUE) &&
  requireNamespace("ordinal", quietly = TRUE) &&
  requireNamespace("parameters", quietly = TRUE)

## -----------------------------------------------------------------------------
library(colleyRstats)

## -----------------------------------------------------------------------------
set.seed(1)
n_id <- 24
d <- data.frame(
  id   = factor(rep(seq_len(n_id), each = 3)),
  cond = factor(rep(c("A", "B", "C"), times = n_id))
)

# Give condition a genuine effect so the example models are well identified.
step <- c(A = 0, B = 1.3, C = 2.4)[as.character(d$cond)]
d$score   <- as.numeric(step + rnorm(nrow(d)))
d$rating  <- ordered(pmin(5L, pmax(1L, round(step + rnorm(nrow(d), sd = 0.7) + 2))))
d$correct <- rbinom(nrow(d), 1, plogis(step - 1))

classify_outcome(d$score)    # continuous
classify_outcome(d$rating)   # ordinal (ordered factor)
classify_outcome(d$correct)  # binary (two distinct values)

## -----------------------------------------------------------------------------
rec_clmm <- recommend_test(d, outcome = "rating", predictors = "cond", cluster = "id")
rec_clmm

## -----------------------------------------------------------------------------
rec_glmm <- recommend_test(d, outcome = "correct", predictors = "cond", cluster = "id")
rec_glmm

## -----------------------------------------------------------------------------
rec_anova <- recommend_test(d, outcome = "score", predictors = "cond")
rec_anova

## -----------------------------------------------------------------------------
rec_clmm$model_function
rec_clmm$reporter
rec_clmm$fit_call
cat(rec_glmm$methods_text)

## ----eval = has_mixed---------------------------------------------------------
m_clmm <- ordinal::clmm(rating ~ cond + (1 | id), data = d)
reportCLMM(m_clmm, dv = "rating")

## ----eval = has_mixed---------------------------------------------------------
m_glmm <- lme4::glmer(correct ~ cond + (1 | id), data = d, family = binomial)
reportGLMM(m_glmm, dv = "accuracy")

## ----eval = has_mixed---------------------------------------------------------
m_lmm <- lme4::lmer(score ~ cond + (1 | id), data = d)
reportGLMM(m_lmm, dv = "score")

