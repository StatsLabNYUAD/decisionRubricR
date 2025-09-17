#' Rescaled Matthews correlation coefficient
#'
#' Maps the usual [-1, 1] range to [0, 1] so that higher is always better.
#' @noRd
mcc_rescaled <- function(truth, decision) {
  TP <- sum(truth == 1 & decision == 1)
  TN <- sum(truth == 0 & decision == 0)
  FP <- sum(truth == 0 & decision == 1)
  FN <- sum(truth == 1 & decision == 0)
  denom <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  mcc   <- if (denom == 0) 0 else (TP * TN - FP * FN) / denom
  (mcc + 1) / 2
}

#' Cronbach's alpha for a matrix/data frame of rubric components
#' @param mat numeric matrix/data frame with columns = rubric parts
#' @return raw Cronbach's alpha
#' @export
alpha_rubric <- function(mat) {
  psych::alpha(mat)$total$raw_alpha
}

#' Spearman correlation between composite score and a latent trait
#' @param comp numeric vector of composite scores
#' @param latent numeric vector of latent ability/skill
#' @return Spearman's rho
#' @export
skill_corr <- function(comp, latent) {
  stats::cor(comp, latent, method = "spearman")
}
