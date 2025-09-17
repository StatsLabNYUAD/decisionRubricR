#' Score a set of probabilistic predictions with the teaching rubric
#'
#' Computes five components (calibration slope proximity to 1, AUC,
#' rescaled MCC, Brier Skill Score, and decision-curve Net Benefit)
#' and a weighted composite.
#'
#' @param truth Integer/binary vector (0/1) of true outcomes.
#' @param prob  Numeric vector of predicted probabilities in [0,1].
#' @param tau   Decision threshold (default 0.30).
#' @param n_bins Number of quantile bins for calibration slope (default 10).
#'
#' @return A named list with the five component scores and the composite.
#' @export
score_rubric <- function(truth, prob,
                         tau     = 0.30,
                         n_bins  = 10) {
   ## ... (paste the full new body here) ...
}
