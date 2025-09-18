#' Score a set of probabilistic predictions with the teaching rubric
#'
#' Computes five components (calibration-slope proximity to 1, AUC,
#' rescaled MCC, Brier Skill Score, and decision-curve Net Benefit)
#' and a weighted composite.
#'
#' @param truth Integer/binary vector (0/1) of true outcomes.
#' @param prob  Numeric vector of predicted probabilities in [0,1].
#' @param tau   Decision threshold (default 0.30).
#' @param n_bins Number of quantile bins for calibration slope (default 10).
#' @return A named list with the five component scores and the composite.
#' @export
score_rubric <- function(truth, prob,
                         tau     = 0.30,
                         n_bins  = 10) {

  stopifnot(length(truth) == length(prob))
  stopifnot(all(truth %in% c(0,1)))
  stopifnot(all(is.finite(prob)) && all(prob >= 0 & prob <= 1))

  ## 1) Calibration slope (proximity to 1)
  cal_tbl <- tibble::tibble(truth = truth, prob = prob) |>
    dplyr::mutate(bin = dplyr::ntile(prob, n_bins)) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(midpoint   = mean(prob),
                     event_rate = mean(truth),
                     .groups    = "drop")
  slope <- stats::coef(stats::lm(event_rate ~ midpoint, data = cal_tbl))[2]
  m1    <- unname(1 - abs(slope - 1))

  ## 2) AUC
  m2 <- as.numeric(pROC::auc(response = truth, predictor = prob,
                             levels = c(0,1), direction = "<", quiet = TRUE))

  ## 3) MCC (rescaled to [0,1])
  decision <- ifelse(prob > tau, 1, 0)  # keep as numeric to avoid integer overflow
  m3 <- mcc_rescaled(truth, decision)

  ## 4) Brier Skill Score
  bs  <- mean((prob  - truth)^2)
  ref <- mean((mean(truth) - truth)^2)
  m4  <- if (ref == 0) 1 else 1 - bs / ref

  ## 5) Net Benefit (rescaled via theoretical range)
  TP <- sum(truth == 1 & decision == 1)
  FP <- sum(truth == 0 & decision == 1)
  n  <- length(truth)
  nb_raw <- TP/n - FP/n * tau/(1 - tau)

  nb_min <- -tau/(1 - tau)   # worst case: all FPs
  nb_max <- 1                # best case: perfect TPs, no FP

  m5 <- (nb_raw - nb_min) / (nb_max - nb_min)
  m5 <- max(0, min(1, m5))   # clamp to [0,1]

  ## Composite
  comp <- unname(0.25*m1 + 0.20*m2 + 0.15*m3 + 0.20*m4 + 0.20*m5)

  list(cal = m1, auc = m2, mcc = m3, bss = m4, nb = m5, comp = comp)
}
