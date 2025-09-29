#' Score rubric metrics and composite
#'
#' Computes five components (calibration slope proximity to 1, AUC,
#' rescaled MCC, Brier Skill Score, and decision-curve Net Benefit)
#' and a weighted composite.
#'
#' @param truth integer/logical vector (0/1).
#' @param prob  numeric in [0,1].
#' @param tau   decision threshold (default 0.30).
#' @param weights numeric length-5 (named or unnamed) giving weights for
#'   Cal, AUC, MCC, BSS, NB. Default = c(.25, .20, .15, .20, .20).
#' @param normalize logical; if TRUE, renormalize weights to sum to 1.
#' @return list with sub-metrics, weights_used, and composite.
#' @details
#' * Calibration: m1 = 1 - |s - 1| clamped to [0,1], where s is the slope
#'   of event rate vs predicted midpoint. This is simple and monotone but
#'   plateaus when |s - 1| ≤ 0; pathological slopes (≈0) map to 0.
#'   Consider quadratic penalties if more sensitivity around 1 is needed.
#' * MCC: rescaled from [-1,1] to [0,1]. If denominator=0 (degenerate),
#'   returns 0.5 (neutral).
#' * Net Benefit: affine-rescaled to [0,1]. Requires prevalence π > 0;
#'   a small epsilon prevents division by zero when π≈0.
#' * Composite: convex combination of components with weights summing to 1,
#'   so Composite ∈ [0,1].
#' @export
score_rubric <- function(truth, prob, tau = 0.30,
                         weights = NULL, normalize = TRUE) {

  stopifnot(length(truth) == length(prob))
  stopifnot(all(truth %in% c(0, 1)))
  stopifnot(all(is.finite(prob)) && all(prob >= 0 & prob <= 1))

  ## --- 1) Calibration slope ------------------------------------------
  n_bins <- 10
  cal_tbl <- dplyr::summarise(
    dplyr::group_by(
      dplyr::mutate(tibble::tibble(truth = truth, prob = prob),
                    bin = dplyr::ntile(prob, n_bins)),
      bin
    ),
    midpoint   = mean(prob),
    event_rate = mean(truth),
    .groups    = "drop"
  )
  slope <- tryCatch(
    stats::coef(stats::lm(event_rate ~ midpoint, data = cal_tbl))[2],
    error = function(e) NA_real_
  )
  m1 <- 1 - abs(slope - 1)
  m1 <- max(0, min(1, m1))

  ## --- 2) AUC ---------------------------------------------------------
  m2 <- as.numeric(
    pROC::auc(response = truth, predictor = prob,
              levels = c(0, 1), direction = "<", quiet = TRUE)
  )

  ## --- 3) MCC (rescaled) ----------------------------------------------
  decision <- ifelse(prob > tau, 1, 0)
  TP <- sum(decision == 1 & truth == 1)
  TN <- sum(decision == 0 & truth == 0)
  FP <- sum(decision == 1 & truth == 0)
  FN <- sum(decision == 0 & truth == 1)
  num <- TP * TN - FP * FN
  den <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
  raw_mcc <- ifelse(den == 0, NA, num/den)
  m3 <- ifelse(is.na(raw_mcc), 0.5, (raw_mcc + 1)/2)

  ## --- 4) Brier Skill Score -------------------------------------------
  bs  <- mean((prob - truth)^2)
  ref <- mean((mean(truth) - truth)^2)
  m4  <- if (ref == 0) 1 else 1 - bs / ref

  ## --- 5) Net Benefit -------------------------------------------------
  n <- length(truth)
  TP <- sum(truth == 1 & decision == 1)
  FP <- sum(truth == 0 & decision == 1)
  nb_raw <- TP/n - FP/n * tau/(1 - tau)
  nb_min <- -tau/(1 - tau); nb_max <- 1
  rng <- nb_max - nb_min
  if (rng <= 0) rng <- 1e-8
  m5 <- (nb_raw - nb_min) / rng
  m5 <- max(0, min(1, m5))

  components <- c(Cal = m1, AUC = m2, MCC = m3, BSS = m4, NB = m5)

  ## --- weights --------------------------------------------------------
  if (is.null(weights)) {
    w <- c(.25, .20, .15, .20, .20)
  } else {
    if (!is.null(names(weights))) {
      weights <- weights[c("Cal","AUC","MCC","BSS","NB")]
    }
    stopifnot(length(weights) == 5)
    w <- as.numeric(weights)
  }
  if (isTRUE(normalize)) {
    s <- sum(w)
    if (is.finite(s) && s > 0) w <- w / s
  }
  comp <- sum(w * components)

  list(
    cal = m1, auc = m2, mcc = m3, bss = m4, nb = m5,
    components  = components,
    weights_used = w,
    composite   = comp,
    comp        = comp   # backward-compatible alias
  )
}
