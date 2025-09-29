#' Score rubric metrics and composite
#'
#' Computes five components (calibration slope proximity to 1, AUC,
#' rescaled MCC, Brier Skill Score, and decision-curve Net Benefit)
#' and a weighted composite.
#'
#' @param truth Integer/logical vector (0/1).
#' @param prob  Numeric vector in [0, 1].
#' @param tau   Decision threshold in (0, 1), default 0.30.
#' @param weights Numeric length-5 (named or unnamed) giving weights for
#'   Cal, AUC, MCC, BSS, NB. Default = c(Cal=.25, AUC=.20, MCC=.15, BSS=.20, NB=.20).
#' @param normalize Logical; if TRUE, renormalize weights to sum to 1.
#' @return A list with elements:
#'   \item{cal, auc, mcc, bss, nb}{the five sub-metrics in [0,1]}
#'   \item{components}{named numeric vector: Cal, AUC, MCC, BSS, NB}
#'   \item{weights_used}{numeric length-5 used after any renormalization}
#'   \item{composite, comp}{the weighted composite in [0,1]}
#' @details
#' * Calibration: m1 = 1 - |s - 1| clamped to [0,1], where s is the slope of
#'   event rate vs predicted midpoint. Simple and monotone; plateaus for slopes
#'   in [0,2]. Consider a quadratic penalty if more sensitivity near 1 is needed.
#' * MCC: rescaled from [-1,1] to [0,1]. If denominator=0 (degenerate),
#'   returns 0.5 (neutral).
#' * Net Benefit: affine-rescaled to [0,1]. Requires prevalence > 0; a small
#'   epsilon protects against division by zero when prevalence is very small.
#' * Composite: convex combination with weights summing to 1, so Composite ∈ [0,1].
#' @importFrom stats lm coef
#' @importFrom pROC auc
#' @export
score_rubric <- function(truth, prob, tau = 0.30,
                         weights = c(Cal=.25, AUC=.20, MCC=.15, BSS=.20, NB=.20),
                         normalize = TRUE) {

  stopifnot(length(truth) == length(prob))
  stopifnot(all(truth %in% c(0, 1)))
  stopifnot(all(is.finite(prob)) && all(prob >= 0 & prob <= 1))
  stopifnot(is.numeric(tau), length(tau) == 1L, is.finite(tau), tau > 0, tau < 1)

  ## --- 1) Calibration slope ------------------------------------------
  n_bins <- 10L
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
  slope <- tryCatch(stats::coef(stats::lm(event_rate ~ midpoint, data = cal_tbl))[2],
                    error = function(e) NA_real_)
  if (!is.finite(slope)) slope <- 0  # fallback
  m1 <- 1 - abs(slope - 1)
  m1 <- pmin(1, pmax(0, m1))

  ## --- 2) AUC ---------------------------------------------------------
  m2 <- as.numeric(pROC::auc(response = truth, predictor = prob,
                             levels = c(0, 1), direction = "<", quiet = TRUE))

  ## --- 3) MCC (rescaled) ----------------------------------------------
  decision <- ifelse(prob >= tau, 1L, 0L)
  TP <- sum(decision == 1L & truth == 1L)
  TN <- sum(decision == 0L & truth == 0L)
  FP <- sum(decision == 1L & truth == 0L)
  FN <- sum(decision == 0L & truth == 1L)
  num <- TP * TN - FP * FN
  den <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
  raw_mcc <- ifelse(den == 0, NA_real_, num/den)
  m3 <- ifelse(is.na(raw_mcc), 0.5, (raw_mcc + 1)/2)

  ## --- 4) Brier Skill Score -------------------------------------------
  bs  <- mean((prob - truth)^2)
  ref <- mean((mean(truth) - truth)^2)
  m4  <- if (ref == 0) 1 else 1 - bs / ref

  ## --- 5) Net Benefit -------------------------------------------------
  n  <- length(truth)
  TP <- sum(truth == 1L & decision == 1L)
  FP <- sum(truth == 0L & decision == 1L)
  nb_raw <- TP/n - FP/n * tau/(1 - tau)
  nb_min <- -tau/(1 - tau); nb_max <- 1
  rng <- nb_max - nb_min
  if (!is.finite(rng) || rng <= 0) rng <- 1e-8
  m5 <- (nb_raw - nb_min) / rng
  m5 <- pmin(1, pmax(0, m5))

  components <- c(Cal = m1, AUC = m2, MCC = m3, BSS = m4, NB = m5)

  ## --- weights --------------------------------------------------------
  if (!is.null(names(weights))) {
    weights <- weights[c("Cal","AUC","MCC","BSS","NB")]
  }
  stopifnot(length(weights) == 5)
  w <- as.numeric(weights)
  if (isTRUE(normalize)) {
    s <- sum(w)
    if (is.finite(s) && s > 0) w <- w / s
  }
  comp <- sum(w * components)

  list(
    cal = m1, auc = m2, mcc = m3, bss = m4, nb = m5,
    components   = components,
    weights_used = w,
    composite    = comp,
    comp         = comp   # backward-compatible alias
  )
}
