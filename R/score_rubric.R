#' Score a five-component decision-quality rubric
#'
#' Computes five metrics—calibration-slope proximity, AUC, rescaled
#' Matthews correlation coefficient (MCC), Brier Skill Score (BSS) and
#' decision-curve Net Benefit (NB)—and returns a weighted composite.
#'
#' @param truth Integer or logical vector of 0/1 outcomes.
#' @param prob  Numeric vector of predicted probabilities in [0,1].
#' @param tau   Decision threshold ( default 0.30 ).
#' @param weights Numeric vector of length 5 giving the weights for
#'   Cal, AUC, MCC, BSS, NB.  If `NULL` the default
#'   `c(.25, .20, .15, .20, .20)` is used.
#' @param normalize Logical; if `TRUE` (default) the supplied `weights`
#'   are renormalised to sum to 1.
#'
#' @details
#' • Calibration: \eqn{m_1 = 1 - |s-1|}, clamped to [0,1], where *s* is
#'   the calibration slope.  The function bins the probabilities
#'   (≈ ≥ 20 points per bin, max 10 bins) and regresses event rate on
#'   midpoint.  Slopes in [0,2] therefore score ≥ 0.\cr
#' • AUC: computed with `pROC::auc(..., direction = "auto")`, so it is
#'   never < 0.5.\cr
#' • MCC: rescaled from [-1,1] to [0,1]; undefined cases map to 0.5
#'   (neutral).\cr
#' • BSS: quadratic skill score relative to prevalence forecast,
#'   rescaled to [0,1].\cr
#' • NB: decision-curve net benefit at `tau`, affine-rescaled to [0,1].
#'
#' The composite is a convex combination of the five components and is
#' therefore guaranteed to lie in [0,1].
#'
#' @return A list with the individual components (`cal`, `auc`, `mcc`,
#'   `bss`, `nb`), the complete named vector `components`, the
#'   `weights_used`, and the scalar `composite`.
#' @export
score_rubric <- function(truth, prob, tau = 0.30,
                         weights = NULL, normalize = TRUE) {

  ## -- 0) Basic validation --------------------------------------------
  stopifnot(length(truth)      == length(prob),
            all(truth %in% c(0, 1)),
            all(is.finite(prob)) && all(prob >= 0 & prob <= 1),
            tau > 0 && tau < 1,
            length(truth) >= 5)

  if (sum(truth) == 0 || sum(truth) == length(truth)) {
    warning("Only one class present in truth vector")
  }

  if (!requireNamespace("pROC",   quietly = TRUE) ||
      !requireNamespace("dplyr",  quietly = TRUE) ||
      !requireNamespace("tibble", quietly = TRUE)) {
    stop("Packages 'pROC', 'dplyr', and 'tibble' are required.")
  }

  ## -- 1) Calibration slope -------------------------------------------
  min_bin_size <- 20
  n_bins <- pmin(10,                                   # upper bound
                 max(2, floor(length(truth) /
                               min_bin_size)))         # lower bound

  if (length(unique(prob)) < 3) {                      # degenerate input
    slope <- 1.0
  } else {
    cal_tbl <- tryCatch(
      dplyr::summarise(
        dplyr::group_by(
          dplyr::mutate(tibble::tibble(truth = truth, prob = prob),
                        bin = dplyr::ntile(prob, n_bins)),
          bin),
        midpoint   = mean(prob),
        event_rate = mean(truth),
        .groups    = "drop"),
      error = function(e) NULL)

    if (is.null(cal_tbl) || anyNA(cal_tbl$midpoint)) {
      slope <- 1.0
    } else {
      slope <- tryCatch({
        coef_stats <- stats::coef(stats::lm(event_rate ~ midpoint,
                                            data = cal_tbl))
        if (length(coef_stats) >= 2) coef_stats[2] else 1.0
      }, error = function(e) 1.0)
    }
  }

  if (is.na(slope) || !is.finite(slope)) slope <- 1.0
  m1 <- 1 - abs(slope - 1)
  m1 <- max(0, min(1, m1))

  ## -- 2) AUC ----------------------------------------------------------
  m2 <- tryCatch({
    if (sum(truth) == 0 || sum(truth) == length(truth)) {
      0.5
    } else {
      as.numeric(
        pROC::auc(response  = truth,
                  predictor = prob,
                  levels    = c(0, 1),
                  direction = "auto",
                  quiet     = TRUE))
    }
  }, error = function(e) {
    warning("AUC calculation failed; returning 0.5")
    0.5
  })

  if (is.na(m2) || !is.finite(m2)) m2 <- 0.5
  m2 <- max(0.5, min(1, m2))       # final clamp (theoretical)

  ## -- 3) MCC (rescaled) ----------------------------------------------
  decision <- ifelse(prob > tau, 1, 0)
  TP <- sum(decision == 1 & truth == 1)
  TN <- sum(decision == 0 & truth == 0)
  FP <- sum(decision == 1 & truth == 0)
  FN <- sum(decision == 0 & truth == 1)

  num <- TP * TN - FP * FN
  den <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

  raw_mcc <- if (den == 0 || !is.finite(den)) NA else num / den
  if (is.na(raw_mcc) || !is.finite(raw_mcc)) raw_mcc <- 0
  m3 <- (raw_mcc + 1) / 2                # map [-1,1] → [0,1]
  m3 <- max(0, min(1, m3))

  ## -- 4) Brier Skill Score -------------------------------------------
  bs  <- mean((prob - truth)^2)
  ref <- mean((mean(truth) - truth)^2)   # π(1-π)

  m4 <- if (ref == 0 || !is.finite(ref)) 0.5 else 1 - bs / ref
  m4 <- max(0, min(1, m4))

  ## -- 5) Net Benefit --------------------------------------------------
  n          <- length(truth)
  prevalence <- mean(truth)

  TP_nb <- TP      # already computed above
  FP_nb <- FP

  nb_raw <- TP_nb / n - FP_nb / n * tau / (1 - tau)

  nb_min <- -tau / (1 - tau)
  nb_max <- prevalence
  rng    <- nb_max - nb_min

  m5 <- if (rng <= 1e-8) 0.5 else (nb_raw - nb_min) / rng
  m5 <- max(0, min(1, m5))

  ## -- Combine ---------------------------------------------------------
  components <- c(Cal = m1, AUC = m2, MCC = m3, BSS = m4, NB = m5)

  if (is.null(weights)) {
    w <- c(0.25, 0.20, 0.15, 0.20, 0.20)
  } else {
    if (!is.null(names(weights))) {
      exp_names <- c("Cal", "AUC", "MCC", "BSS", "NB")
      if (!all(exp_names %in% names(weights)))
        stop("Named weights must include: Cal, AUC, MCC, BSS, NB")
      weights <- weights[exp_names]
    }
    stopifnot(length(weights) == 5, all(weights >= 0), all(is.finite(weights)))
    w <- as.numeric(weights)
  }

  if (normalize) {
    s <- sum(w)
    if (!is.finite(s) || s == 0) {
      warning("Invalid weights; falling back to equal weighting")
      w <- rep(0.2, 5)
    } else {
      w <- w / s
    }
  }

  if (abs(sum(w) - 1) > 1e-10) w <- w / sum(w)  # just in case

  comp <- sum(w * components)
  comp <- max(0, min(1, comp))

  ## -- Return ----------------------------------------------------------
  list(
    cal        = m1,
    auc        = m2,
    mcc        = m3,
    bss        = m4,
    nb         = m5,
    components = components,
    weights_used = w,
    composite    = comp
  )
}
