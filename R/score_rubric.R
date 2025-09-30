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

  # Input validation
  stopifnot(length(truth) == length(prob))
  stopifnot(all(truth %in% c(0, 1)))
  stopifnot(all(is.finite(prob)) && all(prob >= 0 & prob <= 1))
  stopifnot(tau > 0 && tau < 1)
  stopifnot(length(truth) >= 5)  # Minimum sample size
  
  # Check for both classes present
  if (sum(truth) == 0 || sum(truth) == length(truth)) {
    warning("Only one class present in truth vector")
  }

  # Check for required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed for this function to work.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' needed for this function to work.")
  }
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' needed for this function to work.")
  }

  ## --- 1) Calibration slope ------------------------------------------
  n_bins <- min(10, length(unique(prob)))  # Adapt bins to data
  
  if (length(unique(prob)) < 3) {
    # Fallback for degenerate probability distributions
    slope <- 1.0
    m1 <- 1.0
  } else {
    cal_tbl <- tryCatch({
      dplyr::summarise(
        dplyr::group_by(
          dplyr::mutate(tibble::tibble(truth = truth, prob = prob),
                        bin = dplyr::ntile(prob, n_bins)),
          bin
        ),
        midpoint   = mean(prob),
        event_rate = mean(truth),
        .groups    = "drop"
      )
    }, error = function(e) {
      # Fallback: create simple binning
      data.frame(
        midpoint = c(0.25, 0.75),
        event_rate = c(mean(truth[prob < 0.5]), mean(truth[prob >= 0.5]))
      )
    })
    
    slope <- tryCatch({
      coef_result <- stats::coef(stats::lm(event_rate ~ midpoint, data = cal_tbl))
      if (length(coef_result) >= 2) coef_result[2] else 1.0
    }, error = function(e) 1.0)
    
    if (is.na(slope) || !is.finite(slope)) slope <- 1.0
    
    m1 <- 1 - abs(slope - 1)
    m1 <- max(0, min(1, m1))
  }

  ## --- 2) AUC ---------------------------------------------------------
  m2 <- tryCatch({
    if (sum(truth) == 0 || sum(truth) == length(truth)) {
      0.5  # No discrimination possible with one class
    } else {
      as.numeric(
        pROC::auc(response = truth, predictor = prob,
                  levels = c(0, 1), direction = "<", quiet = TRUE)
      )
    }
  }, error = function(e) {
    warning("AUC calculation failed, returning 0.5")
    0.5
  })
  
  if (is.na(m2) || !is.finite(m2)) m2 <- 0.5

  ## --- 3) MCC (rescaled) ----------------------------------------------
  decision <- ifelse(prob > tau, 1, 0)
  TP <- sum(decision == 1 & truth == 1)
  TN <- sum(decision == 0 & truth == 0)
  FP <- sum(decision == 1 & truth == 0)
  FN <- sum(decision == 0 & truth == 1)
  
  num <- TP * TN - FP * FN
  den <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  if (den == 0 || is.na(den) || !is.finite(den)) {
    raw_mcc <- 0  # Neutral when denominator is 0
  } else {
    raw_mcc <- num / den
  }
  
  m3 <- (raw_mcc + 1) / 2  # Rescale from [-1,1] to [0,1]
  m3 <- max(0, min(1, m3))

  ## --- 4) Brier Skill Score -------------------------------------------
  bs <- mean((prob - truth)^2)
  ref <- mean((mean(truth) - truth)^2)  # Climatological variance
  
  if (ref == 0 || !is.finite(ref)) {
    m4 <- 0.5  # Neutral when no climatological variance
  } else {
    m4 <- 1 - bs / ref
    m4 <- max(0, min(1, m4))  # Clamp to [0,1]
  }

  ## --- 5) Net Benefit -------------------------------------------------
  n <- length(truth)
  prevalence <- mean(truth)
  
  # Use the same decision variable from MCC calculation
  TP_nb <- sum(truth == 1 & decision == 1)
  FP_nb <- sum(truth == 0 & decision == 1)
  
  nb_raw <- TP_nb/n - FP_nb/n * tau/(1 - tau)
  
  # More robust rescaling bounds
  nb_min <- -tau/(1 - tau)
  nb_max <- prevalence  # Theoretical maximum when FP=0
  
  rng <- nb_max - nb_min
  if (rng <= 1e-8) {
    m5 <- 0.5  # Neutral when range is too small
  } else {
    m5 <- (nb_raw - nb_min) / rng
    m5 <- max(0, min(1, m5))
  }

  ## --- Combine components ---------------------------------------------
  components <- c(Cal = m1, AUC = m2, MCC = m3, BSS = m4, NB = m5)

  ## --- Handle weights -------------------------------------------------
  if (is.null(weights)) {
    w <- c(0.25, 0.20, 0.15, 0.20, 0.20)
  } else {
    if (!is.null(names(weights))) {
      # Reorder named weights to match component order
      expected_names <- c("Cal", "AUC", "MCC", "BSS", "NB")
      if (all(expected_names %in% names(weights))) {
        weights <- weights[expected_names]
      } else {
        stop("Named weights must include: Cal, AUC, MCC, BSS, NB")
      }
    }
    stopifnot(length(weights) == 5)
    w <- as.numeric(weights)
    stopifnot(all(is.finite(w)) && all(w >= 0))
  }
  
  if (isTRUE(normalize)) {
    s <- sum(w)
    if (is.finite(s) && s > 0) {
      w <- w / s
    } else {
      warning("Invalid weights, using equal weighting")
      w <- rep(0.2, 5)
    }
  }
  
  # Ensure weights sum to 1 (within numerical precision)
  if (abs(sum(w) - 1.0) > 1e-10) {
    w <- w / sum(w)
  }
  
  comp <- sum(w * components)
  comp <- max(0, min(1, comp))  # Final safety clamp

  ## --- Return results -------------------------------------------------
  list(
    cal = m1, 
    auc = m2, 
    mcc = m3, 
    bss = m4, 
    nb = m5,
    components = components,
    weights_used = w,
    composite = comp
  )
}
