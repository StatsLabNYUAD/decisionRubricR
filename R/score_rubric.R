#' Score rubric metrics and composite
#'
#' Computes five components (calibration slope proximity to 1, AUC,
#' rescaled MCC, Brier Skill Score, and decision-curve Net Benefit)
#' and a weighted composite.
#'
#' @param truth integer/logical vector (0/1)
#' @param prob  numeric in [0,1]
#' @param tau   decision threshold (default 0.30)
#' @param weights numeric length-5 (named or unnamed) giving weights for
#'               Cal, AUC, MCC, BSS, NB. If NULL, uses defaults
#'               c(.25, .20, .15, .20, .20).
#' @param normalize logical; if TRUE, renormalize weights to sum to 1
#' @return list with sub-metrics, components, weights_used, and composite.
#' @export
score_rubric <- function(truth, prob, tau = 0.30,
                         weights = NULL, normalize = TRUE) {

  stopifnot(length(truth) == length(prob))
  stopifnot(all(truth %in% c(0, 1)))
  stopifnot(all(is.finite(prob)) && all(prob >= 0 & prob <= 1))

  ## --- 1) Calibration slope (proximity to 1) -------------------------
  n_bins <- 10
  cal_tbl <- dplyr::summarise(
    dplyr::group_by(
      dplyr::mutate(tibble::tibble(truth = truth, prob = prob),
                    bin = dplyr::ntile(prob, n_bins)),
      bin
    ),
    midpoint  = mean(prob),
    event_rate = mean(truth),
    .groups = "drop"
  )
  slope <- stats::coef(stats::lm(event_rate ~ midpoint, data = cal_tbl))[2]
  m1 <- unname(1 - abs(slope - 1))
  m1 <- max(0, min(1, m1))  # clamp to [0,1]

  ## --- 2) AUC ---------------------------------------------------------
  m2 <- as.numeric(
    pROC::auc(response = truth, predictor = prob,
              levels = c(0, 1), direction = "<", quiet = TRUE)
  )

  ## --- 3) MCC on decisions at tau (rescaled to [0,1]) -----------------
  decision <- ifelse(prob > tau, 1, 0)
  m3 <- mcc_rescaled(truth, decision)

  ## --- 4) Brier Skill Score ------------------------------------------
  bs  <- mean((prob - truth)^2)
  ref <- mean((mean(truth) - truth)^2)
  m4  <- if (ref == 0) 1 else 1 - bs / ref

  ## --- 5) Net Benefit at tau (rescaled to [0,1]) ----------------------
  TP <- sum(truth == 1 & decision == 1)
  FP <- sum(truth == 0 & decision == 1)
  n  <- length(truth)
  nb_raw <- TP/n - FP/n * tau/(1 - tau)
  nb_min <- -tau/(1 - tau); nb_max <- 1
  m5 <- (nb_raw - nb_min) / (nb_max - nb_min)
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
  comp <- unname(sum(w * components))

  list(
    cal = m1, auc = m2, mcc = m3, bss = m4, nb = m5,
    components  = components,
    weights_used = w,
    composite   = comp,   # new name
    comp        = comp    # backward-compatible alias
  )
}
