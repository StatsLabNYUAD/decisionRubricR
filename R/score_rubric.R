#' Score rubric metrics and composite
#'
#' @param truth integer/logical 0/1
#' @param prob  numeric in [0,1]
#' @param tau   decision threshold for MCC / net benefit
#' @param weights numeric length-5 (named or unnamed) giving weights
#'   for Cal, AUC, MCC, BSS, NB. If NULL, uses defaults
#'   c(.25,.20,.15,.20,.20).
#' @param normalize logical; if TRUE, renormalize weights to sum to 1
#' @return list with sub-metrics, `components`, `weights_used`, and `composite`.
#' @export
score_rubric <- function(truth, prob, tau = 0.30,
                         weights = NULL, normalize = TRUE) {

  # ... your existing code that computes:
  # m1 <- cal; m2 <- auc; m3 <- mcc; m4 <- bss; m5 <- nb
  # Keep the variable names you currently use.

  # Build a named components vector (canonical names)
  components <- c(Cal = m1, AUC = m2, MCC = m3, BSS = m4, NB = m5)

  # Resolve weights (default or user-supplied)
  if (is.null(weights)) {
    w <- c(Cal = .25, AUC = .20, MCC = .15, BSS = .20, NB = .20)
  } else {
    w <- if (is.null(names(weights))) {
      setNames(as.numeric(weights), names(components))
    } else {
      # allow any order, require the 5 names
      stopifnot(all(names(components) %in% names(weights)))
      as.numeric(weights[names(components)])
    }
    w <- pmax(0, w)
    if (normalize) {
      s <- sum(w); if (s > 0) w <- w / s else w[] <- 1/5
    }
    names(w) <- names(components)
  }

  comp <- as.numeric(sum(w * components))

  # Return structure compatible with old callers + richer fields
  out <- list(
    cal = m1, auc = m2, mcc = m3, bss = m4, nb = m5,
    components   = components,
    weights_used = w,
    composite    = comp
  )
  # For very old code that expects $comp:
  out$comp <- out$composite
  out
}
