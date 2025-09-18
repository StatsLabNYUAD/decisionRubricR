#' Platt (logistic) recalibration
#'
#' Fits a logistic calibration model: glm(truth ~ logit(prob)).
#' Returns calibrated probabilities in [0, 1].
#'
#' @param truth Integer/logical vector of outcomes (0/1).
#' @param prob  Numeric vector of predicted probabilities in [0,1].
#' @return Numeric vector of recalibrated probabilities in [0,1].
#' @examples
#' set.seed(1)
#' truth <- rbinom(100, 1, 0.3)
#' prob  <- runif(100)
#' p2 <- platt_recal(truth, prob)
#' @export
#' @importFrom stats glm binomial predict qlogis
platt_recal <- function(truth, prob) {
  stopifnot(length(truth) == length(prob))
  # guard against 0/1 probabilities (avoid +/-Inf on the logit scale)
  eps  <- 1e-6
  prob <- pmin(pmax(prob, eps), 1 - eps)

  z   <- stats::qlogis(prob)
  fit <- stats::glm(truth ~ z, family = stats::binomial(), start = c(0, 1))
  as.numeric(stats::predict(fit, type = "response"))
}

#' Score after Platt recalibration
#'
#' Applies \code{platt_recal()} to probabilities, then calls \code{score_rubric()}.
#' @inheritParams score_rubric
#' @return A named list with the five component scores and the composite (same shape as \code{score_rubric()}).
#' @export
score_student_rec <- function(truth, prob, ...) {
  prob_star <- platt_recal(truth, prob)
  score_rubric(truth, prob_star, ...)
}
