#' Platt scaling recalibration
#'
#' Fits a logistic calibration model and returns recalibrated probabilities.
#' Standard Platt scaling regresses the binary outcome on the logit of the
#' original scores and maps back to probabilities.
#'
#' @param truth Integer/logical vector coded `0/1`.
#' @param prob  Numeric vector in `[0, 1]` of predicted probabilities.
#' @return Numeric vector of recalibrated probabilities in `[0, 1]`.
#' @examples
#' \dontrun{
#' set.seed(1); truth <- rbinom(200, 1, 0.3); prob <- runif(200)
#' prob_star <- platt_recal(truth, prob)
#' }
#' @export
platt_recal <- function(truth, prob) {
  stopifnot(length(truth) == length(prob),
            all(truth %in% c(0, 1)),
            is.numeric(prob), all(is.finite(prob)),
            all(prob >= 0 & prob <= 1))

  eps  <- 1e-6
  z    <- stats::qlogis(pmin(pmax(prob, eps), 1 - eps))

  fit  <- stats::glm(truth ~ z, family = stats::binomial())
  pcal <- stats::predict(fit, type = "response")
  pmin(pmax(pcal, 0), 1)
}
#' Score a student after recalibration
#'
#' Applies `platt_recal()` to `prob`, then calls [score_rubric()].
#'
#' @param truth Integer/logical vector coded `0/1`.
#' @param prob  Numeric vector in `[0, 1]`.
#' @param ...   Additional arguments passed to [score_rubric()]
#'   (e.g., `tau`, `weights`, `normalize`).
#' @return The list returned by [score_rubric()] (components + composite).
#' @export
score_student_rec <- function(truth, prob, ...) {
  prob_star <- platt_recal(truth, prob)
  score_rubric(truth = truth, prob = prob_star, ...)
}
