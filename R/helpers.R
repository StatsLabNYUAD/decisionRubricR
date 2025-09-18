#' Rescaled Matthews correlation coefficient
#' Uses double precision to avoid integer overflow.
#' @noRd
mcc_rescaled <- function(truth, decision) {
  TP <- sum(truth == 1 & decision == 1, na.rm = TRUE)
  TN <- sum(truth == 0 & decision == 0, na.rm = TRUE)
  FP <- sum(truth == 0 & decision == 1, na.rm = TRUE)
  FN <- sum(truth == 1 & decision == 0, na.rm = TRUE)

  TP <- as.numeric(TP); TN <- as.numeric(TN); FP <- as.numeric(FP); FN <- as.numeric(FN)
  denom <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

  mcc <- if (is.na(denom) || denom == 0) 0 else (TP * TN - FP * FN) / denom
  (mcc + 1) / 2
}
