test_that("score_rubric returns all components", {
  set.seed(1)
  truth <- rbinom(100, 1, 0.4)
  prob  <- runif(100)
  res   <- score_rubric(truth, prob)
  expect_true(all(c("cal","auc","mcc","bss","nb","comp") %in% names(res)))
  expect_true(all(vapply(res, is.numeric, TRUE)))
})

