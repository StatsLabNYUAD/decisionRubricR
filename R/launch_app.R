
#' Launch the decisionRubricR Shiny demo
#' @export
launch_decisionRubricR <- function(...) {
  app_dir <- system.file("app", "decisionRubricR-shiny", package = "decisionRubricR")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Shiny app not found in this build of decisionRubricR.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}

