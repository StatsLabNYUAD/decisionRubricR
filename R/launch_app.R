#' Launch the decisionRubricR Shiny demo
#'
#' Starts the example app bundled with the package.
#'
#' @param ... Additional arguments passed to [shiny::runApp()]
#'   (e.g., `display.mode = "showcase"`).
#' @return Invisibly, the result of [shiny::runApp()].
#' @examples
#' \dontrun{
#'   launch_decisionRubricR()
#' }
#' @seealso [shiny::runApp()]
#' @export
#' @importFrom shiny runApp
launch_decisionRubricR <- function(...) {
  app_dir <- system.file("app", "decisionRubricR-shiny", package = "decisionRubricR")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Shiny app not found in this build of decisionRubricR.", call. = FALSE)
  }
  runApp(app_dir, ...)  # uses @importFrom shiny runApp
}

