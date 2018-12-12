#' shinyapp_jap
#'
#' @return shinyapp of our project
#' @export
#'
#' @import shiny
#' @examples
#' shinyapp
#'
shinyapp <- function() {
  appDir <- system.file("shiny", "template_app1.R", package = "recruitjap")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing mypackage.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
