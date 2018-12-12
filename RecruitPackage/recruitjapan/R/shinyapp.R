#' shinyapp_jap
#'
#' @return shinyapp of our project
#' @export
#'
#' @import shiny
#' @import shinyTime
#' @examples
#' shinyapp
#'
shinyapp <- function(dir) {
  if (dir == "") {
    stop("Could not find example directory. Try re-installing mypackage.", call. = FALSE)
  }

  shiny::runApp(dir, display.mode = "normal")
}
