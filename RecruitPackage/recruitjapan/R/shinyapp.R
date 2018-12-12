#' shinyapp_jap
#'
#' @return shinyapp of our project
#' @export
#'
#' @import shiny
#' @import shinyTime
#' @import nlme
#' @examples
#' dir <- system.file("shiny","template_app1.R", package = "recruitjap")
#' # shinyapp(dir) don't run or it just won't stop the connexion!
#'
shinyapp <- function(){
  dir <- system.file("shiny","template_app1.R", package = "recruitjap")
  if (dir == "") {
    stop("Could not find example directory. Try re-installing mypackage.", call. = FALSE)
  }

  shiny::runApp(dir, display.mode = "normal")
}
