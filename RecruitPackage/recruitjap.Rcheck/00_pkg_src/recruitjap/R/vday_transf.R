#' vday_transf
#'
#'
#' @param data
#'
#' @return returns a one hot encoded dataframe for the day of the visit
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import mltools
#' @import data.table
#' @examples
#' vday_transf(air_data)
vday_transf <- function(data){
  one_h <- data %>%
    select(visit_date) %>%
    map(str_sub, start = 9, end = 10) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}
