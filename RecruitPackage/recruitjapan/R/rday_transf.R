#' rday_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @import stringr
#' @examples
rday_transf <- function(data){
  one_h <- data %>%
    select(reserve_date) %>%
    map(str_sub, start = 9, end = 10) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}
