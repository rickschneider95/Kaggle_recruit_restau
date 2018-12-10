#' vday_transf
#'
#'
#' @param data
#'
#' @return returns a one hot encoded dataframe for the day of the visit
#' @export
#'
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom mltools one_hot
#' @importFrom data.table as.data.table
#' @importFrom stringr str_sub
#' @import magrittr
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

#' vtime_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
vtime_transf <- function(data){
  one_h <- data %>%
    select(visit_time) %>%
    map(str_sub, start = 0, end = 3) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' vmonth_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
vmonth_transf <- function(data){
  one_h <- data %>%
    select(visit_date) %>%
    map(str_sub, start = 6, end = 7) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' vyear_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
vyear_transf <- function(data){
  one_h <- data %>%
    select(visit_date) %>%
    map(str_sub, start = 1, end = 4) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' basic_area_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
basic_area_transf <- function(data){
  one_h <- data %>%
    select(area_name) %>%
    map(as.character) %>%
    mutate(area_name = case_when(area_name == "Osaka Prefecture Osaka None" ~ "Ōsaka-fu Prefecture Osaka None",
                                 TRUE ~ area_name)) %>% ## This is just an error in the data set
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  try <- data %>% cbind(one_h)
}

#' vdayweek_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
vdayweek_transf <- function(data){
  one_h <- data %>%
    select(day_of_the_week_visit) %>% # Already factor
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' lat_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
lat_transf <- function(data){
  one_h <- data %>%
    select(longitude) %>%
    map(round, digits = 1) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' lon_transf
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
lon_transf <- function(data){
  one_h <- data %>%
    select(latitude) %>%
    map(round, digits = 1) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' vholflg_transf
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
vholflg_transf <- function(data){
  one_h <- data %>%
    select(holiday_flg_visit) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' genre_transf
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
genre_transf <- function(data){
  one_h <- data %>%
    select(genre_name) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}
