#' vday_transf
#'
#' @param data dataframe with categorical variable "visit_date" indicating date of visit
#'
#' @return data
#' @export
#'
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom mltools one_hot
#' @importFrom data.table as.data.table
#' @importFrom stringr str_sub
#' @import magrittr
#'
#' @examples
#' ex <- data.frame(visit_date = c("2016-01-13","2016-04-02","2017-02-11"))
#' vday_transf(ex) %>% head()
#'
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
#' @param data dataframe with categorical variable "visit_time" indicating time of visit
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(visit_time = c("13:00:00","02:00:00","15:00:00"))
#' vtime_transf(ex) %>% head()
#'
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
#' @param data dataframe with categorical variable "visit_date" indicating date of visit
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(visit_date = c("2016-01-13","2016-04-02","2017-02-11"))
#' vmonth_transf(ex) %>% head()
#'
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
#' @param data dataframe with categorical variable "visit_date" indicating date of visit
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(visit_date = c("2016-01-13","2016-04-02","2017-02-11"))
#' vyear_transf(ex) %>% head()
#'
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
#' @param data dataframe with categorical variable "area_name" indicating the name of the area in which the restaurant is located (prefecture)
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(area_name = c("Fukuoka-ken Itoshima-shi Maebarunishi"))
#' basic_area_transf(ex) %>% head()
#'
basic_area_transf <- function(data){
  one_h <- data %>%
    select(area_name) %>%
    mutate(area_name = as.character(area_name)) %>%
    mutate(area_name = case_when(area_name == "Osaka Prefecture Osaka None" ~ "\u008Csaka-fu Prefecture Osaka None",
                                 TRUE ~ area_name)) %>% ## This is just an error in the data set
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  try <- data %>% cbind(one_h)
}

#' vdayweek_transf
#'
#' @param data dataframe with categorical variable "day_of_the_week_visit" indicating the day on which there was a visit
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(day_of_the_week_visit = c("Friday","Tuesday","Friday"))
#' vdayweek_transf(ex) %>% head()
#'
vdayweek_transf <- function(data){
  one_h <- data %>%
    select(day_of_the_week_visit) %>% # Already factor
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' lat_transf
#'
#' @param data dataframe with quantitative variable "latitude" indicating lat coordinates of a given restaurant
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(longitude = c(139.7719,139.0363,139.6982))
#' lon_transf(ex) %>% head()
#'
lon_transf <- function(data){
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
#' @param data dataframe with quantitative variable "longitude" indicating lon coordinates of a given restaurant
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(latitude = c(34.66474,34.75695,35.67492))
#' lat_transf(ex) %>% head()
#'
lat_transf <- function(data){
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
#' @param data dataframe with binary variable "holiday_flg_visit" indicating if date of visit was on a holiday in japan
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(holiday_flg_visit = c("1","0","1","0","0"))
#' vholflg_transf(ex) %>% head()
#'
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
#' @param data dataframe with categorical variable "genre_name" indicating style of restaurant which was booked/visited
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(genre_name = c("Yakiniku/Korean food","Cafe/Sweets","Creative cuisine"))
#' genre_transf(ex) %>% head()
#'
genre_transf <- function(data){
  one_h <- data %>%
    select(genre_name) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}
