#' rday_transf
#'
#' @param data dataframe with categorical variable "reserve_date" indicating date of reservation
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(reserve_date = c("2016-01-13","2016-04-02","2017-02-11"))
#' rday_transf(ex) %>% head()
#'
rday_transf <- function(data){
  one_h <- data %>%
    select(reserve_date) %>%
    map(str_sub, start = 9, end = 10) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' rtime_transf
#'
#' @param data dataframe with categorical variable "reserve_time" indicating time of reservation
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(reserve_time = c("13:00:00","02:00:00","15:00:00"))
#' rtime_transf(ex) %>% head()
#'
rtime_transf <- function(data){
  one_h <- data %>%
    select(reserve_time) %>%
    map(str_sub, start = 0, end = 3) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' rmonth_transf
#'
#' @param data dataframe with categorical variable "reserve_date" indicating date of reservation
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(reserve_date = c("2016-01-13","2016-04-02","2017-02-11"))
#' rmonth_transf(ex) %>% head()
#'
rmonth_transf <- function(data){
  one_h <- data %>%
    select(reserve_date) %>%
    map(str_sub, start = 6, end = 7) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' ryear_transf
#'
#' @param data dataframe with categorical variable "reserve_date" indicating date of reservation
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(reserve_date = c("2016-01-13","2016-04-02","2017-02-11"))
#' ryear_transf(ex) %>% head()
#'
ryear_transf <- function(data){
  one_h <- data %>%
    select(reserve_date) %>%
    map(str_sub, start = 1, end = 4) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' rdayweek_transf
#'
#' @param data dataframe with categorical variable "day_of_the_week_reserve" indicating the day on which reservation was made
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(day_of_the_week_reserve = c("Friday","Tuesday","Friday"))
#' rdayweek_transf(ex) %>% head()
#'
rdayweek_transf <- function(data){
  one_h <- data %>%
    select(day_of_the_week_reserve) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' rholflg_transf
#'
#' @param data dataframe with binary variable "holiday_flg_reserve" indicating if corresponding date is a holiday in japan
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(holiday_flg_reserve = c("1","0","1","0","0"))
#' rholflg_transf(ex) %>% head()
#'
rholflg_transf <- function(data){
  one_h <- data %>%
    select(holiday_flg_reserve) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' dn_transf
#'
#' @param data dataframe with categorical variable "visit_time" indicating time of visit in a given restaurant
#'
#' @return data
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'
#' @examples
#' ex <- data.frame(visit_time = c("13:00:00","02:00:00","15:00:00"))
#' dn_transf(ex) %>% head()
#'
dn_transf <- function(data){
  one_h <- data %>%
    select(visit_time) %>%
    mutate(visit_time = as.integer(str_sub(as.character(visit_time),
                                           start = 0,
                                           end = 3))) %>%
    mutate(visit_time = case_when(visit_time >= 4 & visit_time <= 16 ~ "noon",          # Lunch
                                  TRUE ~ "night")) %>%    # Dinner
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' latency_transf
#'
#' @param data dataframe with categorical variable "reserve_date" indicating date on which reservation was made
#'
#' @return data
#' @export
#'
#' @importFrom zoo as.Date
#' @examples
#' ex <- data.frame(reserve_date = c("2016-01-13"), visit_date = c("2017-03-01"))
#' latency_transf(ex) %>% head()
#'
latency_transf <- function(data){
  one_h <- data
  one_h <- one_h %>%
    mutate(latency_days = as.integer(ifelse(is.na(reserve_date), 0, as.numeric(as.Date(visit_date) - as.Date(reserve_date))))) %>%
    select(latency_days) %>%
    mutate(latency_days = as.factor(case_when(latency_days >= 0 & latency_days <= 15 ~ "casual",
                                              latency_days > 15 & latency_days <= 45 ~ "fancy",
                                              TRUE ~ "very fancy")))
  one_h <- one_hot(as.data.table(one_h))
  data <- data %>% cbind(one_h)
}

#' bme_rmonth_transf
#'
#' @param data dataframe with categorical variable "reserve_date" indicating date on which reservation was made
#'
#' @return data
#' @export
#'
#' @examples
#' ex <- data.frame(reserve_date = c("2016-01-13","2016-04-02","2017-02-11"))
#' bme_rmonth_transf(ex) %>% head()
#'
bme_rmonth_transf <- function(data){
  one_h <- data
  one_h$reserve_date <- as.integer(str_sub(as.character(one_h$reserve_date), start = 9, end = 10))
  one_h <- one_h %>%
    select(reserve_date) %>%
    mutate(reserve_date = as.factor(case_when(reserve_date >= 0 & reserve_date <= 12 ~ "beginning",
                                              reserve_date >= 13 & reserve_date <= 22 ~ "middle",
                                              TRUE ~ "end")))
  one_h <- one_hot(as.data.table(one_h))
  data <- data %>% cbind(one_h)
}
