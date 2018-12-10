#' rday_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
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

#' rtime_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
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
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
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
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
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
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
rdayweek_transf <- function(data){
  one_h <- data %>%
    select(day_of_the_week_reserve) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' rholflg_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
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
#' @param data
#'
#' @return data
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @examples
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
#' @param data
#'
#' @return data
#' @export
#'
#' @importFrom zoo as.Date
#' @examples
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
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
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
