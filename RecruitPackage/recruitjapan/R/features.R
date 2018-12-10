#' area_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
area_transf <- function(data){
  one_h <- data %>%
    select(area_name) %>%
    mutate(area_name = as.character(area_name)) %>%
    mutate(area_name = case_when(area_name == "Osaka Prefecture Osaka None" ~ "Ōsaka-fu Prefecture Osaka None",
                                 TRUE ~ area_name)) %>% # error in the data
    map(str_sub, start = 0, end = 8) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' bme_vmonth_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
bme_vmonth_transf <- function(data){
  one_h <- data %>%
    select(visit_date) %>%
    mutate(visit_date = as.character(str_sub(visit_date, start = 9, end = 10))) %>%
    mutate(visit_date = as.factor(case_when(visit_date >= 0 & visit_date <= 12 ~ "beginning",
                                            visit_date >= 13 & visit_date <= 22 ~ "middle",
                                            TRUE ~ "end"))) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' genre_gr_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
genre_gr_transf <- function(data){
  one_h <- data %>%
    select(genre_name) %>%
    mutate(genre_name = case_when(genre_name == "Izakaya" |
                                    genre_name == "Okonomiyaki/Monja/Teppanyaki" |
                                    genre_name == "Japanese food in general" |
                                    genre_name == "Japanese cuisine/Kaiseki" ~ "Japanese food",
                                  genre_name == "Italian/French" |
                                    genre_name == "Italian" ~ "Western food",
                                  genre_name == "Creative cuisine" |
                                    genre_name == "Creative Japanese food" ~ "Creation",
                                  genre_name == "Karaoke" |
                                    genre_name == "Party" |
                                    genre_name == "Karaoke/Party" ~ "Amusement bar",
                                  genre_name == "Asian" ~ "Yakiniku/Korean food",
                                  genre_name == "Cafe/Sweets" ~ "Cafe",
                                  genre_name == "Steak/Hamburger/Curry" ~ "Grilled meat",
                                  TRUE ~ as.character(genre_name))) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' barrest_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
barrest_transf <- function(data){
  one_h <- data %>%
    select(genre_name) %>%
    mutate(genre_name = case_when(genre_name == "Asian" |
                                    genre_name == "Okonomiyaki/Monja/Teppanyaki" |
                                    genre_name == "Japanese food" |
                                    genre_name == "Western food" |
                                    genre_name == "Italian/French" |
                                    genre_name == "International cuisine" |
                                    genre_name == "Yakiniku/Korean food" |
                                    genre_name == "Izakaya" |
                                    genre_name == "Creative cuisine" ~ "Restaurant",
                                  genre_name == "Dining bar" |
                                    genre_name == "Bar/Cocktail" |
                                    genre_name == "Karaoke/Party" |
                                    genre_name == "Cafe/Sweets" ~ "Bar",
                                  TRUE ~ "Else")) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' goldw_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @importFrom lubridate ymd
#' @examples
goldw_transf <- function(data){
  one_h <- data %>%
    mutate(visit_date = ymd(visit_date)) %>%
    mutate(goldenweek = case_when(visit_date > ymd("20160428") & visit_date < ymd("20160507") ~ 1,
                                  visit_date > ymd("20170428") & visit_date < ymd("20170507") ~ 1,
                                  TRUE ~ 0)) %>%
    select(goldenweek) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' goldd_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
goldd_transf <- function(data){
  one_h <- data %>%
    mutate(visit_date = ymd(visit_date)) %>%
    mutate(goldendays = case_when(visit_date == ymd("20160429") |
                                    visit_date == ymd("20160503") |
                                    visit_date == ymd("20160504") |
                                    visit_date == ymd("20160505") |
                                    visit_date == ymd("20170429") |
                                    visit_date == ymd("20170503") |
                                    visit_date == ymd("20170504") |
                                    visit_date == ymd("20170505") ~ 1,
                                  TRUE ~ 0)) %>%
    select(goldendays) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' wknd_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
wknd_transf <- function(data){
  one_h <- data %>%
    mutate(wknd = case_when(day_of_the_week_visit == "Friday" |
                              day_of_the_week_visit == "Saturday" |
                              day_of_the_week_visit == "Sunday" ~ "weekend",
                            TRUE ~ "weekday")) %>%
    select(wknd) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' wealth_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples
wealth_transf <- function(data){
  one_h <- data %>%
    mutate(area_name = as.character(area_name)) %>%
    mutate(area_name = case_when(area_name == "Osaka Prefecture Osaka None" ~ "Ōsaka-fu Prefecture Osaka None",
                                 TRUE ~ area_name)) %>% # error in the data
    mutate(area_name = str_sub(area_name, start = 0, end = 8)) %>%
    mutate(wealth = case_when(area_name == "Tōkyō-to" ~ "High",
                              area_name == "Shizuoka" | area_name == "Hyōgo-ke" | area_name == "Ōsaka-fu" ~ "Middle/High",
                              area_name == "Niigata-" | area_name == "Miyagi-k" | area_name == "Hokkaidō" ~ "Middle",
                              area_name == "Fukuoka-" ~ "Middle/Low",
                              TRUE ~ "Low")) %>%
    select(wealth) %>%
    map(as.factor) %>%
    map(as.data.table) %>%
    map(one_hot)
  data <- data %>% cbind(one_h)
}

#' nth_day_trasnf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @importFrom dplyr left_join
#' @examples
nth_day_trasnf <- function(data){
  days <- c(1:365,1:152)
  dates <- date_info %>%
    cbind(days) %>%
    select(visit_date,days)
  data <- data %>%
    left_join(dates, by = "visit_date")
}

#' lag_transf
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @importFrom RcppRoll roll_meanr
#' @importFrom dplyr lag
#' @examples
lag_transf <- function(data){
  data <- data %>%
    group_by(id) %>%
    mutate(lag_1 = lag(visitors, 1),
           lag_2 = lag(visitors, 2),
           lag_3 = lag(visitors, 3),
           lag_4 = lag(visitors, 4),
           lag_5 = lag(visitors, 5),
           lag_6 = lag(visitors, 6),
           lag_7 = lag(visitors, 7),
           lag_8 = lag(visitors, 8),
           lag_9 = lag(visitors, 9),
           lag_10 = lag(visitors, 10),
           lag_11 = lag(visitors, 11),
           lag_12 = lag(visitors, 12),
           lag_13 = lag(visitors, 13),
           lag_14 = lag(visitors, 14),
           lag_15 = lag(visitors, 15),
           lag_16 = lag(visitors, 16),
           lag_17 = lag(visitors, 17),
           lag_18 = lag(visitors, 18),
           lag_19 = lag(visitors, 19),
           lag_20 = lag(visitors, 20),
           lag_21 = lag(visitors, 21),
           lag_22 = lag(visitors, 22),
           lag_23 = lag(visitors, 23),
           lag_24 = lag(visitors, 24),
           lag_25 = lag(visitors, 25),
           lag_26 = lag(visitors, 26),
           lag_27 = lag(visitors, 27),
           lag_28 = lag(visitors, 28),
           lag_30 = lag(visitors, 30),
           lag_33 = lag(visitors, 33),
           lag_36 = lag(visitors, 36),
           lag_39 = lag(visitors, 39),
           lag_40 = lag(visitors, 40),
           lag_50 = lag(visitors, 50),
           lag_60 = lag(visitors, 60),
           lag_70 = lag(visitors, 70),
           lag_80 = lag(visitors, 80),
           lag_90 = lag(visitors, 90),
           lag_100 = lag(visitors, 100),
           lag_120 = lag(visitors, 120),
           lag_150 = lag(visitors, 150),
           lag_180 = lag(visitors, 180),
           avg_7 = lag(roll_meanr(visitors, 7), 1),
           avg_3 = lag(roll_meanr(visitors, 3), 1),
           avg_7 = lag(roll_meanr(visitors, 7), 1),
           avg_14 = lag(roll_meanr(visitors, 14), 1),
           avg_21 = lag(roll_meanr(visitors, 21), 1),
           avg_28 = lag(roll_meanr(visitors, 28), 1),
           avg_50 = lag(roll_meanr(visitors, 50), 1),
           avg_100 = lag(roll_meanr(visitors, 100), 1),
           avg_150 = lag(roll_meanr(visitors, 150), 1),
           avg_200 = lag(roll_meanr(visitors, 200), 1),
           avg_250 = lag(roll_meanr(visitors, 250), 1),
           avg_300 = lag(roll_meanr(visitors, 300), 1)
    )
}
