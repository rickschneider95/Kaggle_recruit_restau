usethis::use_build_ignore("devtools_history.R")
usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("mltools")
usethis::use_package("data.table")

usethis::use_package("stringr")

devtools::document()

usethis::use_package("magrittr")

usethis::use_pipe()

remotes::install_github("Thinkr-open/attachment")
attachment::att_to_description()

devtools::check()

usethis::use_package("conflicted")
attachment::att_to_description()

usethis::use_package("shiny")

install.packages("shinyTime")
attachment::att_to_description()
