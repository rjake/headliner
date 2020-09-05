library(tidyverse)
library(nycflights13)
library(lubridate)

flights_jfk <-
  flights %>%
  filter(
    origin == "JFK",
    carrier %in% c("B6", "DL", "AA"),
    dest %in% c("LAS", "LAX", "SFO"),
    between(hour, 9, 21)
  ) %>%
  mutate(
    year = year - (row_number() %% 2),
    date = make_date(year, month, day)
  ) %>%
  select(
    year, date, hour, carrier, dest, tailnum, distance, air_time,
    contains("delay")
  )

usethis::use_data(flights_jfk, overwrite = TRUE)
