library(pixarfilms)
library(tidyverse)
library(lubridate)

pixar_films <-
  pixarfilms::pixar_films |>
  transmute(
    order = as.integer(number),
    film,
    release_date,
    year = year(release_date),
    rating = film_rating,
    run_time
  ) |>
  left_join(
    pixarfilms::public_response |>
      select(
        film,
        rotten_tomatoes,
        metacritic)) |>
  left_join(
    pixarfilms::box_office |> #slice(1) |> t()
      transmute(
        film,
        #budget_m = round(budget / 1e6, 1),
        bo_domestic = round(box_office_us_canada / 1e6, 1),
        bo_intl = round(box_office_other  / 1e6, 1)
      )
  ) |>
  drop_na() |>
  print(n = 5)

usethis::use_data(pixar_films, overwrite = TRUE)

