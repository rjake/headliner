library(tidyverse)

animal_sleep <-
  msleep %>%
  select(
    common_name = name,
    order,
    hours_asleep = sleep_total,
    hours_awake = awake,
    brain_kg = brainwt,
    body_kg = bodywt
  ) %>%
  drop_na() %>%
  arrange(common_name) %>%
  #arrange(brainwt / bodywt) %>%
  # arrange(abs(sleep_total - awake_total)) %>%
  print()

usethis::use_data(animal_sleep, overwrite = TRUE)
