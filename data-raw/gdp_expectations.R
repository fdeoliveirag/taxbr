library(tidyverse)
library(rbcb)


gdp_expectations <- get_market_expectations(
  type = "annual", indic = "PIB Total"
) %>%
  janitor::clean_names() %>%
  transmute(
    dt  = rollback(data, roll_to_first = TRUE),
    ref = as.numeric(data_referencia),
    avg = media,
    med = mediana,
    std = desvio_padrao,
    min = minimo,
    max = maximo,
    n   = numero_respondentes
  ) %>%
  mutate(year = year(dt), month = month(dt)) %>%
  filter(
    ref %in% 2017:2021, year %in% 2017:2021, month %in% c(6, 12),
    (ref - year + month) %in% c(0 + 6, 1 + 6, 1 + 12)
  ) %>%
  group_by(ref, year, month) %>%
  slice(n()) %>%
  ungroup()

usethis::use_data(gdp_expectations, overwrite = TRUE)

