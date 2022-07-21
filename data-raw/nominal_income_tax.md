
# code to prepare the `nominal_income_tax` dataset

-   monthly values from 1997.01 to 2022.05
-   source: Tesouro Nacional

### helpers

``` r
# clean names
lookup <- c(
  "Renda"    = "ir",
  "Física"   = "ir_pf",
  "Jurídica" = "ir_pj",
  "Retido"   = "ir_rf",
  "Trabalho" = "ir_rf_l",
  "Capital"  = "ir_rf_k",
  "Exterior" = "ir_rf_x",
  "Outros"   = "ir_rf_u"
)

names(lookup) <- sprintf(".*%s.*", names(lookup))
```

``` r
# handle Excel dates
jetdate <- function(x) as.Date(as.integer(x), origin = "1899-12-30")
```

### importing `.xlsx`

``` r
rtn_url <- "http://sisweb.tesouro.gov.br/apex/cosis/thot/link/rtn/serie-historica?conteudo=cdn"
rtn_ws <- fs::path(here::here(), "data-raw/rtn.xlsx")

download.file(rtn_url, rtn_ws, method = "libcurl", mode = "wb", extra = "-R")
```

### wrangling

``` r
nominal_income_tax <- rtn_ws |>
  readxl::read_xlsx(sheet = 4, skip  = 4, n_max = 57) |>
  dplyr::rename(rubrica = "Discriminação") |>
  dplyr::filter(grepl("1.1.03", rubrica)) |>
  dplyr::mutate(
    rubrica = rubrica |>
      stringr::str_replace_all(lookup)
  ) |>
  tidyr::pivot_longer(
    -rubrica,
    names_to        = "dt",
    names_transform = jetdate
  ) |>
  tidyr::pivot_wider(names_from = rubrica)
```

### exporting `.rda`

``` r
usethis::use_data(nominal_income_tax, overwrite = TRUE)
```
