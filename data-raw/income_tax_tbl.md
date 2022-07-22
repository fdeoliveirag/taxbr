
# code to prepare the `income_tax_tbl` dataset

<br>

*Brazilian federal income tax (Imposto de Renda), gross revenues*

|                 |                                                        |
|:----------------|:-------------------------------------------------------|
| **date range**  | from 1986.01 to 2022.05                                |
| **periodicity** | monthly values                                         |
| **unit**        | c.m.u. (million)                                       |
| **source**      | Secretaria da Receita Federal - Ministério da Economia |

<br>

### i. series id

``` r
series_id <- c(
  "IR",      # Total
  "IRPF",    # Pessoa Física
  "IRPJ",    # Pessoa Jurídica
  "IRRF",    # Retido na Fonte - Total
  "IRRFRT",  # Retido na Fonte - Rendimentos do Trabalho
  "IRRFRC",  # Retido na Fonte - Rendimentos do Capital
  "IRRFRE",  # Retido na Fonte - Remessas ao Exterior
  "IRRFOUT"  # Retido na Fonte - Outros Rendimentos
)
```

### ii. load and join series

``` r
income_tax_tbl <- series_id |>
  purrr::map(~ taxbr::wrap_ipea(sprintf("SRF12_%s12", .x), colname = .x)) |>
  purrr::reduce(~ dplyr::left_join(.x, .y, by = "dt"))

tail(income_tax_tbl)
```

    ## # A tibble: 6 × 9
    ##   dt             IR   IRPF   IRPJ   IRRF IRRFRT IRRFRC IRRFRE IRRFOUT
    ##   <date>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
    ## 1 2021-12-01 53575.  4470. 16951. 32153. 13272.  9804.  7816.   1261.
    ## 2 2022-01-01 84823.  2707. 52252. 29864. 17563.  6485.  4392.   1424.
    ## 3 2022-02-01 41350.  2731. 17313. 21305. 11900.  5014.  3249.   1142.
    ## 4 2022-03-01 51108.  2813. 22829. 25466. 16059.  4667.  3650.   1091.
    ## 5 2022-04-01 61801.  3607. 32430. 25765. 15043.  5963.  3423.   1335.
    ## 6 2022-05-01 54442. 13627. 16249. 24566. 14401.  5807.  3011.   1347.

### iii. share series

``` r
usethis::use_data(income_tax_tbl, overwrite = TRUE)
```

<br>
