## code to prepare `IR` dataset goes here

# ----
`%>%` <- magrittr::`%>%`


# ----
rtn_url <- "http://sisweb.tesouro.gov.br/apex/cosis/thot/link/rtn/serie-historica?conteudo=cdn"

rtn_ws <- "data-raw/rtn.xlsx"

download.file(rtn_url, rtn_ws, method = "libcurl", mode = "wb", extra = "-R")


# ----
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


# ----
jetdate <- function(x) as.Date(as.integer(x), origin = "1899-12-30")


# ----
IR <- rtn_ws %>%
  readxl::read_xlsx(sheet = 4, skip = 4, n_max = 57) %>%
  dplyr::rename(rubrica = "Discriminação") %>%
  dplyr::filter(grepl("1.1.03", rubrica)) %>%
  dplyr::mutate(
    rubrica = rubrica %>%
      stringr::str_replace_all(lookup)
  ) %>%
  tidyr::pivot_longer(
    -rubrica,
    names_to        = "dt",
    names_transform = jetdate
  ) %>%
  tidyr::pivot_wider(names_from = rubrica)


# ----
usethis::use_data(IR, overwrite = TRUE)
