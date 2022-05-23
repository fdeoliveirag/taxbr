#' Deflate monetary values by IPCA
#'
#' @export
deflate <- function(values, dates, ref_date) {

  ipeadata <- "http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='%s')"

  ipca <- httr2::request(sprintf(ipeadata, "PRECOS12_IPCA12"))
  ipca <- httr2::req_perform(ipca)

  ipca <- httr2::resp_body_json(ipca, simplifyVector = TRUE)
  ipca <- ipca[["value"]]

  d <- as.Date(ipca$VALDATA)
  v <- ipca$VALVALOR

  i <- v[d == ref_date] / v

  i[match(dates, d)] * values
}
