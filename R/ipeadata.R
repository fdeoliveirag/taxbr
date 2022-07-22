#' From Ipeadata Series To Tidy Dataframes
#'
#' @export
ipeadata <- function(code, colname = series) {

  api <- "http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='%s')"

  series_req <- httr2::request(sprintf(api, code))
  series_resp <- httr2::req_perform(series_req)

  series_lst <- httr2::resp_body_json(series_resp, simplifyVector = TRUE)
  series_df <- series_lst$value

  series_df |>
    tibble::as_tibble() |>
    dplyr::select(dt = VALDATA, {{colname}} := VALVALOR) |>
    dplyr::mutate(dt = as.Date(dt))
}
