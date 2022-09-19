#' From Ipeadata Series to Tidy Dataframes
#'
#' @export
wrap_ipea <- function(id, colname = "series") {

  colname <- rlang::sym(colname)

  api_url <- "http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='%s')"
  api_req <- httr2::request(sprintf(api_url, id))

  # TODO: handle API timeout errors
  api_resp <- httr2::req_perform(api_req)

  api_resp |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::chuck("value") |>
    tibble::as_tibble() |>
    dplyr::select(dt = VALDATA, !!colname := VALVALOR) |>
    dplyr::mutate(dt = as.Date(dt))
}

