#' Deflate Monetary Values By IPCA
#'
#' @export
deflate <- function(values, dates, ref_date) {

  code <- "PRECOS12_IPCA12"

  code |>
    ipeadata() |>
    dplyr::mutate(idx = series[dt == ref_date] / series) |>
    dplyr::summarize(idx[match(dates, dt)] * values) |>
    dplyr::pull()
}
