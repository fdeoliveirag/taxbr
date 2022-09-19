#' Deflate Monetary Values by Brazilian Price Indices
#'
#' @export
deflate_vec <- function(dates, values, ref, index = "IPCA") {

  dates <- as.Date(dates)
  index <- match.arg(index)

  if (index == "IPCA") index_id <- "PRECOS12_IPCA12"

  index_tbl <- wrap_ipea(index_id, colname = "idx")

  index_tbl |>
    dplyr::mutate(idx = idx[dt == ref] / idx) |>
    dplyr::summarize(idx[match(dates, dt)] * values) |>
    dplyr::pull()
}

