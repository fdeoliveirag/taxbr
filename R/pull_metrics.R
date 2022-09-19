#' Collect SMAPE & MASE
#'
#' @export
pull_metrics <- function(results, response, horizon, model_id) {

  response <- rlang::enquo(response)
  h <- sprintf("h%02d", horizon)

  truth <- income_tax_data$layer3 |>
    purrr::pluck(h) |>
    dplyr::select(dt, y = !!response) |>
    tail(horizon)

  estimate <- results |>
    purrr::chuck(h) |>
    dplyr::select(dt, y_hat = !!response)

  fcst <- dplyr::full_join(truth, estimate, by = "dt")

  dplyr::bind_rows(
    fcst |> yardstick::smape(y, y_hat),
    fcst |> yardstick::mase(y, y_hat)
  ) |>
    dplyr::mutate(.model_id = model_id)
}

