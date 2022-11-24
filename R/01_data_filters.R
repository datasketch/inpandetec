#' @export
data_filter <- function(data, var_inputs) {
  if (is.null(data)) return()
  df <- data
  if (is.null(var_inputs)) return()
  if (class(var_inputs) != "list") return()
  tem_ls <-
    seq_along(var_inputs) |>
    purrr::map(function(.x) {
      df <<- df |>
        dplyr::filter(!!dplyr::sym(names(var_inputs)[.x]) %in% var_inputs[[.x]])
    })
  rm(tem_ls)
  df
}

