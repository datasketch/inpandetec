#' @export
data_filter <- function(data, var_inputs) {
  if (is.null(data)) return()
  df <- data
  if (is.null(var_inputs)) return()
  if (!is.list(var_inputs)) return()
  tem_ls <-
    seq_along(var_inputs) |>
    purrr::map(function(.x) {
      if (!is.null(var_inputs[[.x]])) {
        name_var <- names(var_inputs)[.x]
        filter_var <- var_inputs[[.x]]
        if ("Edad" %in% name_var) {
          if (length(filter_var) == 2) filter_var <- filter_var[1]:filter_var[2]
        }
        df <<- df |>
          dplyr::filter(!!dplyr::sym(name_var) %in% filter_var)
      }
    })
  rm(tem_ls)
  df
}

