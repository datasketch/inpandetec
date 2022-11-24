#' @export
var_aggregation <- function(data, ...) {
  if (is.null(data)) return()
  data |>
    dplyr::group_by_all() |>
    dplyr::summarise(...)
}
