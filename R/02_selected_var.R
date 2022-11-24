#' @export
var_selection <- function(data, ...) {
  if (is.null(data)) return()
  data |> dplyr::select(...)
}
