test_that("filter data", {
  test_list <- list("País" = c("Guatemala"),
                     "Orientación sexual" = c("Bisexual", "Gay"))
  data_result <- data_filter(data_to_app, var_inputs = test_list)
  data_expect <- data_to_app |> dplyr::filter(País %in% "Guatemala",
                                       `Orientación sexual` %in% c("Bisexual", "Gay"))
  expect_equal(data_result, data_expect)
})
