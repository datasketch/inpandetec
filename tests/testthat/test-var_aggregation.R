test_that("Aggregation", {

  var_to_agg <- var_selection(data = data_to_app, "País")
  data_result <- var_aggregation(var_to_agg, Total = dplyr::n())
  data_expect <- data_to_app |> dplyr::group_by(País) |> dplyr::summarise(Total = dplyr::n())
  expect_equal(data_result, data_expect)
})
