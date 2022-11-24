test_that("Selected vars", {
  data_result <- var_selection(data = data_to_app, "País", "Otra identidad de género")
  data_expect <- data_to_app[,c("País", "Otra identidad de género")]
  expect_equal(data_result, data_expect)

  data_result <- var_selection(data = data_to_app, Edad, `Identidad de género`, `Orientación sexual`)
  data_expect <- data_to_app[,c("Edad", "Identidad de género", "Orientación sexual")]
  expect_equal(data_result, data_expect)

})
