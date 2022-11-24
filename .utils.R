# create data file
usethis::use_data_raw()

# add test to data filters
usethis::use_test("data_filter")
# add test to var select
usethis::use_test("var_selection")
# add test to var_aggregation
usethis::use_test("var_aggregation")

# build
devtools::load_all()
devtools::document()
devtools::install()

# check package
devtools::check()

# add packages in description
attachment::att_amend_desc()
