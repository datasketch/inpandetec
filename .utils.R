# create data file
usethis::use_data_raw()

# add test to data filters
usethis::use_test("data_filter")


# build
devtools::load_all()
devtools::document()
devtools::install()

# check package
devtools::check()
