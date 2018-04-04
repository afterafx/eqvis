library(eqvis)
context("test-eq_map")

file_url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
raw_data <- read.delim(file_url)
cl_data <- eq_clean_data(raw_data)
fl_data <- cl_data %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")

test_that("Check is data is a leaflet object", {
  expect_that(fl_data, is_a("leaflet"))
})

