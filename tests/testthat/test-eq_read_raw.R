library(eqvis)
context("test_eq_read_raw")

file_url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
raw_data <- read.delim(file_url)

test_that("Data from URL converted to a Dataframe", {
  expect_that(raw_data, is_a("data.frame"))
})
