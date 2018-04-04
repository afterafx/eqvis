library(eqvis)
context("test_clean_data")

file_url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
raw_data <- read.delim(file_url)

test_that("Test clean data if it's a dataframe", {
  expect_that(eq_clean_data(raw_data), is_a("data.frame"))
})
