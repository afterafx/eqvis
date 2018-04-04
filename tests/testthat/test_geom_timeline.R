library(eqvis)
library(ggplot2)
context("test-geom_timeline")

file_url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
raw_data <- read.delim(file_url)
cl_data <- eq_clean_data(raw_data)

test_plot <- ggplot2::ggplot(data = subset(cl_data, !is.na(EQ_PRIMARY) & COUNTRY %in% c("RUSSIA")),
                aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, xmin = as.Date('2000-01-01',"%Y-%m-%d"),
                xmax = as.Date('2016-12-31',"%Y-%m-%d"), color = DEATHS, fill = DEATHS, label = LOCATION_NAME)) +
  geom_timeline(alpha = 0.5)

test_that("ggplot is valid", {
  expect_that(test_plot, is_a("ggplot"))
})
