#' Cleaning up the Raw data
#'
#' The \code{eq_clean_data} function takes the raw data frame data and cleans up the data.
#' Takes the (\code{YEAR, MONTH, and DAY}) column and converts them into a date format.
#' It also sends the \code{LOCATION_NAME} to be cleaned up.
#'
#' @param x data from the earthquake dataset
#'
#' @return Returns a cleaned data frame with a new \code{DATE} column containing the (\code{YEAR, {MONTH, and DAY}) in a date format.
#' It also removed the country and colon from \code{LOCATION_NAME} as well as change the format to title case.
#'
#' @note Added 1's for dates with missing values.
#'
#' @importFrom ("lubridate", "ymd")
#'
#' @examples
#' \dontrun{
#' cleaned_data <- eq_clean_data(raw_data)
#' }
#'
#' @export

eq_clean_data <- function(x){

  x$DATE <- paste(x$YEAR, ifelse(is.na(x$MONTH),1, x$MONTH), ifelse(is.na(x$DAY),1, x$DAY)) %>%
    lubridate::ymd()

  x$LOCATION_NAME <- eq_location_clean(x$LOCATION_NAME)

  x
}

#' Cleaning location column
#'
#' Removes the country and colon from all the entries in \code{LOCATION_NAME}
#'
#' @param location_name Column containing location data.
#'
#' @return Returns with a cleaned up \code{LOCATION_NAME} column with no country and colon present.
#'
#'@export


eq_location_clean <- function(location_name){

  location_name <- gsub(pattern = '[A-Z ]*: *',replacement = " ", x = location_name)
  location_name <- gsub(pattern = "\\b([A-Za-z])([A-Za-z]*)", replacement = "\\U\\1\\L\\2", x = location_name, perl = TRUE)

  location_name
}



