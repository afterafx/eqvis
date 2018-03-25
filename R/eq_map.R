#'Earthquake Map
#'
#'Function takes the filtered data frame and produces an interactive map that shows each
#'quakes epicenter based on there \code{LATITUDE} and \code{LONGITUDE}. The size of the radius of each circle is
#'based on the earthquakes magnitude \code{EQ_PRIMARY}.
#'
#'@param data the filtered cleaned data
#'@param annot_col user defined data, for example picking \code{DATE}
#'
#'@import ("maps")
#'@importFrom ("leaflet", "leaflet", "addTiles", "addCircleMarkers")
#'
#'@examples
#'\dontrun{
#'readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#' eq_clean_data() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  eq_map(annot_col = "popup_text")
#'}
#'


eq_map <- function(data, annot_col){
  #data <- as.data.frame(data)
  map <- leaflet::leaflet(data)
  map <- leaflet::addTiles(map)
  map <- leaflet::addCircleMarkers(map, as.numeric(data$LONGITUDE), as.numeric(data$LATITUDE),
                                   radius = data$EQ_PRIMARY,
                                   popup = ~eval(parse(text = annot_col)))
  map
}


#'Added Info for Popup Text
#'
#'This function adds on to the \code{eq_map} function to include more
#'information to the pop up, adding \code{LOCATION NAME}, \code{EQ_PRIMARY},
#'and \code{TOTAL_DEATHS}.
#'
#'@param data cleaned filtered data
#'
#'@examples
#'\dontrun{
#'readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#' eq_clean_data() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#'}
#'


eq_create_label <- function(data){
  paste(sep = "<br/>", paste('<b>Location:</b>', data$LOCATION_NAME),
        paste('<b>Magnitude:</b>', data$EQ_PRIMARY),
        paste('<b>Total Deaths:</b>', data$TOTAL_DEATHS))
}
