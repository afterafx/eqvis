#' Building a Timeline geom
#'
#'When running \code{geom_timeline} function, it produces a timeline of the
#'earthquakes to visualize them by date.
#'
#'@return Returns a timeline plot in date order of each earthquake that occur within certain countries and hows the
#'magnitude of each earthquake by its Richter Scale value as well as the number of deaths associated with each earthquake.
#'
# @param data cleaned earthquake data
# @param ... other arguments used for layer function such as aesthetics.
#'
#'@importFrom ("ggplot2", "ggproto", "Geom", "aes", "draw_key_point", "layer")
#'@importFrom ("grid", "pointsGrob", "segmentsGrob", "gpar", "grobTree")
#'@importFrom ("scales", "alpha")
#'
#'@examples
#'\dontrun{
#'ggplot(data=subset(cleaned_data, !is.na(EQ_PRIMARY) & COUNTRY %in% c("RUSSIA")),
#' aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, xmin = as.Date('2000-01-01',"%Y-%m-%d"),
#'    xmax = as.Date('2016-12-31',"%Y-%m-%d"), color = DEATHS, fill = DEATHS, label = LOCATION_NAME)) +
#'  geom_timeline(alpha = .5) +
#'  scale_size_continuous(name = "Richter Scale Value") +
#'  scale_color_continuous(name = "# Deaths") +
#'  scale_fill_continuous(guide=FALSE) +
#'  labs(y = "") +
#'  theme(legend.position = "bottom",
#'        legend.key = element_blank(), legend.box = "horizontal", panel.background = element_blank(),
#'        axis.line.x = element_line(size = .5, color = "black"), axis.ticks.x = element_line(size = .5))
#'}


GeomTimeline <- ggplot2::ggproto("GeoTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y = 0.5, shape = 20, alpha = 0.5, size = 1, colour = "black", fill = "grey", stroke = 0.5),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord){
                                   ## Transform the data first
                                   coords <- coord$transform(data, panel_scales)

                                   ## Construct a grid grob
                                   date_grob <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       col = scales::alpha(coords$colour, coords$alpha),
                                       fill = scales::alpha(coords$fill, coords$alpha),
                                       fontsize = coords$size * .pt + coords$stroke * .stroke/2
                                     )
                                   )


                                   timeline_grob <- grid::segmentsGrob(
                                     x0 = coords$xmin,
                                     y0 = coords$y,
                                     x1 = coords$xmax,
                                     y1 = coords$y,
                                     gp = grid::gpar(
                                       col = "lightgrey",
                                       lwd = 1
                                     )
                                   )

                                   grid::grobTree(timeline_grob, date_grob, name = "timeline")

                                 })

geom_timeline <- function(mapping = NULL, data = NULL, stat = "timeline",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
            geom = GeomTimeline, mapping = mapping,
            data = data, stat = stat, position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
  )
}

#'Adds labels to the top earthquakes based on magnitude and user input
#'
# @param n_max max number of earthquakes based on magnitude.
#'
#'@importFrom ("ggplot2", "ggproto", "Geom", "aes", "draw_key_point", "layer")
#'@importFrom ("grid", "pointsGrob", "segmentsGrob", "gpar", "grobTree")
#'
#'@examples
#'\dontrun{
#'ggplot(data=subset(cleaned_data, !is.na(EQ_PRIMARY) & COUNTRY %in% c("USA", "RUSSIA")),
#' aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, xmin = as.Date('2000-01-01',"%Y-%m-%d"),
#'    xmax = as.Date('2016-12-31',"%Y-%m-%d"), color = DEATHS, fill = DEATHS, label = LOCATION_NAME)) +
#'  geom_timeline(alpha = .5) +
#'  scale_size_continuous(name = "Richter Scale Value") +
#'  scale_color_continuous(name = "# Deaths") +
#'  scale_fill_continuous(guide=FALSE) +
#'  labs(y = "") +
#'  geom_timeline_label(aes(n_max = 5)) +
#'  theme(legend.position = "bottom",
#'        legend.key = element_blank(), legend.box = "horizontal", panel.background = element_blank(),
#'        axis.line.x = element_line(size = 1, color = "black"), axis.ticks.x = element_line(size = 1))
#'}

GeomTimelineLabel <- ggplot2::ggproto("GeoTimelineLabel", ggplot2::Geom,
                                    required_aes = c("x", "label"),
                                    default_aes = ggplot2::aes(y = 0, colour = "lightgrey"),
                                    draw_key = ggplot2::draw_key_point,
                                    draw_panel = function(data, panel_scales, coord){
                                      coords <- coord$transform(data, panel_scales)

                                      ## line_length = .1 / length(unique(data$y))

                                      word_grob <- grid::textGrob(
                                        coords$label,
                                        coords$x,
                                        coords$y + .05,
                                        hjust = 0,
                                        vjust = 0,
                                        rot = 45
                                      )

                                      vertical_grob <- grid::segmentsGrob(
                                        coords$x,
                                        coords$y,
                                        coords$x,
                                        coords$y + .05
                                      )

                                      grid::grobTree(vertical_grob, word_grob, name = "timelineLabel")
                                    }
)


geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "timeline",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
            geom = GeomTimelineLabel, mapping = mapping,
            data = data, stat = stat, position = position,
            show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
  )
}

#'Stat function to support GeomTimeline and GeomTimelineLabel

StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(xmin = NULL, xmax = NULL, n_max = NULL),
                                 compute_group = function(data, scales) {
                                   if (is.null(data$xmin)) data$xmin <- min(data$x)
                                   if (is.null(data$xmax)) data$xmax <- max(data$x)
                                   df <- data[data$x >= data$xmin & data$x <= data$xmax,]
                                   if (is.null(df$n_max)) df$n_max <- length(df$x)
                                   df <- df[order(df$size,decreasing=T)[1:df$n_max[1]],]
                                   df
                                 }
)


stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimeline, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
