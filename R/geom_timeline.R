library(ggplot2)
library(dplyr)
library(grid)
library(scales)

geom_timeline <- function(mapping = NULL, data = NULL, na.rm = TRUE,
                          position = "identity", stat = "identity",
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}


GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 non_missing_aes = c("size", "shape", "colour","y"),
                                 default_aes = ggplot2::aes(
                                   shape = 19, colour = "black", size = 5, fill = NA,
                                   alpha = 0.2, stroke = 0.5, y = 0.2
                                 ),

                                 draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                                   coords <- coord$transform(data, panel_params)
                                   points = grid::pointsGrob(
                                     coords$x, coords$y,
                                     pch = coords$shape,
                                     gp = gpar(
                                       col = scales::alpha(coords$colour, coords$alpha),
                                       fill = scales::alpha(coords$fill, coords$alpha),
                                       # Stroke is added around the outside of the point
                                       fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
                                       lwd = coords$stroke * ggplot2::.stroke / 2
                                     )
                                   )
                                   lines = lapply(unique(coords$y), FUN = function(x) grid::linesGrob(y = c(x,x)))
                                   grobList = c(list(points), lines)
                                   grid::gTree(children = do.call(gList, grobList))
                                 },
                                 draw_key = ggplot2::draw_key_point
)
