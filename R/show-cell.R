#' Show CELL
#' @description A quick way to show region of `CELL()`.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param fill fill colour of region.
#' @param colour border colour of region.
#' @param digits integer indicating the number of decimal places.
#' @param xn,yn number of xaxis/yaxis ticks.
#' @param ... other parameters passing to `ArcRectGrob()`.
#' @return invisible grobs.
#' @rdname show_cell
#' @author Hou Yun
#' @export
#' @examples
#' show_cell(CELL(start = 120, end = 60, r0 = 0.5))
show_cell <- function(region = CELL(),
                      fill = "grey90",
                      colour = NA,
                      digits = 2,
                      xn = 7,
                      yn = 7,
                      ...) {
  stopifnot(is_CELL(region))

  xseq <- seq(region$x.range[1], region$x.range[2], length.out = xn) %% 360
  x_labels <- paste0(round(xseq, digits = digits), "°")
  yseq <- seq(region$y.range[1], region$y.range[2], length.out = yn)
  y_labels <- as.character(round(yseq, digits = digits))
  y_breaks <- if (region$r0 > region$r1) {
    seq(1, 0, length.out = yn)
  } else {
    seq(0, 1, length.out = yn)
  }
  coord <- PANEL(x_limits = c(0, 1),
                 x_breaks = seq(0, 1, length.out = xn),
                 x_labels = x_labels,
                 x_range = c(0, 1),
                 y_limits = c(0, 1),
                 y_breaks = y_breaks,
                 y_labels = y_labels,
                 y_range = c(0, 1))

  xaxis <- ArcxAxisGrob(coord = coord, region = region)
  yaxis <- ArcyAxisGrob(coord = coord, region = region)
  panel <- ArcPanelGrob(region = region, fill = fill, colour = colour, ...)

  grobs <- gTree(children = gList(panel, xaxis, yaxis))
  new_panel()
  grid.draw(grobs)
  invisible(grobs)
}
