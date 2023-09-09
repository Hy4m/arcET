#' Show CELL
#' @description A quick way to show region of `CELL()`.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param fill fill colour of region.
#' @param colour border colour of region.
#' @param digits integer indicating the number of decimal places.
#' @param xn,yn number of xaxis/yaxis ticks.
#' @param newpage logical, indicating whether a new graphics device needs to be opened.
#' @param vp grid viewport object.
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
                      newpage = is.null(vp),
                      vp = NULL,
                      ...) {
  stopifnot(is_CELL(region))

  xseq <- seq(region$x.range[1], region$x.range[2], length.out = xn) %% 360
  x_labels <- paste0(round(xseq, digits = digits), iconv("\u00b0", to = "UTF-8"))
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

  grobs <- gTree(children = gList(panel, xaxis, yaxis),
                 vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1)))
  gt <- gtable(widths = unit(c(1.5, 1, 1.5), c("cm", "null", "cm")),
               heights = unit(c(1.5, 1, 1.5), c("cm", "null", "cm")),
               respect = TRUE)
  gt <- gtable_add_grob(gt, grobs, t = 2, l = 2, clip = "off",
                        name = "Cell-Region")
  if (isTRUE(newpage)) {
    grid::grid.newpage()
  }

  grDevices::recordGraphics(requireNamespace("arcET", quietly = TRUE),
                            list(), getNamespace("arcET"))
  if (is.null(vp)) {
    grid::grid.draw(gt)
  } else {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
    }
    grid::grid.draw(grobs)

    grid::upViewport()
  }

  invisible(gt)
}
