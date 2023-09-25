#' Show CELL
#' @description A quick way to show region of `CELL()`.
#' @param ... any CELL objects (created by `CELL()` function) used to set
#' the drawing area.
#' @param fill fill colour of region.
#' @param colour border colour of region.
#' @param background,border fill and border colour of plot region (unit circle).
#' @param digits integer indicating the number of decimal places.
#' @param xn,yn number of xaxis/yaxis ticks.
#' @param newpage logical, indicating whether a new graphics device needs to be opened.
#' @param vp grid viewport object.
#' @return invisible grobs.
#' @rdname show_cell
#' @author Hou Yun
#' @export
#' @examples
#' show_cell(CELL(120, 60, 0.6))
#' show_cell(CELL(120, 60, 0.6), CELL(300, 240, 0.3, 0.6))
show_cell <- function(...,
                      fill = "lightblue",
                      colour = NA,
                      background = "grey95",
                      border = NA,
                      digits = 2,
                      xn = 7,
                      yn = 7,
                      newpage = is.null(vp),
                      vp = NULL) {
  dots <- list(...)

  if (!all(vapply_lgl(dots, is_CELL))) {
    cli::cli_abort("All `region` should be created by {.fn CELL}")
  }

  gt <- gtable(widths = unit(c(1.5, 1, 1.5), c("cm", "null", "cm")),
               heights = unit(c(1.5, 1, 1.5), c("cm", "null", "cm")),
               respect = TRUE)

  bg <- grid::circleGrob(x = 0,
                         y = 0,
                         r = 1,
                         default.units = "native",
                         vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1)),
                         gp = gpar(col = border, fill = background))
  gt <- gtable_add_grob(gt, bg, t = 2, l = 2, z = -Inf, clip = "off",
                        name = "background")

  for (ii in seq_along(dots)) {
    REGION <- dots[[ii]]
    xseq <- seq(REGION$x.range[1], REGION$x.range[2], length.out = xn) %% 360
    x_labels <- paste0(round(xseq, digits = digits), iconv("\u00b0", to = "UTF-8"))
    yseq <- seq(REGION$y.range[1], REGION$y.range[2], length.out = yn)
    y_labels <- as.character(round(yseq, digits = digits))
    y_breaks <- if (REGION$r0 > REGION$r1) {
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


    xaxis <- ArcxAxisGrob(coord = coord, region = REGION)
    yaxis <- ArcyAxisGrob(coord = coord, region = REGION)
    panel <- ArcPanelGrob(region = REGION, fill = fill, colour = colour)
    direct_x <- ArcSegmentsGrob(x = REGION$x.range[1],
                                xend = REGION$x.range[2],
                                y = min(REGION$y.range),
                                yend = min(REGION$y.range),
                                colour = "red",
                                arrow = grid::arrow(length = unit(0.3, "cm")))
    direct_y <- ArcSegmentsGrob(x = min(REGION$x.range),
                                xend = min(REGION$x.range),
                                y = REGION$y.range[1],
                                yend = REGION$y.range[2],
                                colour = "red",
                                arrow = grid::arrow(length = unit(0.3, "cm")))
    grobs <- gTree(children = gList(panel, xaxis, yaxis, direct_x, direct_y),
                   vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1)),
                   name = "Cell-Region")
    gt <- gtable_add_grob(gt, grobs, t = 2, l = 2, clip = "off",
                          name = paste("Cell-Region", ii, sep = "-"))
  }

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
    grid::grid.draw(gt)

    grid::upViewport()
  }

  invisible(gt)
}

#' Show Layout
#' @description A quick way to show layout of ArcPlot.
#' @param plot an ArcPlot object.
#' @param fill fill colour of region.
#' @param colour border colour of region.
#' @param background,border fill and border colour of plot region (unit circle).
#' @param ... other parameters passing to `ArcBannerTextGrob()`.
#' @param newpage logical, indicating whether a new graphics device needs to be opened.
#' @param vp grid viewport object.
#' @return invisible grobs.
#' @rdname show_layout
#' @author Hou Yun
#' @export
show_layout <- function(plot,
                        fill = "lightblue",
                        colour = NA,
                        background = "grey95",
                        border = NA,
                        ...,
                        newpage = is.null(vp),
                        vp = NULL) {
  stopifnot(is_ArcPlot(plot))

  gt <- gtable(widths = unit(c(1.5, 1, 1.5), c("cm", "null", "cm")),
               heights = unit(c(1.5, 1, 1.5), c("cm", "null", "cm")),
               respect = TRUE)

  bg <- grid::circleGrob(x = 0,
                         y = 0,
                         r = 1,
                         default.units = "native",
                         vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1)),
                         gp = gpar(col = border, fill = background))
  gt <- gtable_add_grob(gt, bg, t = 2, l = 2, z = -Inf, clip = "off",
                        name = "background")

  for (ii in seq_along(plot$region)) {
    region <- plot$region[[ii]]
    CellID <- plot$CellID[[ii]]

    panel <- ArcPanelGrob(region = region, fill = fill, colour = colour)
    label <- ArcBannerTextGrob(x = region$mid_x, y = region$mid_y,
                               label = CellID, ...)

    grob <- gTree(children = gList(panel, label),
                  vp = viewport(xscale = c(-1, 1), yscale = c(-1, 1)))
    gt <- gtable_add_grob(gt, grob, t = 2, l = 2, clip = "off",
                          name = paste(CellID, "panel", sep = "-"))
  }

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
    grid::grid.draw(gt)

    grid::upViewport()
  }

  invisible(gt)
}
