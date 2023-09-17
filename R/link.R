#' Arc Chord Grob
#'
#' @description These functions can draw chord on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values of start points.
#' @param y positive numeric vector (in radius) specifying y-values of start points.
#' @param xend numeric vector (in degree) specifying x-values of end points.
#' @param yend positive numeric vector (in radius) specifying y-values of end points.
#' @param width width of start (in degree), should be less than 360.
#' @param width_end width of end (in degree), should be less than 360.
#' @param colour color of lines.
#' @param fill fill colour of lines
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @param height numeric value in [0, 1].
#' @param n integer value. The larger the value, the smoother the curve.
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param ... other parameters passing to `pieces_data()`.
#' @details `ArcLinkGrob()` used to drawing segment links, and `ArcBezierGrob()`
#' used to drawing bezier links on center.
#' @return a grob object.
#' @rdname ArcChordGrob
#' @author Hou Yun
#' @export
ArcChordGrob <- function(x = 20,
                         y = 0.5,
                         xend = 70,
                         yend = 0.5,
                         width = 10,
                         width_end = 20,
                         colour = NA,
                         fill = "grey25",
                         linewidth = 0.5,
                         linetype = 1,
                         alpha = NA,
                         height = 0.6,
                         n = 100,
                         lineend = "butt",
                         linejoin = "round",
                         linemitre = 10,
                         ...) {
  data <- data_frame0(x = (x %% 360) + 360,
                      y = y,
                      xend = (xend %% 360) + 360,
                      yend = yend,
                      width = abs(width) %% 360,
                      width_end = abs(width_end) %% 360,
                      colour = colour,
                      fill = fill,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)

  large_than_360 <- (data$width + data$width_end) > 360
  if (any(large_than_360)) {
    cli::cli_warn(c("Sum of `width` and `width_end` must be less than 360,",
                    "Removed {sum(large_than_360)} rows with too large width."))
    data <- data[!large_than_360, , drop = FALSE]
  }

  if (empty(data)) {
    return(zeroGrob())
  }

  data <- gen_chord(data = data, height = height, n = n, ...)
  first_row <- data[!duplicated(data$group), , drop = FALSE]

  grid::polygonGrob(x = data$x,
                    y = data$y,
                    id = data$group,
                    default.units = "native",
                    gp = gpar(col = alpha(first_row$colour, first_row$alpha),
                              fill = alpha(first_row$fill, first_row$alpha),
                              lwd = first_row$linewidth * .pt,
                              lty = first_row$linetype,
                              lineend = lineend,
                              linejoin = linejoin,
                              linemitre = linemitre))
}

#' Arc Link Grob
#'
#' @description These functions can draw segments or bezier curve on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values of start points.
#' @param y positive numeric vector (in radius) specifying y-values of start points.
#' @param xend numeric vector (in degree) specifying x-values of end points.
#' @param yend positive numeric vector (in radius) specifying y-values of end points.
#' @param colour color of line.
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @param arrow NULL or arrow object created by `arrow()`.
#' @param height numeric value in [0, 1].
#' @param n integer value. The larger the value, the smoother the curve.
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param ... not used.
#' @details `ArcLinkGrob()` used to drawing segment links, and `ArcBezierGrob()`
#' used to drawing bezier links on center.
#' @return a grob object.
#' @rdname ArcBezierGrob
#' @author Hou Yun
#' @export
ArcBezierGrob <- function(x = 20,
                          y = 0.5,
                          xend = 70,
                          yend = 0.5,
                          height = 0.6,
                          n = 100,
                          colour = "black",
                          linewidth = 0.5,
                          linetype = 1,
                          alpha = NA,
                          arrow = NULL,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 10,
                          ...) {
  data <- data_frame0(x = x,
                      y = y,
                      xend = xend,
                      yend = yend,
                      colour = colour,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)

  if (empty(data)) {
    return(zeroGrob())
  }

  data <- gen_bezier(data = data, height = height, n = n, ...)
  first_row <- data[!duplicated(data$group), , drop = FALSE]

  grid::polylineGrob(x = data$x,
                     y = data$y,
                     id = data$group,
                     arrow = arrow,
                     default.units = "native",
                     gp = gpar(col = alpha(first_row$colour, first_row$alpha),
                               lwd = first_row$linewidth * .pt,
                               lty = first_row$linetype,
                               lineend = lineend,
                               linejoin = linejoin,
                               linemitre = linemitre))
}

#' @param arc logical. If TURE will cut segments into smaller segments.
#' @rdname ArcBezierGrob
#' @export
ArcLinkGrob <- function(x = 20,
                        y = 0.5,
                        xend = 70,
                        yend = 0.7,
                        arc = TRUE,
                        colour = "black",
                        linewidth = 0.5,
                        linetype = 1,
                        alpha = NA,
                        ...) {
  ArcSegmentsGrob(x = x,
                  y = y,
                  xend = xend,
                  yend = yend,
                  arc = arc,
                  colour = colour,
                  linewidth = linewidth,
                  linetype = linetype,
                  alpha = alpha,
                  ...)
}



