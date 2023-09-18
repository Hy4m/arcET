#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_hex
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomPoint
#' @export
GeomPoint2grob <- function(data,
                           trans = NULL, # always extract from ggplot
                           coord = PANEL(),
                           region = CELL(),
                           ...,
                           clip = FALSE,
                           na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob())
  }

  data <- trans(data)
  if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPointsGrob, !!!data, ...)
}

#' @rdname GeomPoint
#' @export
GeomHex2grob <- function(data,
                         trans = NULL,
                         coord = PANEL(),
                         region = CELL(),
                         ...,
                         lineend = "butt",
                         linejoin = "mitre",
                         linemitre = 10,
                         clip = FALSE,
                         na.rm = FALSE) {
  ## modify from GeomHex$draw_group
  if (empty(data)) {
    return(zeroGrob())
  }

  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  n <- nrow(data)
  data$group <- 1:n
  data <- data[rep(data$group, each = 6), , drop = FALSE]

  dx <- resolution(data$x, FALSE)
  dy <- resolution(data$y, FALSE)/sqrt(3)/2 * 1.15
  hexC <- hexbin::hexcoords(dx, dy, n = n)
  data$x <- hexC$x + data$x
  data$y <- hexC$y + data$y

  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPolygonGrob, !!!data, ..., arc = FALSE,
       lineend = lineend, linejoin = linejoin, linemitre = linemitre)
}
