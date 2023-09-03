#' @title Points Layer
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_point
#' @return a gg layer object.
#' @family layer
#' @rdname layer_point
#' @author Hou Yun
#' @export
layer_point <- function(..., show.legend = FALSE) {
  ggplot2::geom_point(..., show.legend = show.legend)
}

#' @rdname layer_point
#' @export
layer_jitter <- function(..., show.legend = FALSE) {
  ggplot2::geom_jitter(..., show.legend = show.legend)
}

#' @rdname layer_point
#' @export
layer_count <- function(..., show.legend = FALSE) {
  ggplot2::geom_count(..., show.legend = show.legend)
}

#' @rdname layer_point
#' @export
layer_hex <- function(..., show.legend = FALSE) {
  ggplot2::geom_hex(..., show.legend = show.legend)
}

#' @rdname layer_point
#' @export
layer_bin_2d <- function(..., show.legend = FALSE) {
  ggplot2::geom_bin_2d(..., show.legend = show.legend)
}

#' @rdname layer_point
#' @export
layer_bin2d <- layer_bin_2d

#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param ... other parameters passing to `Arc*Grob()` function.
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
