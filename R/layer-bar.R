#' @title Bar
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_bar
#' @return a gg layer object.
#' @family layer
#' @rdname layer_bar
#' @author Hou Yun
#' @export
layer_bar <- function(..., show.legend = FALSE) {
  ggplot2::geom_bar(..., show.legend = show.legend)
}

#' @rdname layer_bar
#' @export
layer_col <- function(..., show.legend = FALSE) {
  ggplot2::geom_col(..., show.legend = show.legend)
}

#' @rdname layer_bar
#' @export
layer_histogram <- function(..., show.legend = FALSE) {
  ggplot2::geom_histogram(..., show.legend = show.legend)
}

#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param flipped_aes TRUE means that coordinates are inherit `CoordFlip`.
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_bar
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomBar
#' @export
GeomBar2grob <- function(data,
                         trans = NULL,
                         coord = PANEL(),
                         region = CELL(),
                         ...,
                         clip = FALSE,
                         na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob())
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  data <- data[intersect(names(data), c("xmin", "xmax", "ymin", "ymax", "colour",
                                        "fill", "linewidth", "linetype", "alpha"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcRectGrob, !!!data, ...)
}

#' @rdname GeomBar
#' @export
GeomCol2grob <- function(data,
                         trans = NULL,
                         coord = PANEL(),
                         region = CELL(),
                         ...,
                         clip = FALSE,
                         na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob())
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  data <- data[intersect(names(data), c("xmin", "xmax", "ymin", "ymax", "colour",
                                        "fill", "linewidth", "linetype", "alpha"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcRectGrob, !!!data, ...)
}
