#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_raster
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomRaster
#' @export
GeomRaster2grob <- function(data,
                            trans = NULL,
                            coord = PANEL(),
                            region = CELL(),
                            ...,
                            clip = FALSE,
                            na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  data <- data[intersect(names(data), c("xmin", "ymin", "xmax", "ymax", "fill", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = FALSE, na.rm = na.rm)
  exec(ArcRectGrob, !!!data, ..., linewidth = 0, colour = NA, linetype = 1, arc = FALSE)
}
