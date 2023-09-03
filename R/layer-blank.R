#' @title Blank
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_blank
#' @return a gg layer object.
#' @family layer
#' @rdname layer_blank
#' @author Hou Yun
#' @export
layer_blank <- function(..., show.legend = FALSE) {
  ggplot2::geom_blank(..., show.legend = show.legend)
}

#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomBlank
#' @export
GeomBlank2grob <- function(data,
                           trans = NULL,
                           coord = PANEL(),
                           region = CELL(),
                           ...) {
  zeroGrob()
}
