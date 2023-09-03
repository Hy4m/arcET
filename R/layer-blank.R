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

#' @export
GeomBlank2grob <- function(data,
                           trans = NULL,
                           coord = PANEL(),
                           region = CELL(),
                           ...) {
  zeroGrob()
}
