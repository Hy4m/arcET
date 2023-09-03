#' Arc Points Grob
#'
#' @description These functions can draw symbols on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param shape numeric or character vector specifying shape of symbols.
#' @param size numeric vector specifying size (in pt) of symbols.
#' @param colour color of symbols.
#' @param fill background of symbols when shape in 21-25.
#' @param alpha transparency of symbols.
#' @param stroke stroke size (in pt) of symbols.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcPointsGrob
#' @author Hou Yun
#' @importFrom grid pointsGrob gpar unit is.unit
#' @importFrom ggplot2 .pt .stroke
#' @importFrom scales alpha
#' @export
ArcPointsGrob <- function(x = 90,
                          y = 0.5,
                          shape = 1,
                          size = 1,
                          colour = "black",
                          fill = NA,
                          alpha = NA,
                          stroke = 0.5,
                          ...) {
  data <- data_frame0(x = x,
                      y = y,
                      shape = shape,
                      size = size,
                      colour = colour,
                      fill = fill,
                      alpha = alpha,
                      stroke = stroke)

  if (empty(data)) {
    return(zeroGrob())
  }

  stroke_size <- ifelse(is.na(data$stroke), 0, data$stroke)
  data <- polar2cartesian(data)

  grid::pointsGrob(
    x = data$x,
    y = data$y,
    pch = data$shape,
    gp = gpar(col = alpha(data$colour, data$alpha),
              fill = alpha(data$fill, data$alpha),
              fontsize = data$size * .pt + stroke_size * .stroke/2,
              lwd = data$stroke * .stroke/2),
    default.units = "native"
  )
}


