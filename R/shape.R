#' Arc Shape Grob
#'
#' @description These functions can draw shape on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of shape.
#' @param fill background of shape.
#' @param linewidth border size of shape.
#' @param linetype border type of shape.
#' @param alpha transparency of shape.
#' @param group numeric vector used to separate locations in x and y
#' into multiple polygons.
#' @inheritParams ggforce::geom_shape
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param arc logical, if TRUE will split each path in small pieces.
#' @param steps step length used to split data. A smaller value means a
#' smoother curve.
#' @param simplify logical, When TRUE, line segments equal to x will not be split.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcShapeGrob
#' @author Hou Yun
#' @export
ArcShapeGrob <- function(x = c(120, 60, 60, 120, 120),
                         y = c(0.5, 0.5, 0.9, 0.9, 0.5),
                         colour = NA,
                         fill = "grey20",
                         linewidth = 0.5,
                         linetype = 1,
                         alpha = NA,
                         group = 1L,
                         expand = 0,
                         radius = 0,
                         lineend = "butt",
                         linejoin = "round",
                         linemitre = 10,
                         arc = TRUE,
                         steps = 0.01,
                         simplify = FALSE,
                         ...) {
  data <- data_frame0(x = x,
                      y = y,
                      colour = colour,
                      fill = fill,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha,
                      group = group %||% 1L)

  if (!is.integer(data$group)) {
    data$group <- as.integer(factor(data$group))
  }

  data <- add_endpoint(data)
  data <- data[count_by_group(data) >= 3, , drop = FALSE]
  if (empty(data)) {
    return(zeroGrob())
  }

  if (isTRUE(arc)) {
    data <- pieces_data(data, steps = steps, simplify = simplify)
  }
  data <- polar2cartesian(data)

  first_rows <- data[!duplicated(data$group), , drop = FALSE]
  shapeGrob(x = data$x,
            y = data$y,
            id = data$group,
            default.units = "native",
            radius = radius,
            expand = expand,
            gp = gpar(col = alpha(first_rows$colour, first_rows$alpha),
                      fill = alpha(first_rows$fill, first_rows$alpha),
                      lwd = first_rows$linewidth * .pt,
                      lty = first_rows$linetype,
                      lineend = lineend,
                      linejoin = linejoin,
                      linemitre = linemitre))
}


#' @noRd
shapeGrob <- function(x = c(0, 0.5, 1, 0.5),
                      y = c(0.5, 1, 0.5, 0),
                      id = NULL,
                      id.lengths = NULL,
                      expand = 0,
                      radius = 0,
                      default.units = "npc",
                      name = NULL,
                      gp = gpar(),
                      vp = NULL) {
  rlang::check_installed("ggforce", reason = "for `ArcShapeGrob()`.")
  shapeGrob <- getFromNamespace("shapeGrob", ns = "ggforce")

  shapeGrob(x = x,
            y = y,
            id = id,
            id.lengths = id.lengths,
            expand = expand,
            radius = radius,
            default.units = default.units,
            name = name,
            gp = gp,
            vp = vp)
}
