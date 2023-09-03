#' @title Boxplot
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_boxplot
#' @return a gg layer object.
#' @family layer
#' @rdname layer_boxplot
#' @author Hou Yun
#' @export
layer_boxplot <- function(..., show.legend = FALSE) {
  ggplot2::geom_boxplot(..., show.legend = show.legend)
}

#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param flipped_aes TRUE means that coordinates are inherit `CoordFlip`.
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_boxplot
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomBoxplot
#' @export
GeomBoxplot2grob <- function(data,
                             trans = NULL,
                             coord = PANEL(),
                             region = CELL(),
                             ...,
                             flipped_aes = FALSE,
                             notch = FALSE,
                             notchwidth = 0.5,
                             fatten = 2,
                             outlier.colour = NULL,
                             outlier.fill = NULL,
                             outlier.shape = 19,
                             outlier.size = 1.5,
                             outlier.stroke = 0.5,
                             outlier.alpha = NULL,
                             clip = FALSE,
                             na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if (is.null(data$linewidth) && !is.null(data$size)) {
    data$linewidth <- data$size
  }

  ngroup <- count_by_group(data)
  if (any(ngroup != 1)) {
    cli::cli_abort(c("Can only draw one boxplot per group",
                     i = "Did you forget {.code aes(group = ...)}?"))
  }

  flipped <- "flipped_aes" %in% names(data) && any(data$flipped_aes)
  data <- flip_data(data, flipped)
  common <- list(colour = data$colour, linewidth = data$linewidth,
                 linetype = data$linetype, fill = alpha(data$fill, data$alpha),
                 group = data$group)
  box <- data_frame0(xmin = data$xmin,
                     xmax = data$xmax,
                     ymin = data$lower,
                     y = data$middle,
                     ymax = data$upper,
                     ynotchlower = if(notch) data$notchlower else NA,
                     ynotchupper = if(notch) data$notchupper else NA,
                     notchwidth = notchwidth,
                     alpha = data$alpha,
                     flipped_aes = flipped,
                     !!!common)
  box <- flip_data(box, flipped)
  box <- GeomCrossbar2grob(data = box,
                           trans = trans,
                           coord = coord,
                           region = region,
                           flipped_aes = flipped_aes,
                           fatten = fatten,
                           clip = clip,
                           na.rm = na.rm,
                           ...)

  whiskers <- data_frame0(x = c(data$x, data$x),
                          xend = c(data$x, data$x),
                          y = c(data$upper, data$lower),
                          yend = c(data$ymax, data$ymin),
                          alpha = NA_real_,
                          linewidth = c(common$linewidth, common$linewidth),
                          linetype = c(common$linetype, common$linetype),
                          colour = c(common$colour, common$colour))
  whiskers <- flip_data(whiskers, flipped)
  whiskers <- GeomSegment2grob(data = whiskers,
                               trans = trans,
                               coord = coord,
                               region = region,
                               clip = clip,
                               na.rm = na.rm,
                               ...)

  has_outliers <- !is.null(data$outliers) && length(unlist(data$outliers)) > 0
  if (has_outliers) {
    n <- vapply_dbl(data$outliers, length)
    outliers <- data[rep(1:nrow(data), n),
                     intersect(names(data), c("colour", "fill", "stroke",
                                              "alpha", "size", "shape")),
                     drop = FALSE]
    outliers$y <- unlist(data$outliers)
    outliers$x <- rep(data$x, n)
    outliers <- flip_data(outliers, flipped)
    outliers <- trans(outliers)
    outliers <- transform(outliers,
                          colour = outlier.colour %||% colour,
                          fill = outlier.fill %||% fill,
                          shape = outlier.shape %||% shape,
                          size = outlier.size %||% size,
                          stroke = outlier.stroke %||% stroke,
                          alpha = outlier.alpha %||% alpha)
    outliers <- cartesian2polar(outliers, coord = coord, region = region,
                                clip = clip, na.rm = na.rm)

    outliers <- exec(ArcPointsGrob, !!!outliers)
  }

  grid::gList(box, whiskers, if (has_outliers) outliers)
}
