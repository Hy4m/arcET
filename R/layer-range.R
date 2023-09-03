#' @title Intervals
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_linerange
#' @return a gg layer object.
#' @family layer
#' @rdname layer_linerange
#' @author Hou Yun
#' @export
layer_pointrange <- function(..., show.legend = FALSE) {
  ggplot2::geom_pointrange(..., show.legend = show.legend)
}

#' @rdname layer_linerange
#' @export
layer_linerange <- function(..., show.legend = FALSE) {
  ggplot2::geom_linerange(..., show.legend = show.legend)
}

#' @rdname layer_linerange
#' @export
layer_errorbar <- function(..., show.legend = FALSE) {
  ggplot2::geom_errorbar(..., show.legend = show.legend)
}

#' @rdname layer_linerange
#' @export
layer_errorbarh <- function(..., show.legend = FALSE) {
  ggplot2::geom_errorbarh(..., show.legend = show.legend)
}

#' @rdname layer_linerange
#' @export
layer_crossbar <- function(..., show.legend = FALSE) {
  ggplot2::geom_crossbar(..., show.legend = show.legend)
}

#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param flipped_aes TRUE means that coordinates are inherit `CoordFlip`.
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_pointrange
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomLinerange
#' @export
GeomLinerange2grob <- function(data,
                               trans = NULL,
                               coord = PANEL(),
                               region = CELL(),
                               ...,
                               flipped_aes = FALSE,
                               clip = FALSE,
                               na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  flipped <- if (isTRUE(flipped_aes)) {
    if (any(data$flipped_aes)) {
      FALSE
    } else {
      TRUE
    }
  } else {
    any(data$flipped_aes)
  }
  data <- flip_data(data, flipped)
  data <- transform(data, xend = x, y = ymin, yend = ymax)
  data <- data[intersect(names(data), c("x", "y", "xend", "yend", "colour",
                                        "linetype", "linewidth", "alpha"))]
  data <- flip_data(data, flipped)
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcSegmentsGrob, !!!data, ...)
}

#' @rdname GeomLinerange
#' @export
GeomPointrange2grob <- function(data,
                                trans = NULL,
                                coord = PANEL(),
                                region = CELL(),
                                ...,
                                flipped_aes = FALSE,
                                fatten = 4,
                                clip = FALSE,
                                na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }

  if ("size" %in% names(data) && !"linewidth" %in% names(data)) {
    data$linewidth <- data$size
  }
  g_line <- GeomLinerange2grob(data = data[setdiff(names(data), "size")],
                               trans = trans,
                               coord = coord,
                               region = region,
                               ...,
                               flipped_aes = flipped_aes,
                               clip = clip,
                               na.rm = na.rm)
  if (!all(c("x", "y") %in% names(data))) {
    return(g_line)
  } else {
    points <- data[intersect(names(data), c("x", "y", "colour", "fill",
                                            "stroke", "alpha", "size", "shape"))]
    points <- transform(data, size = size * fatten)
    g_point <- GeomPoint2grob(data = points,
                              trans = trans,
                              coord = coord,
                              region = region,
                              clip = clip,
                              na.rm = na.rm)
    grid::gList(g_line, g_point)
  }
}

#' @rdname GeomLinerange
#' @export
GeomErrorbar2grob <- function(data,
                              trans = NULL,
                              coord = PANEL(),
                              region = CELL(),
                              ...,
                              flipped_aes = FALSE,
                              clip = FALSE,
                              na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  flipped <- if (isTRUE(flipped_aes)) {
    if (any(data$flipped_aes)) {
      FALSE
    } else {
      TRUE
    }
  } else {
    any(data$flipped_aes)
  }
  data <- flip_data(data, flipped)

  extra <- c("colour", "linetype", "linewidth", "alpha")
  data <- data_frame0(x = c(data$x, data$xmin, data$xmin),
                      y = c(data$ymin, data$ymin, data$ymax),
                      xend = c(data$x, data$xmax, data$xmax),
                      yend = c(data$ymax, data$ymin, data$ymax),
                      data[rep(1:nrow(data), 3), extra])

  data <- flip_data(data, flipped)
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcSegmentsGrob, !!!data, ...)
}

#' @rdname GeomLinerange
#' @export
GeomErrorbarh2grob <- function(data,
                               trans = NULL,
                               coord = PANEL(),
                               region = CELL(),
                               ...,
                               flipped_aes = FALSE,
                               clip = FALSE,
                               na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  data <- flip_data(data, !flipped_aes)
  extra <- c("colour", "linetype", "linewidth", "alpha")
  data <- data_frame0(x = c(data$x, data$xmin, data$xmin),
                      y = c(data$ymin, data$ymin, data$ymax),
                      xend = c(data$x, data$xmax, data$xmax),
                      yend = c(data$ymax, data$ymin, data$ymax),
                      data[rep(1:nrow(data), 3), extra])

  data <- flip_data(data, !flipped_aes)
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcSegmentsGrob, !!!data, ...)
}

#' @rdname GeomLinerange
#' @export
GeomCrossbar2grob <- function(data,
                              trans = NULL,
                              coord = PANEL(),
                              region = CELL(),
                              ...,
                              flipped_aes = FALSE,
                              fatten = 2.5,
                              clip = FALSE,
                              na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  flipped <- "flipped_aes" %in% names(data) && any(data$flipped_aes)
  data <- flip_data(data, flipped)
  middle <- transform(data, x = xmin, xend = xmax, yend = y,
                      linewidth = linewidth * fatten, alpha = NA)
  middle <- middle[intersect(names(middle), c("x", "y", "xend", "yend", "colour",
                                              "linetype", "linewidth", "alpha"))]

  has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
    !any(is.na(data$ynotchlower)) && !any(is.na(data$ynotchupper))

  if (has_notch) {
    notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin)/2
    middle$x <- middle$x + notchindent
    middle$xend <- middle$xend - notchindent
    box <- data_frame0(x = c(data$xmin, data$xmin, data$xmin + notchindent,
                             data$xmin, data$xmin, data$xmax, data$xmax,
                             data$xmax - notchindent, data$xmax, data$xmax, data$xmin),
                       y = c(data$ymax, data$ynotchupper, data$y, data$ynotchlower,
                             data$ymin, data$ymin, data$ynotchlower, data$y,
                             data$ynotchupper, data$ymax, data$ymax),
                       alpha = rep(data$alpha, 11),
                       colour = rep(data$colour, 11),
                       linewidth = rep(data$linewidth, 11),
                       linetype = rep(data$linetype, 11),
                       fill = rep(data$fill, 11),
                       group = rep(seq_len(nrow(data)), 11))
  } else {
    box <- data_frame0(x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
                       y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
                       alpha = rep(data$alpha, 5),
                       colour = rep(data$colour, 5),
                       linewidth = rep(data$linewidth, 5),
                       linetype = rep(data$linetype, 5),
                       fill = rep(data$fill, 5),
                       group = rep(seq_len(nrow(data)), 5))
  }

  middle <- trans(flip_data(middle, flipped))
  box <- trans(flip_data(box, flipped))
  middle <- cartesian2polar(middle, coord = coord, region = region,
                            clip = clip, na.rm = na.rm)
  box <- cartesian2polar(box, coord = coord, region = region,
                         clip = clip, na.rm = na.rm)

  grid::gList(exec(ArcPolygonGrob, !!!box, ...),
              exec(ArcSegmentsGrob, !!!middle, ...))
}
