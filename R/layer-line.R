#' @title Path Layer
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_path
#' @return a gg layer object.
#' @family layer
#' @rdname layer_path
#' @author Hou Yun
#' @export
layer_path <- function(..., show.legend = FALSE) {
  ggplot2::geom_path(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_line <- function(..., show.legend = FALSE) {
  ggplot2::geom_line(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_segment <- function(..., show.legend = FALSE) {
  ggplot2::geom_segment(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_hline <- function(..., show.legend = FALSE) {
  ggplot2::geom_hline(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_vline <- function(..., show.legend = FALSE) {
  ggplot2::geom_vline(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_rug <- function(..., show.legend = FALSE) {
  ggplot2::geom_rug(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_abline <- function(..., show.legend = FALSE) {
  ggplot2::geom_abline(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_spoke <- function(..., show.legend = FALSE) {
  ggplot2::geom_spoke(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_smooth <- function(..., show.legend = FALSE) {
  ggplot2::geom_smooth(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_step <- function(..., show.legend = FALSE) {
  ggplot2::geom_step(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_contour <- function(..., show.legend = FALSE) {
  ggplot2::geom_contour(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_density_2d <- function(..., show.legend = FALSE) {
  ggplot2::geom_density_2d(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_density2d <- layer_density_2d

#' @rdname layer_path
#' @export
layer_qq_line <- function(..., show.legend = FALSE) {
  ggplot2::geom_qq_line(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_quantile <- function(..., show.legend = FALSE) {
  ggplot2::geom_quantile(..., show.legend = show.legend)
}

#' @rdname layer_path
#' @export
layer_function <- function(..., show.legend = FALSE) {
  ggplot2::geom_function(..., show.legend = show.legend)
}

#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param flipped_aes TRUE means that coordinates are inherit `CoordFlip`.
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_step
#' @inheritParams ArcPathGrob
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomPath
#' @export
GeomPath2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linewidth", "alpha", "group"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomLine2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linewidth", "alpha", "group"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomSegment2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "xend", "yend", "colour",
                                        "linetype", "linewidth", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcSegmentsGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomSpoke2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "xend", "yend", "colour",
                                        "linetype", "linewidth", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcSegmentsGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomHline2grob <- function(data,
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

  data$y <- data$yintercept
  data$yend <- data$yintercept
  data <- trans(data)

  if (isTRUE(flipped_aes)) {
    data$y <- 0
    data$yend <- 1
  } else {
    data$x <- 0
    data$xend <- 1
  }

  data <- data[intersect(names(data), c("x", "y", "xend", "yend", "colour",
                                        "linetype", "linewidth", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcSegmentsGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomVline2grob <- function(data,
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

  data$x <- data$xintercept
  data$xend <- data$xintercept
  data <- trans(data)

  if (isTRUE(flipped_aes)) {
    data$x <- 0
    data$xend <- 1
  } else {
    data$y <- 0
    data$yend <- 1
  }
  data <- data[intersect(names(data), c("x", "y", "xend", "yend", "colour",
                                        "linetype", "linewidth", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcSegmentsGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomRug2grob <- function(data,
                         trans = NULL,
                         coord = PANEL(),
                         region = CELL(),
                         ...,
                         sides = "bl",
                         clip = FALSE,
                         na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linewidth", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcRugGrob, !!!data, sides = sides, region = region, ...)
}

#' @rdname GeomPath
#' @export
GeomAbline2grob <- function(data,
                            trans = NULL,
                            coord = PANEL(),
                            region = CELL(),
                            ...,
                            flipped_aes = FALSE,
                            steps = 0.01,
                            simplify = TRUE,
                            clip = FALSE,
                            na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob())
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  if (isTRUE(flipped_aes)) {
    data$x <- coord$y$data.range[1]
    data$xend <- coord$y$data.range[2]
  } else {
    data$x <- coord$x$data.range[1]
    data$xend <- coord$x$data.range[2]
  }
  data$y <- data$x * data$slope + data$intercept
  data$yend <- data$xend * data$slope + data$intercept

  data <- trans(data)
  data <- data[intersect(names(data), c("x", "y", "xend", "yend", "colour",
                                        "linetype", "linewidth", "alpha"))]
  data <- segments2path(data)
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = FALSE, na.rm = na.rm)
  data <- pieces_data(data = data, steps = steps, simplify = simplify)
  if (isTRUE(clip)) {
    data <- region$clip(data = data)
  }
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomSmooth2grob <- function(data,
                            trans = NULL,
                            coord = PANEL(),
                            region = CELL(),
                            se = TRUE,
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
  data <- data[intersect(names(data), c("x", "y", "xmin", "ymin", "xmax", "ymax",
                                        "fill","colour", "linetype", "linewidth",
                                        "alpha", "group"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  if (isFALSE(se)) {
    data$alpha <- NA
    return(exec(ArcPathGrob, !!!data, ...))
  }

  if (isTRUE(flipped_aes)) {
    ribbon <- data[intersect(names(data), c("y", "xmin", "xmax", "colour",
                                            "fill", "linetype", "linewidth",
                                            "alpha", "group"))]
    has_ribbon <- all(c("y", "xmin", "xmax") %in% names(ribbon))
  } else {
    ribbon <- data[intersect(names(data), c("x", "ymin", "ymax", "colour",
                                            "fill", "linetype", "linewidth",
                                            "alpha", "group"))]
    has_ribbon <- all(c("x", "ymin", "ymax") %in% names(ribbon))
  }

  ribbon <- transform(ribbon, colour = NA)
  path <- transform(data, alpha = NA)
  grid::gList(if (has_ribbon) exec(ArcRibbonGrob, !!!ribbon, ...),
              exec(ArcPathGrob, !!!path, ...))
}

#' @rdname GeomPath
#' @export
GeomContour2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linewidth", "alpha", "group"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomDensity2d2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linetype", "alpha", "group"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomQuantile2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linetype", "alpha", "group"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomStep2grob <- function(data,
                          trans = NULL,
                          coord = PANEL(),
                          region = CELL(),
                          ...,
                          flipped_aes = FALSE,
                          direction = "hv",
                          clip = FALSE,
                          na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob())
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- trans(data)
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linetype", "alpha", "group"))]
  data <- flip_data(data, flipped_aes)
  data <- stairstep(data = data, direction = direction)
  data <- flip_data(data, flipped_aes)

  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPath
#' @export
GeomFunction2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linetype", "alpha", "group"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}
