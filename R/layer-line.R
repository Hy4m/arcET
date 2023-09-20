#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param flipped_aes TRUE means that coordinates are inherit `CoordFlip`.
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_step
#' @inheritParams ggplot2::geom_rug
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
  data <- data[order(data$group, data$x), , drop = FALSE]
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

  gTree(children = gList(if (has_ribbon) exec(ArcRibbonGrob, !!!ribbon, ...),
                         exec(ArcPathGrob, !!!path, ...)))
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
  exec(ArcPathGrob, !!!data, ..., arc = FALSE)
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
  exec(ArcPathGrob, !!!data, ..., arc = FALSE)
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
