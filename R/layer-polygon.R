#' @title Polygon Layer
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_polygon
#' @return a gg layer object.
#' @family layer
#' @rdname layer_polygon
#' @author Hou Yun
#' @export
layer_polygon <- function(..., show.legend = FALSE) {
  ggplot2::geom_polygon(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_rect <- function(..., show.legend = FALSE) {
  ggplot2::geom_rect(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_tile <- function(..., show.legend = FALSE) {
  ggplot2::geom_tile(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_area <- function(..., show.legend = FALSE) {
  ggplot2::geom_area(..., show.legend = show.legend)
}

#' @export
layer_ribbon <- function(..., show.legend = FALSE) {
  ggplot2::geom_ribbon(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_contour_filled <- function(..., show.legend = FALSE) {
  ggplot2::geom_contour_filled(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_density <- function(..., show.legend = FALSE) {
  ggplot2::geom_density(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_density2d_filled <- function(..., show.legend = FALSE) {
  ggplot2::geom_density2d_filled(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_density_2d_filled <- function(..., show.legend = FALSE) {
  ggplot2::geom_density2d_filled(..., show.legend = show.legend)
}

#' @rdname layer_polygon
#' @export
layer_freqpoly <- function(..., show.legend = FALSE) {
  ggplot2::geom_freqpoly(..., show.legend = show.legend)
}

#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param flipped_aes TRUE means that coordinates are inherit `CoordFlip`.
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_ribbon
#' @inheritParams ArcPolygonGrob
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomPolygon
#' @export
GeomPolygon2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "fill", "linetype",
                                        "linewidth", "alpha", "group", "subgroup"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPolygonGrob, !!!data, ...)
}

#' @rdname GeomPolygon
#' @export
GeomRect2grob <- function(data,
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
  data <- data[intersect(names(data), c("xmin", "ymin", "xmax", "ymax",
                                        "colour", "fill", "linetype",
                                        "linewidth", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcRectGrob, !!!data, ...)
}

#' @rdname GeomPolygon
#' @export
GeomTile2grob <- function(data,
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
  data <- data[intersect(names(data), c("xmin", "ymin", "xmax", "ymax",
                                        "colour", "fill", "linetype",
                                        "linewidth", "alpha"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = FALSE, na.rm = na.rm)
  exec(ArcRectGrob, !!!data, ...)
}

#' @rdname GeomPolygon
#' @export
GeomArea2grob <- function(data,
                          trans = NULL,
                          coord = PANEL(),
                          region = CELL(),
                          ...,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 10,
                          outline.type = "both",
                          flipped_aes = FALSE,
                          clip = FALSE,
                          na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- data[count_by_group(data) >= 2, , drop = FALSE]
  if (empty(data)) {
    return(zeroGrob)
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
  data <- ribbon_data(data = data, flipped_aes = flipped, outline.type = outline.type)
  poly <- cartesian2polar(data$poly, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  line <- cartesian2polar(data$line, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  g_poly <- exec(ArcPolygonGrob, !!!poly, ..., lineend = lineend,
                 linejoin = linejoin, linemitre = linemitre)

  if (!empty(line)) {
    g_line <- exec(ArcPathGrob, !!!line, ..., lineend = lineend,
                   linejoin = linejoin, linemitre = linemitre)
  } else {
    g_line <- NULL
  }

  grid::gList(g_poly, g_line)
}


#' @rdname GeomPolygon
#' @export
GeomRibbon2grob <- function(data,
                            trans = NULL,
                            coord = PANEL(),
                            region = CELL(),
                            ...,
                            flipped_aes = FALSE,
                            lineend = "butt",
                            linejoin = "round",
                            linemitre = 10,
                            outline.type = "both",
                            clip = FALSE,
                            na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  data <- data[count_by_group(data) >= 2, , drop = FALSE]
  if (empty(data)) {
    return(zeroGrob)
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
  data <- ribbon_data(data = data, flipped_aes = flipped, outline.type = outline.type)
  poly <- cartesian2polar(data$poly, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  line <- cartesian2polar(data$line, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  g_poly <- exec(ArcPolygonGrob, !!!poly, ..., lineend = lineend,
                 linejoin = linejoin, linemitre = linemitre)

  if (!empty(line)) {
    g_line <- exec(ArcPathGrob, !!!line, ..., lineend = lineend,
                   linejoin = linejoin, linemitre = linemitre)
  } else {
    g_line <- NULL
  }

  grid::gList(g_poly, g_line)
}

#' @rdname GeomPolygon
#' @export
GeomDensity2grob <- function(data,
                             trans = NULL,
                             coord = PANEL(),
                             region = CELL(),
                             ...,
                             lineend = "butt",
                             linejoin = "round",
                             linemitre = 10,
                             outline.type = "upper",
                             flipped_aes = FALSE,
                             clip = FALSE,
                             na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }
  data <- data[count_by_group(data) >= 2, , drop = FALSE]
  if (empty(data)) {
    return(zeroGrob)
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
  data <- ribbon_data(data = data, flipped_aes = flipped, outline.type = outline.type)
  poly <- cartesian2polar(data$poly, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  line <- cartesian2polar(data$line, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  g_poly <- exec(ArcPolygonGrob, !!!poly, ..., lineend = lineend,
                 linejoin = linejoin, linemitre = linemitre)

  if (!empty(line)) {
    g_line <- exec(ArcPathGrob, !!!line, ..., lineend = lineend,
                   linejoin = linejoin, linemitre = linemitre)
  } else {
    g_line <- NULL
  }

  grid::gList(g_poly, g_line)
}

#' @rdname GeomPolygon
#' @export
GeomContourFilled2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "fill", "linetype",
                                        "linewidth", "alpha", "group",
                                        "subgroup"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPolygonGrob, !!!data, ...)
}

#' @rdname GeomPolygon
#' @export
GeomDensity2dFilled2grob <- function(data,
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
  data <- data[intersect(names(data), c("x", "y", "colour", "fill", "linetype",
                                        "linewidth", "alpha", "group",
                                        "subgroup"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPolygonGrob, !!!data, ...)
}

#' @noRd
ribbon_data <- function(data,
                        flipped_aes = FALSE,
                        outline.type = "both") {
  if (empty(data)) {
    return(data)
  }

  poly <- line <- NULL
  for (df in split(data, data$group)) {
    aes <- df[1, intersect(names(df), c("fill", "colour", "linewidth",
                                        "linetype", "alpha"))]
    df <- unclass(df)
    keep <- if (is.null(df$align_padding)) TRUE else !df$align_padding

    upper <- data_frame0(x = df$x[keep],
                         y = df$ymax[keep],
                         group = df$group[1],
                         !!!aes)
    lower <- data_frame0(x = rev(df$x),
                         y = rev(df$ymin),
                         group = df$group[1],
                         !!!aes)

    p <- vec_rbind0(upper, lower, upper[1, , drop = FALSE])
    poly <- vec_rbind0(poly, p)

    if (identical(outline.type, "full")) {
      line <- vec_rbind0(line, p)
    } else if (identical(outline.type, "both")) {
      l <- vec_rbind0(upper, lower)
      l$group <- c(paste0(upper$group, "upper"), paste0(lower$group, "lower"))
      line <- vec_rbind0(line, l)
    } else if (identical(outline.type, "upper")) {
      line <- vec_rbind0(line, upper)
    } else if (identical(outline.type, "lower")) {
      line <- vec_rbind0(line, lower)
    }
  }

  poly <- transform(poly, colour = NA, linewidth = 0, linetype = 1)
  poly <- flip_data(poly, flipped_aes)
  if (!empty(line)) {
    line <- transform(line, alpha = NA)
    line$group <- as.integer(factor(line$group))
    line <- flip_data(line, flipped_aes)
  }

  list(poly = poly, line = line)
}
