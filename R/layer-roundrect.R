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
#' @inheritParams ggplot2::geom_histogram
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomRound
#' @export
GeomRoundcolArcET2grob <- function(data,
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
  data <- data[intersect(names(data), c("xmin", "xmax", "ymin", "ymax", "colour",
                                        "fill", "linewidth", "linetype", "alpha"))]

  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  data <- rect2polygon(data)
  exec(ArcShapeGrob, !!!data, ...)
}

#' @rdname GeomRound
#' @export
GeomRoundbarArcET2grob <- function(data,
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
  data <- data[intersect(names(data), c("xmin", "xmax", "ymin", "ymax", "colour",
                                        "fill", "linewidth", "linetype", "alpha"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  data <- rect2polygon(data)
  exec(ArcShapeGrob, !!!data, ...)
}

#' @rdname GeomRound
#' @export
GeomRoundrectArcET2grob <- function(data,
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
  data <- data[intersect(names(data), c("xmin", "xmax", "ymin", "ymax", "colour",
                                        "fill", "linewidth", "linetype", "alpha"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  data <- rect2polygon(data)
  exec(ArcShapeGrob, !!!data, ...)
}

#' @rdname GeomRound
#' @export
GeomRoundtileArcET2grob <- function(data,
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
  data <- data[intersect(names(data), c("xmin", "xmax", "ymin", "ymax", "colour",
                                        "fill", "linewidth", "linetype", "alpha"))]
  data <- cartesian2polar(data = data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  data <- rect2polygon(data)
  exec(ArcShapeGrob, !!!data, ...)
}

#' @rdname GeomRound
#' @export
GeomRoundpolygonArcET2grob <- function(data,
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
  exec(ArcShapeGrob, !!!data, ...)
}

#' @rdname GeomRound
#' @export
GeomShapeArcET2grob <- GeomRoundpolygonArcET2grob

#' @title Roundrect Layer
#' @description functions to drawing round rect, bar, histogram and polygon.
#' @inheritParams ggplot2::layer
#' @inheritParams ggforce::geom_shape
#' @inheritParams ggplot2::geom_histogram
#' @inheritParams ggplot2::geom_col
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @return a ggplot layer.
#' @rdname geom_round
#' @author Hou Yun
#' @export
geom_roundhistogram <- function(mapping = NULL,
                                data = NULL,
                                stat = "bin",
                                position = "identity",
                                ...,
                                binwidth = NULL,
                                bins = NULL,
                                orientation = NA,
                                radius = 0,
                                lineend = "butt",
                                linejoin = "round",
                                linemitre = 10,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundbarArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(binwidth = binwidth,
                       bins = bins,
                       orientation = orientation,
                       na.rm = na.rm,
                       pad = FALSE,
                       radius = radius,
                       lineend = lineend,
                       linejoin = linejoin,
                       linemitre = linemitre,
                       ...))
}

#' @rdname geom_round
#' @export
geom_roundpolygon <- function(mapping = NULL,
                              data = NULL,
                              stat = "identity",
                              position = "identity",
                              ...,
                              expand = 0,
                              radius = 0,
                              lineend = "butt",
                              linejoin = "round",
                              linemitre = 10,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundpolygonArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(expand = expand,
                              radius = radius,
                              lineend = lineend,
                              linejoin = linejoin,
                              linemitre = linemitre,
                              na.rm = na.rm,
                              ...))
}

#' @rdname geom_round
#' @export
geom_roundcol <- function(mapping = NULL,
                          data = NULL,
                          position = "stack",
                          ...,
                          just = 0.5,
                          width = NULL,
                          radius = 0,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 10,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = "identity",
        geom = GeomRoundcolArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(just = just,
                              width = width,
                              radius = radius,
                              lineend = lineend,
                              linejoin = linejoin,
                              linemitre = linemitre,
                              na.rm = na.rm,
                              ...))
}

#' @rdname geom_round
#' @export
geom_roundbar <- function(mapping = NULL,
                          data = NULL,
                          stat = "count",
                          position = "stack",
                          ...,
                          just = 0.5,
                          width = NULL,
                          orientation = NA,
                          radius = 0,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 10,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundbarArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(just = just,
                              width = width,
                              orientation = orientation,
                              radius = radius,
                              lineend = lineend,
                              linejoin = linejoin,
                              linemitre = linemitre,
                              na.rm = na.rm,
                              ...))
}

#' @rdname geom_round
#' @export
geom_roundtile <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           ...,
                           radius = 0,
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundtileArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(radius = radius,
                              lineend = lineend,
                              linejoin = linejoin,
                              linemitre = linemitre,
                              na.rm = na.rm,
                              ...))
}

#' @rdname geom_round
#' @export
geom_roundrect <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           ...,
                           expand = 0,
                           radius = 0,
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundrectArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(expand = expand,
                              radius = radius,
                              lineend = lineend,
                              linejoin = linejoin,
                              linemitre = linemitre,
                              na.rm = na.rm,
                              ...))
}

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomRoundpolygonArcET <- ggproto(
  "GeomRoundpolygonArcET", GeomPolygon,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", linemitre = 10, expand = 0,
                        radius = 0, arc = TRUE, steps = 0.005, simplify = TRUE) {
    if (empty(data)) {
      return(zeroGrob())
    }

    data <- check_linewidth(data, snake_class(self))
    data <- data[order(data$group), , drop = FALSE]
    data <- coord$transform(data, panel_params)

    first_rows <- data[!duplicated(data$group), , drop = FALSE]
    ggname("geom_roundpolygon", shapeGrob(x = data$x,
                                          y = data$y,
                                          id = data$group,
                                          expand = expand,
                                          radius = radius,
                                          default.units = "native",
                                          gp = gpar(col = alpha(first_rows$colour, first_rows$alpha),
                                                    fill = alpha(first_rows$fill, first_rows$alpha),
                                                    lwd = first_rows$linewidth * .pt,
                                                    lty = first_rows$linetype,
                                                    lineend = lineend,
                                                    linejoin = linejoin,
                                                    linemitre = linemitre)))
  }
)

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomRoundtileArcET <- ggproto(
  "GeomRoundbarArcET", GeomBar,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", linemitre = 10, radius = 0,
                        arc = TRUE, steps = 0.005, simplify = TRUE) {
    ggproto_parent(GeomRoundrectArcET, self)$draw_panel(data = data,
                                                        panel_params = panel_params,
                                                        coord = coord,
                                                        expand = 0,
                                                        radius = radius,
                                                        lineend = lineend,
                                                        linejoin = linejoin,
                                                        linemitre = linemitre)
  }
)

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomRoundbarArcET <- ggproto(
  "GeomRoundbarArcET", GeomBar,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", linemitre = 10, radius = 0,
                        flipped_aes = FALSE, arc = TRUE, steps = 0.005,
                        simplify = TRUE) {
    ggproto_parent(GeomRoundrectArcET, self)$draw_panel(data = data,
                                                        panel_params = panel_params,
                                                        coord = coord,
                                                        expand = 0,
                                                        radius = radius,
                                                        lineend = lineend,
                                                        linejoin = linejoin,
                                                        linemitre = linemitre)
  }
)

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomRoundcolArcET <- ggproto(
  "GeomRoundcolArcET", GeomCol,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", linemitre = 10, radius = 0,
                        flipped_aes = FALSE, arc = TRUE, steps = 0.005,
                        simplify = TRUE) {
    ggproto_parent(GeomRoundrectArcET, self)$draw_panel(data = data,
                                                        panel_params = panel_params,
                                                        coord = coord,
                                                        expand = 0,
                                                        radius = radius,
                                                        lineend = lineend,
                                                        linejoin = linejoin,
                                                        linemitre = linemitre)
  }
)

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomRoundrectArcET <- ggproto(
  "GeomRoundrectArcET", GeomRect,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", linemitre = 10, expand = 0,
                         radius = 0, arc = TRUE, steps = 0.005, simplify = TRUE) {
    if (empty(data)) {
      return(zeroGrob())
    }

    data <- check_linewidth(data, snake_class(self))
    data <- rect2polygon(data)
    data <- data[order(data$group), , drop = FALSE]
    data <- coord$transform(data, panel_params)

    first_rows <- data[!duplicated(data$group), , drop = FALSE]
    ggname("geom_roundrect", shapeGrob(x = data$x,
                                       y = data$y,
                                       id = data$group,
                                       expand = expand,
                                       radius = radius,
                                       default.units = "native",
                                       gp = gpar(col = alpha(first_rows$colour, first_rows$alpha),
                                                 fill = alpha(first_rows$fill, first_rows$alpha),
                                                 lwd = first_rows$linewidth * .pt,
                                                 lty = first_rows$linetype,
                                                 lineend = lineend,
                                                 linejoin = linejoin,
                                                 linemitre = linemitre)))
  }
)
