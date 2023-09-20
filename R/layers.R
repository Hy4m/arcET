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
#' @inheritParams ggplot2::geom_step
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomPathArc
#' @export
GeomPathArcET2grob <- function(data,
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

#' @rdname GeomPathArc
#' @export
GeomLineArcET2grob <- function(data,
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

#' @rdname GeomPathArc
#' @export
GeomSegmentArcET2grob <- function(data,
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

  data <- segments2path(data)
  data <- trans(data)
  data <- data[intersect(names(data), c("x", "y", "colour", "linetype",
                                        "linewidth", "alpha", "group"))]
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcPathGrob, !!!data, ...)
}

#' @rdname GeomPathArc
#' @export
GeomStepArcET2grob <- function(data,
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


#' Arc path-like layer
#' @description All `geom_*_arc()` functions arc same as `geom_*()` in `ggplot2`
#' package. However, the `geom_*_arc()` functions can set `arc`, `steps` and `simplify`
#' parameter, which can used to adjust the curve smoothness after conversion to ArcPlot.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::geom_step
#'
#' @return a gg layer object.
#' @author Hou Yun
#' @rdname geom_path_arc
#' @export
geom_path_arc <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 10,
                          arrow = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPathArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(lineend = lineend,
                       linejoin = linejoin,
                       linemitre = linemitre,
                       arrow = arrow,
                       na.rm = na.rm,
                       ...))
}

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomPathArcET <- ggproto(
  "GeomPathArcET", GeomPath,
  draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                         lineend = "butt", linejoin = "round", linemitre = 10,
                         arc = TRUE, steps = 0.005, simplify = TRUE, na.rm = FALSE) {
    ggproto_parent(GeomPath, self)$draw_panel(data = data,
                                              panel_params = panel_params,
                                              coord = coord,
                                              arrow = arrow,
                                              lineend = lineend,
                                              linejoin = linejoin,
                                              linemitre = linemitre,
                                              na.rm = na.rm)
  }
)

#' @rdname geom_path_arc
#' @export
geom_line_arc <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPathArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(orientation = orientation,
                       na.rm = na.rm,
                       ...))
}

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomLineArcET <- ggproto(
  "GeomLineArcET", GeomLine,
  draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                         lineend = "butt", linejoin = "round", linemitre = 10,
                         arc = TRUE, steps = 0.005, simplify = TRUE, na.rm = FALSE) {
    ggproto_parent(GeomLine, self)$draw_panel(data = data,
                                              panel_params = panel_params,
                                              coord = coord,
                                              arrow = arrow,
                                              lineend = lineend,
                                              linejoin = linejoin,
                                              linemitre = linemitre,
                                              na.rm = na.rm)
  }
)

#' @rdname geom_path_arc
#' @export
geom_segment_arc <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             ...,
                             arrow = NULL,
                             arrow.fill = NULL,
                             lineend = "butt",
                             linejoin = "round",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSegmentArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(arrow = arrow,
                       arrow.fill = arrow.fill,
                       lineend = lineend,
                       linejoin = linejoin,
                       na.rm = na.rm,
                       ...))
}

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomSegmentArcET <- ggproto(
  "GeomSegmentArcET", GeomSegment,
  draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                         arrow.fill = NULL, lineend = "butt", linejoin = "round",
                         linemitre = 10, arc = TRUE, steps = 0.005, simplify = TRUE,
                         na.rm = FALSE) {
    ggproto_parent(GeomSegment, self)$draw_panel(data = data,
                                                 panel_params = panel_params,
                                                 coord = coord,
                                                 arrow = arrow,
                                                 arrow.fill = arrow.fill,
                                                 lineend = lineend,
                                                 linejoin = linejoin,
                                                 linemitre = linemitre,
                                                 na.rm = na.rm)
  }
)

#' @rdname geom_path_arc
#' @export
geom_step_arc <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          direction = "hv",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomStepArcET,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(direction = direction,
                       na.rm = na.rm,
                       ...))
}

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomStepArcET <- ggproto(
  "GeomStepArcET", GeomStep,
  draw_panel = function (self, data, panel_params, coord, direction = "hv",
                         lineend = "butt", linejoin = "round", linemitre = 10,
                         arc = TRUE, steps = 0.005, simplify = TRUE, na.rm = FALSE) {
    ggproto_parent(GeomStep, self)$draw_panel(data = data,
                                              panel_params = panel_params,
                                              coord = coord,
                                              direction = direction)
  }
)

