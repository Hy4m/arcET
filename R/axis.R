#' Arc x-axis Grob
#' @description These functions can draw x-axis on polar coordinate.
#'
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param position axis position specification, and default is "bottom".
#' @param ticks.length a grid unit object or numeric, and numerical value
#' means that the length of ticks is measured in millimeters.
#' @param line.gp should be created by `ggplot2::element_line()`.
#' @param tick.gp should be created by `ggplot2::element_line()`.
#' @param text.gp should be created by `ggplot2::element_text()`.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param steps step length used to split data. A smaller value means a
#' smoother curve.
#' @param ... not used.
#' @return a `xAxisGrob` object.
#' @rdname ArcxAxisGrob
#' @author Hou Yun
#' @export
ArcxAxisGrob <- function(coord = PANEL(),
                         region = CELL(),
                         position = "top",
                         ticks.length = 1.5,
                         line.gp = element_line(),
                         tick.gp = element_line(),
                         text.gp = element_text(),
                         steps = 0.01,
                         ...) {
  if (!is_PANEL(coord)) {
    cli::cli_abort(c("{.arg coord} must be an `PANEL` object,",
                     i = "it can be created by {.fun PANEL}."))
  }
  if (!is_CELL(region)) {
    cli::cli_abort(c("{.arg region} must be an `CELL` object,",
                     i = "it can be created by {.fun CELL}."))
  }
  if (!inherits(line.gp, "element_line") && !inherits(line.gp, "element_blank")) {
    cli::cli_abort(c("{.arg line.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(tick.gp, "element_line") && !inherits(tick.gp, "element_blank")) {
    cli::cli_abort(c("{.arg tick.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(text.gp, "element_text") && !inherits(text.gp, "element_blank")) {
    cli::cli_abort(c("{.arg text.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_text} or {.fun element_blank}."))
  }
  position <- match.arg(position, c("top", "bottom", "none"))

  if (x_is_NULLcoord(coord) || position == "none") {
    return(zeroGrob())
  }

  grid::gTree(coord = coord,
              region = region,
              position = position,
              ticks.length = ticks.length,
              line.gp = line.gp,
              tick.gp = tick.gp,
              text.gp = text.gp,
              steps = steps,
              cl = "ArcxAxisGrob")
}

#' Arc y-axis Grob
#' @description These functions can draw y-axis on polar coordinate.
#'
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param position axis position specification, and default is "left".
#' @param ticks.length a grid unit object or numeric, and numerical value
#' means that the length of ticks is measured in millimeters.
#' @param line.gp should be created by `ggplot2::element_line()`.
#' @param tick.gp should be created by `ggplot2::element_line()`.
#' @param text.gp should be created by `ggplot2::element_text()`.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param ... not used.
#' @return a `yAxisGrob` object.
#' @rdname ArcyAxisGrob
#' @author Hou Yun
#' @export
ArcyAxisGrob <- function(coord = PANEL(),
                         region = CELL(),
                         position = "left",
                         ticks.length = 1.5,
                         line.gp = element_line(),
                         tick.gp = element_line(),
                         text.gp = element_text(),
                         ...) {
  if (!is_PANEL(coord)) {
    cli::cli_abort(c("{.arg coord} must be an `PANEL` object,",
                     i = "it can be created by {.fun PANEL}."))
  }
  if (!is_CELL(region)) {
    cli::cli_abort(c("{.arg region} must be an `CELL` object,",
                     i = "it can be created by {.fun CELL}."))
  }
  if (!inherits(line.gp, "element_line") && !inherits(line.gp, "element_blank")) {
    cli::cli_abort(c("{.arg line.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(tick.gp, "element_line") && !inherits(tick.gp, "element_blank")) {
    cli::cli_abort(c("{.arg tick.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(text.gp, "element_text") && !inherits(text.gp, "element_blank")) {
    cli::cli_abort(c("{.arg text.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_text} or {.fun element_blank}."))
  }
  position <- match.arg(position, c("left", "right", "none"))

  if (y_is_NULLcoord(coord) || position == "none") {
    return(zeroGrob())
  }

  grid::gTree(coord = coord,
              region = region,
              position = position,
              ticks.length = ticks.length,
              line.gp = line.gp,
              tick.gp = tick.gp,
              text.gp = text.gp,
              cl = "ArcyAxisGrob")
}

#' @export
makeContent.ArcxAxisGrob <- function(x) {
  coord <- x$coord
  region <- x$region
  position <- x$position
  ticks.length <- x$ticks.length
  line.gp <- x$line.gp
  tick.gp <- x$tick.gp
  text.gp <- x$text.gp
  steps <- x$steps

  has_line <- has_tick <- has_text <- TRUE

  if (inherits(line.gp, "element_blank")) {
    has_line <- FALSE
  } else {
    line <- ArcSegmentsGrob(x = region$x.range[1],
                            y = if (position == "top") max(region$y.range) else min(region$y.range),
                            xend = region$x.range[2],
                            yend = if (position == "top") max(region$y.range) else min(region$y.range),
                            colour = line.gp$colour %||% "black",
                            linewidth = line.gp$linewidth %||% 0.5,
                            linetype = line.gp$linetype %||% 1,
                            lineend = line.gp$lineend %||% "butt",
                            arrow = if (line.gp$arrow) grid::arrow() else NULL,
                            steps = steps
    )
  }

  if (inherits(tick.gp, "element_blank") || is.null(coord$x$breaks)) {
    has_tick <- FALSE
    ticks.length <- 0
  } else {
    if (!is.unit(ticks.length)) {
      ticks.length <- unit(ticks.length, "mm")
    }
    ticks.length <- convert_height(ticks.length, "native", TRUE)
    tick <- cartesian2polar(data = data_frame0(x = coord$x$breaks,
                                               xend = coord$x$breaks),
                            coord = coord,
                            region = region,
                            clip = FALSE,
                            na.rm = TRUE)
    if (position == "top") {
      tick$y <- max(region$y.range)
      tick$yend <- max(region$y.range) + ticks.length
    } else {
      tick$y <- min(region$y.range)
      tick$yend <- min(region$y.range) - ticks.length
    }
    tick <- ArcSegmentsGrob(x = tick$x,
                            y = tick$y,
                            xend = tick$xend,
                            yend = tick$yend,
                            colour = tick.gp$colour %||% "black",
                            linewidth = tick.gp$linewidth %||% 0.5,
                            linetype = tick.gp$linetype %||% 1,
                            lineend = tick.gp$lineend %||% "butt",
                            arrow = if (tick.gp$arrow) grid::arrow() else NULL,
                            steps = steps
    )
  }

  if (inherits(text.gp, "element_blank") || is.null(coord$x$breaks)) {
    has_text <- FALSE
  } else {
    one_mm <- convert_height(unit(1, "mm"), "native", TRUE)
    label <- cartesian2polar(data = data_frame0(x = coord$x$breaks,
                                                label = coord$x$labels),
                             coord = coord,
                             region = region,
                             clip = FALSE,
                             na.rm = TRUE)
    if (position == "top") {
      if (ticks.length <= 0){
        label$y <- max(region$y.range) + one_mm
      } else {
        label$y <- max(region$y.range) + ticks.length + one_mm
      }
      label$angle <- text_angle(label$x, "clockwise")
      label$hjust <- 0
      label$vjust <- 0.5
    } else {
      if (ticks.length <= 0){
        label$y <- min(region$y.range) - one_mm
      } else {
        label$y <- min(region$y.range) - ticks.length - one_mm
      }
      label$angle <- text_angle(label$x, "clockwise")
      label$hjust <- 1
      label$vjust <- 0.5
    }

    label <- ArcTextGrob(label = label$label,
                         x = label$x,
                         y = label$y,
                         angle = label$angle,
                         hjust = label$hjust,
                         vjust = label$vjust,
                         colour = text.gp$colour %||% "grey35",
                         size = text.gp$size %||% 3,
                         family = text.gp$family %||% "",
                         fontface = text.gp$face %||% 1,
                         lineheight = text.gp$lineheight %||% 1.2,
                         parse = if (is.expression(label$label)) TRUE else FALSE,
                         auto_adjust = FALSE)
  }

  grid::setChildren(x, grid::gList(if (has_line) line,
                                   if (has_tick) tick,
                                   if (has_text) label))
}

#' @export
makeContent.ArcyAxisGrob <- function(x) {
  coord <- x$coord
  region <- x$region
  position <- x$position
  ticks.length <- x$ticks.length
  line.gp <- x$line.gp
  tick.gp <- x$tick.gp
  text.gp <- x$text.gp

  has_line <- has_tick <- has_text <- TRUE
  angle <- if (position == "left") max(region$x.range) else min(region$x.range)
  vp <- grid::viewport(x = 0,
                       y = 0,
                       width = unit(2, "native"),
                       height = unit(1, "native"),
                       just = c(0.5, 0),
                       angle = (angle %% 360) - 90,
                       xscale = c(-1, 1),
                       yscale = c(0, 1),
                       default.units = "native")

  if (inherits(line.gp, "element_blank")) {
    has_line <- FALSE
  } else {
    line <- grid::segmentsGrob(x0 = 0,
                               y0 = region$y.range[1],
                               x1 = 0,
                               y1 = region$y.range[2],
                               gp = gpar(col = line.gp$colour %||% "black",
                                         lwd = (line.gp$linewidth %||% 0.5) * .pt,
                                         lty = line.gp$linetype %||% 1,
                                         lineend = line.gp$lineend %||% "butt"),
                               arrow = if (line.gp$arrow) grid::arrow() else NULL,
                               default.units = "native",
                               vp = vp)
  }

  if (inherits(tick.gp, "element_blank") || is.null(coord$y$breaks)) {
    has_tick <- FALSE
    ticks.length <- 0
  } else {
    if (!is.unit(ticks.length)) {
      ticks.length <- unit(ticks.length, "mm")
    }
    ticks.length <- convert_width(ticks.length, "native", TRUE)

    y <- cartesian2polar(data = data_frame0(y = coord$y$breaks),
                         coord = coord,
                         region = region,
                         clip = FALSE,
                         na.rm = TRUE)$y
    if (position == "left") {
      xend <- -ticks.length
    } else {
      xend <- ticks.length
    }
    tick <- grid::segmentsGrob(x0 = 0,
                               y0 = y,
                               x1 = xend,
                               y1 = y,
                               gp = gpar(col = tick.gp$colour %||% "black",
                                         lwd = (tick.gp$linewidth %||% 0.5) * .pt,
                                         lty = tick.gp$linetype %||% 1,
                                         lineend = tick.gp$lineend %||% "butt"),
                               arrow = if (line.gp$arrow) grid::arrow() else NULL,
                               default.units = "native",
                               vp = vp)
  }

  if (inherits(text.gp, "element_blank") || is.null(coord$y$breaks)) {
    has_text <- FALSE
  } else {
    one_mm <- convert_width(unit(1, "mm"), "native", TRUE)
    y <- cartesian2polar(data = data_frame0(y = coord$y$breaks),
                         coord = coord,
                         region = region,
                         clip = FALSE,
                         na.rm = TRUE)$y
    if (position == "left") {
      if (ticks.length <= 0){
        xend <- -one_mm
      } else {
        xend <- -ticks.length - one_mm
      }

      hjust <- 1
    } else {
      if (ticks.length <= 0){
        xend <- one_mm
      } else {
        xend <- ticks.length + one_mm
      }

      hjust <- 0
    }

    label <- grid::textGrob(label = coord$y$labels,
                            x = xend,
                            y = y,
                            rot = 0,
                            hjust = hjust,
                            vjust = 0.5,
                            gp = gpar(col = text.gp$colour %||% "grey35",
                                      fontsize = (text.gp$size %||% 3) * .pt,
                                      fontfamily = text.gp$family %||% "",
                                      fontface = text.gp$face %||% 1,
                                      lineheight = text.gp$lineheight %||% 1.2),
                            default.units = "native",
                            vp = vp)
  }

  grid::setChildren(x, grid::gList(if (has_line) line,
                                   if (has_tick) tick,
                                   if (has_text) label))
}
