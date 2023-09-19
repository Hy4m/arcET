## TODO: clean and speedup
#' Arc x-axis Grob
#' @description These functions can draw x-axis on polar coordinate.
#'
#' @param title charccter specifying axis title.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param position axis position specification, and default is "bottom".
#' @param ticks.length a grid unit object or numeric, and numerical value
#' means that the length of ticks is measured in millimeters.
#' @param title.gp should be created by `ggplot2::element_text()`.
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
ArcxAxisGrob <- function(title = NULL,
                         coord = PANEL(),
                         region = CELL(),
                         position = "top",
                         ticks.length = 1.5,
                         title.gp = element_text(),
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
  if (!inherits(title.gp, "element_text") && inherits(title.gp, "element_markdown") &&
      !inherits(text.gp, "element_blank")) {
    cli::cli_abort(c("{.arg title.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_text} or {.fun element_blank}."))
  }
  if (!inherits(line.gp, "element_line") && !inherits(line.gp, "element_blank")) {
    cli::cli_abort(c("{.arg line.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(tick.gp, "element_line") && !inherits(tick.gp, "element_blank")) {
    cli::cli_abort(c("{.arg tick.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(text.gp, "element_text") && inherits(title.gp, "element_markdown") &&
      !inherits(text.gp, "element_blank")) {
    cli::cli_abort(c("{.arg text.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_text} or {.fun element_blank}."))
  }
  position <- match.arg(position, c("top", "bottom", "none"))

  if (x_is_NULLcoord(coord) || position == "none") {
    return(zeroGrob())
  }

  grid::gTree(title = title,
              coord = coord,
              region = region,
              position = position,
              ticks.length = ticks.length,
              title.gp = title.gp,
              line.gp = line.gp,
              tick.gp = tick.gp,
              text.gp = text.gp,
              steps = steps,
              cl = "ArcxAxisGrob")
}

#' Arc y-axis Grob
#' @description These functions can draw y-axis on polar coordinate.
#'
#' @param title charccter specifying axis title.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param position axis position specification, and default is "left".
#' @param ticks.length a grid unit object or numeric, and numerical value
#' means that the length of ticks is measured in millimeters.
#' @param title.gp should be created by `ggplot2::element_text()`.
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
ArcyAxisGrob <- function(title = NULL,
                         coord = PANEL(),
                         region = CELL(),
                         position = "left",
                         ticks.length = 1.5,
                         title.gp = element_text(),
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

  if (!inherits(title.gp, "element_text") && inherits(title.gp, "element_markdown") &&
      !inherits(text.gp, "element_blank")) {
    cli::cli_abort(c("{.arg title.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_text} or {.fun element_blank}."))
  }
  if (!inherits(line.gp, "element_line") && !inherits(line.gp, "element_blank")) {
    cli::cli_abort(c("{.arg line.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(tick.gp, "element_line") && !inherits(tick.gp, "element_blank")) {
    cli::cli_abort(c("{.arg tick.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_line} or {.fun element_blank}."))
  }
  if (!inherits(text.gp, "element_text") && inherits(title.gp, "element_markdown") &&
      !inherits(text.gp, "element_blank")) {
    cli::cli_abort(c("{.arg text.gp} must be an `element` object,",
                     i = "it can be created by {.fun element_text} or {.fun element_blank}."))
  }
  position <- match.arg(position, c("left", "right", "none"))

  if (y_is_NULLcoord(coord) || position == "none") {
    return(zeroGrob())
  }

  grid::gTree(title = title,
              coord = coord,
              region = region,
              position = position,
              ticks.length = ticks.length,
              title.gp = title.gp,
              line.gp = line.gp,
              tick.gp = tick.gp,
              text.gp = text.gp,
              cl = "ArcyAxisGrob")
}

#' @export
makeContent.ArcxAxisGrob <- function(x) {
  g <- .ArcxAxisGrob(title = x$title,
                     coord = x$coord,
                     region = x$region,
                     position = x$position,
                     ticks.length = x$ticks.length,
                     title.gp = x$title.gp,
                     line.gp = x$line.gp,
                     tick.gp = x$tick.gp,
                     text.gp = x$text.gp)
  grid::setChildren(x, children = g$children)
}

#' @export
makeContent.ArcyAxisGrob <- function(x) {
  g <- .ArcyAxisGrob(title = x$title,
                     coord = x$coord,
                     region = x$region,
                     position = x$position,
                     ticks.length = x$ticks.length,
                     title.gp = x$title.gp,
                     line.gp = x$line.gp,
                     tick.gp = x$tick.gp,
                     text.gp = x$text.gp)
  grid::setChildren(x, children = g$children)
}

#' @noRd
.ArcxAxisGrob <- function(title = NULL,
                          coord = PANEL(),
                          region = CELL(),
                          position = "left",
                          ticks.length = unit(2.75, "pt"),
                          title.gp = element_text(),
                          line.gp = element_line(),
                          tick.gp = element_line(),
                          text.gp = element_text(),
                          steps = 0.01,
                          ...) {
  if (!is.unit(ticks.length)) {
    ticks.length <- unit(ticks.length, "mm")
  }
  ticks.length <- convert_height(ticks.length, "native", TRUE)
  one_mm <- convert_height(unit(1, "mm"), "native", TRUE)
  r0 <- min(region$y.range)
  r1 <- max(region$y.range)
  current <- if (position == "top") r1 else r0

  has_title <- has_line <- has_tick <- has_text <- TRUE
  ## build axis line
  if (inherits(line.gp, "element_blank")) {
    has_line <- FALSE
  } else {
    line.arrow <- if (is.logical(line.gp$arrow) && !line.gp$arrow) {
      NULL
    } else {
      line.gp$arrow
    }
    line <- ArcSegmentsGrob(x = region$x.range[1],
                            y = if (position == "top") r1 else r0,
                            xend = region$x.range[2],
                            yend = if (position == "top") r1 else r0,
                            colour = line.gp$colour %||% "grey20",
                            linewidth = line.gp$linewidth %||% 0.5,
                            linetype = line.gp$linetype %||% 1,
                            lineend = line.gp$lineend %||% "butt",
                            arrow = line.arrow,
                            arc = TRUE,
                            steps = steps)
  }

  ## build axis ticks
  if (inherits(tick.gp, "element_blank") || is.null(coord$x$breaks)) {
    has_tick <- FALSE
    if (position == "top") {
      current <- current + one_mm
    } else {
      current <- current - one_mm
    }
  } else {
    xs <- scales::rescale(coord$x$breaks, to = region$x.range, from = coord$x$range)
    tick.arrow <- if (is.logical(tick.gp$arrow) && !tick.gp$arrow) {
      NULL
    } else {
      tick.gp$arrow
    }

    tick <- ArcSegmentsGrob(x = xs,
                            y = if (position == "top") r1 else r0,
                            xend = xs,
                            yend = if (position == "top") r1 + ticks.length else r0 - ticks.length,
                            colour = tick.gp$colour %||% "grey20",
                            linewidth = tick.gp$linewidth %||% 0.5,
                            linetype = tick.gp$linetype %||% 1,
                            lineend = tick.gp$lineend %||% "butt",
                            arrow = tick.arrow,
                            arc = FALSE)
    if (position == "top") {
      current <- if (ticks.length < 0) current + one_mm else current + ticks.length + one_mm
    } else {
      current <- if (ticks.length < 0) current - one_mm else current - ticks.length - one_mm
    }
  }

  ## build axis labels
  if (inherits(text.gp, "element_blank") || is.null(coord$x$breaks)) {
    has_text <- FALSE
  } else {
    xs <- scales::rescale(coord$x$breaks, to = region$x.range, from = coord$x$range)

    label <- ArcTextGrob(label = coord$x$labels,
                         x = xs,
                         y = current,
                         angle = "clockwise",
                         hjust = if (position == "top") 0 else 1,
                         vjust = 0.5,
                         colour = text.gp$colour %||% "grey30",
                         size = (text.gp$size %||% 8.8)/.pt,
                         family = text.gp$family %||% "",
                         fontface = text.gp$face %||% 1,
                         lineheight = text.gp$lineheight %||% 1.2,
                         parse = if (is.expression(coord$x$labels)) TRUE else FALSE,
                         auto_adjust = FALSE)

    gp <- gpar(fontsize = text.gp$size %||% 8.8,
               fontfamily = text.gp$family %||% "",
               fontface = text.gp$face %||% 1,
               lineheight = text.gp$lineheight %||% 1.2)
    width <- text_width(coord$x$labels, "native", gp = gp)
    current <- if (position == "top") current + max(width) + one_mm else current - max(width) - one_mm
  }

  ## build axis title
  if (inherits(title.gp, "element_blank") || is.null(title)) {
    has_title <- FALSE
  } else {
    title <- ArcBannerTextGrob(label = title,
                               x = region$mid_x,
                               y = current,
                               hjust = 0.5,
                               vjust = if (position == "top") 0 else 1,
                               colour = title.gp$colour %||% "black",
                               size = (title.gp$size %||% 11)/.pt,
                               family = title.gp$family %||% "",
                               fontface = title.gp$face %||% 1,
                               lineheight = title.gp$lineheight %||% 1.2)
  }

  grid::gTree(children = grid::gList(if (has_line) line,
                                     if (has_tick) tick,
                                     if (has_text) label,
                                     if (has_title) title),
              name = "ArcxAxisGrob")
}

#' @noRd
.ArcyAxisGrob <- function(title = NULL,
                          coord = PANEL(),
                          region = CELL(),
                          position = "left",
                          ticks.length = unit(2.75, "pt"),
                          title.gp = element_text(),
                          line.gp = element_line(),
                          tick.gp = element_line(),
                          text.gp = element_text(),
                          ...) {
  if (!is.unit(ticks.length)) {
    ticks.length <- unit(ticks.length, "mm")
  }
  ticks.length <- convert_height(ticks.length, "native", TRUE)
  angle <- if (position == "left") max(region$x.range) else min(region$x.range)
  one_mm <- convert_width(unit(1, "mm"), "native", TRUE)

  current <- 0
  has_title <- has_line <- has_tick <- has_text <- TRUE
  vp <- grid::viewport(x = 0,
                       y = 0,
                       width = unit(2, "native"),
                       height = unit(1, "native"),
                       just = c(0.5, 0),
                       angle = (angle %% 360) - 90,
                       xscale = c(-1, 1),
                       yscale = c(0, 1),
                       default.units = "native")

  ## build axis line
  if (inherits(line.gp, "element_blank")) {
    has_line <- FALSE
  } else {
    line.arrow <- if (is.logical(line.gp$arrow) && !line.gp$arrow) {
      NULL
    } else {
      line.gp$arrow
    }
    line <- grid::segmentsGrob(x0 = 0,
                               y0 = region$y.range[1],
                               x1 = 0,
                               y1 = region$y.range[2],
                               gp = gpar(col = line.gp$colour %||% "grey20",
                                         lwd = (line.gp$linewidth %||% 0.5) * .pt,
                                         lty = line.gp$linetype %||% 1,
                                         lineend = line.gp$lineend %||% "butt"),
                               arrow = line.arrow,
                               default.units = "native",
                               vp = vp)
  }

  ## build axis ticks
  if (inherits(tick.gp, "element_blank") || is.null(coord$y$breaks)) {
    has_tick <- FALSE
    if (position == "right") {
      current <- current + one_mm
    } else {
      current <- current - one_mm
    }
  } else {
    y <- scales::rescale(coord$y$breaks, to = region$y.range, from = coord$y$range)
    tick.arrow <- if (is.logical(tick.gp$arrow) && !tick.gp$arrow) {
      NULL
    } else {
      tick.gp$arrow
    }
    if (position == "left") {
      xend <- -ticks.length
    } else {
      xend <- ticks.length
    }
    tick <- grid::segmentsGrob(x0 = 0,
                               y0 = y,
                               x1 = xend,
                               y1 = y,
                               gp = gpar(col = tick.gp$colour %||% "grey20",
                                         lwd = (tick.gp$linewidth %||% 0.5) * .pt,
                                         lty = tick.gp$linetype %||% 1,
                                         lineend = tick.gp$lineend %||% "butt"),
                               arrow = tick.arrow,
                               default.units = "native",
                               vp = vp)
    if (position == "right") {
      current <- if (ticks.length < 0) current + one_mm else current + ticks.length + one_mm
    } else {
      current <- if (ticks.length < 0) current - one_mm else current - ticks.length - one_mm
    }
  }

  ## build axis labels
  if (inherits(text.gp, "element_blank") || is.null(coord$y$breaks)) {
    has_text <- FALSE
  } else {
    y <- scales::rescale(coord$y$breaks, to = region$y.range, from = coord$y$range)
    label <- grid::textGrob(label = coord$y$labels,
                            x = current,
                            y = y,
                            rot = 0,
                            hjust = if (position == "left") 1 else 0,
                            vjust = 0.5,
                            gp = gpar(col = text.gp$colour %||% "grey35",
                                      fontsize = text.gp$size %||% 8.8,
                                      fontfamily = text.gp$family %||% "",
                                      fontface = text.gp$face %||% 1,
                                      lineheight = text.gp$lineheight %||% 1.2),
                            default.units = "native",
                            vp = vp)

    gp <- gpar(fontsize = text.gp$size %||% 8.8,
               fontfamily = text.gp$family %||% "",
               fontface = text.gp$face %||% 1,
               lineheight = text.gp$lineheight %||% 1.2)
    width <- text_width(coord$y$labels, "native", gp = gp)
    current <- if (position == "right") current + max(width) + one_mm else current - max(width) - one_mm
  }

  ## build axis title
  if (inherits(title.gp, "element_blank") || is.null(title)) {
    has_title <- FALSE
  } else {
    title <- grid::textGrob(label = title,
                            x = current,
                            y = region$mid_y,
                            rot = 90,
                            hjust = 0.5,
                            vjust = if (position == "left") 0 else 1,
                            gp = gpar(col = title.gp$colour %||% "black",
                                      fontsize = title.gp$size %||% 11,
                                      fontfamily = title.gp$family %||% "",
                                      fontface = title.gp$face %||% 1,
                                      lineheight = title.gp$lineheight %||% 1.2),
                            default.units = "native",
                            vp = vp)
  }

  grid::gTree(children = grid::gList(if (has_line) line,
                                     if (has_tick) tick,
                                     if (has_text) label,
                                     if (has_title) title),
              name = "ArcyAxisGrob")
}

