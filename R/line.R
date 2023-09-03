#' Arc Path Grob
#'
#' @description These functions can draw path on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of line.
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param group used to separate locations in x and y into sub-paths.
#' @param arc logical, if TRUE will split each path in small pieces.
#' @param steps step length used to split data. A smaller value means a
#' smoother curve.
#' @param simplify logical, When TRUE, line segments equal to x will not be split.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcPathGrob
#' @author Hou Yun
#' @importFrom vctrs vec_unique
#' @export
ArcPathGrob <- function(x = c(0, 20, 70),
                        y = c(0.2, 0.5, 1),
                        colour = "black",
                        linewidth = 0.5,
                        linetype = 1,
                        alpha = NA,
                        group = 1L,
                        arrow = NULL,
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
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha,
                      group = group)

  ## modify from GeomPath$draw_panel
  if (!anyDuplicated(data$group)) {
    cli::cli_inform(c("{.fn {ArcPathGrob}}: Each group consists of only one observation.",
                      i = "Do you need to adjust the {.field group} parameter?"))
  }
  data <- data[count_by_group(data) >= 2, , drop = FALSE]
  if (nrow(data) < 2) {
    return(zeroGrob())
  }

  if (isTRUE(arc)) {
    data <- pieces_data(data, steps = steps, simplify = simplify)
  }
  data <- polar2cartesian(data)
  data <- data[order(data$group), , drop = FALSE]

  aes <- c("alpha", "colour", "linewidth", "linetype")
  attr <- lapply_dfr(split(data, data$group), function(df) {
    linetype <- vec_unique(df$linetype)
    data_frame0(solid = identical(linetype, 1) || identical(linetype, "solid"),
                constant = nrow(vec_unique(df[, names(df) %in% aes])) == 1)
  })
  solid_lines <- all(attr$solid)
  constant <- all(attr$constant)
  if (!solid_lines && !constant) {
    cli::cli_abort("{.fn {ArcPathGrob}} can't have varying {.field colour}, {.field linewidth}, and/or {.field alpha} along the line when {.field linetype} isn't solid")
  }
  n <- nrow(data)
  group_diff <- data$group[-1] != data$group[-n]
  start <- c(TRUE, group_diff)
  end <- c(group_diff, TRUE)
  if (!constant) {
    segmentsGrob(x0 = data$x[!end],
                 y0 = data$y[!end],
                 x1 = data$x[!start],
                 y1 = data$y[!start],
                 default.units = "native",
                 arrow = arrow,
                 gp = gpar(col = alpha(data$colour, data$alpha)[!end],
                           fill = alpha(data$colour, data$alpha)[!end],
                           lwd = data$linewidth[!end] * .pt,
                           lty = data$linetype[!end],
                           lineend = lineend,
                           linejoin = linejoin,
                           linemitre = linemitre))
  } else {
    polylineGrob(x = data$x,
                 y = data$y,
                 id = match(data$group, vec_unique(data$group)),
                 default.units = "native",
                 arrow = arrow,
                 gp = gpar(col = alpha(data$colour, data$alpha)[start],
                           fill = alpha(data$colour, data$alpha)[start],
                           lwd = data$linewidth[start] * .pt,
                           lty = data$linetype[start],
                           lineend = lineend,
                           linejoin = linejoin,
                           linemitre = linemitre))
  }
}

#' @rdname ArcPathGrob
#' @export
ArcLineGrob <- function(...) {
  ArcPathGrob(...)
}

#' Arc Path Grob
#'
#' @description These functions can draw segments on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values of start point.
#' @param y positive numeric vector (in radius) specifying y-values of start point.
#' @param xend numeric vector (in degree) specifying x-values of end point.
#' @param yend positive numeric vector (in radius) specifying y-values of end point.
#' @param colour color of lines.
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param arc logical, if TRUE will split each path in small pieces.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcSegmentsGrob
#' @author Hou Yun
#' @export
ArcSegmentsGrob <- function(x = 30,
                            y = 0.4,
                            xend = 150,
                            yend = 0.4,
                            colour = "black",
                            linewidth = 0.5,
                            linetype = 1,
                            alpha = NA,
                            arrow = NULL,
                            lineend = "butt",
                            linejoin = "round",
                            linemitre = 10,
                            arc = TRUE,
                            steps = 0.01,
                            simplify = FALSE,
                            ...) {
  data <- data_frame0(x = x,
                      y = y,
                      xend = xend,
                      yend = yend,
                      colour = colour,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)
  data <- segments2path(data)

  exec(ArcPathGrob, !!!data,
       arrow = arrow,
       lineend = lineend,
       linejoin = linejoin,
       linemitre = linemitre,
       arc = arc,
       steps = steps,
       simplify = simplify,
       ...)
}

#' Arc Horizontal and Vertical Line
#' @description `ArcHlineGrob()` can be used to draw horizontal line, and
#' `ArcVlineGrob()` can be used to draw vertical line.
#'
#' @param xintercept intercept on the x axis.
#' @param yintercept intercept on the y axis.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the panel area.
#' @param ... other parameters passing to `ArcSegmentsGrob()`.
#' @return a grob object.
#' @rdname ArcHlineGrob
#' @author Hou Yun
#' @export
ArcHlineGrob <- function(yintercept = 0.5,
                         region = CELL(),
                         ...) {
  ids <- yintercept >= min(region$y.range) & yintercept <= max(region$y.range)
  if (!any(ids)) {
    return(zeroGrob())
  }

  yintercept <- yintercept[ids]
  ArcSegmentsGrob(x = region$x.range[1],
                  y = yintercept,
                  xend = region$x.range[2],
                  yend = yintercept,
                  ...)
}

#' @rdname ArcHlineGrob
#' @export
ArcVlineGrob <- function(xintercept = 90,
                         region = CELL(),
                         ...) {
  df <- data_frame0(x = xintercept)
  df <- region$clip(df)
  if (empty(df)) {
    return(zeroGrob())
  }

  ArcSegmentsGrob(x = df$x,
                  y = region$r0,
                  xend = df$x,
                  yend = region$r1,
                  ...)
}

#' Arc Steps Grob
#'
#' @description These functions can draw steps on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of lines.
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @param group used to separate locations in x and y into sub-paths.
#' @inheritParams ggplot2::geom_step
#' @param ... other parameters passing to `ArcPathGrob()`.
#' @return a grob object.
#' @rdname ArcStepsGrob
#' @author Hou Yun
#' @export
ArcStepsGrob <- function(x = c(0, 20, 70),
                         y = c(0.2, 0.5, 1),
                         colour = "black",
                         linewidth = 0.5,
                         linetype = 1,
                         alpha = NA,
                         group = 1L,
                         direction = "hv",
                         ...) {
  data <- data_frame0(x = x,
                      y = y,
                      colour = colour,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha,
                      group = group)
  if (nrow(data) <= 1) {
    return(zeroGrob)
  }

  data <- lapply_dfr(split(data, data$group), steps2path, direction = direction)
  ArcPathGrob(x = data$x,
              y = data$y,
              colour = data$colour,
              linewidth = data$linewidth,
              linetype = data$linetype,
              alpha = data$alpha,
              group = data$group,
              ...)
}

#' Arc Rug Grob
#'
#' @description These functions can draw rugs on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of lines.
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @inheritParams ggplot2::geom_rug
#' @param region a CELL object (created by `CELL()` function) used to set
#' the panel area.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcRugGrob
#' @author Hou Yun
#' @export
ArcRugGrob <- function(x = NULL,
                       y = NULL,
                       colour = "black",
                       linewidth = 0.5,
                       linetype = 1,
                       alpha = NA,
                       sides = "bl",
                       outside = FALSE,
                       length = unit(2, "mm"),
                       region = CELL(),
                       lineend = "butt",
                       ...) {
  stopifnot(is_CELL(region))
  sides <- intersect(unlist(strsplit(sides, "", TRUE)), c("r", "l", "b", "t"))
  if (length(sides) < 1 || (is.null(x) && is.null(y))) {
    return(zeroGrob())
  }

  data <- data_frame0(x = x,
                      y = y,
                      colour = colour,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)

  if (isTRUE(clip)) {
    data <- region$clip(data)
  }
  if (empty(data)) {
    return(zeroGrob())
  }

  n <- nrow(data)
  data <- data[rep(1:n, length(sides)), , drop = FALSE]
  data$sides <- rep(sides, each = n)

  grid::gTree(data = data,
              outside = outside,
              length = length,
              region = region,
              lineend = lineend,
              cl = "RugGrob")
}

#' @importFrom grid makeContent
#' @export
makeContent.RugGrob <- function(x) {
  data <- x$data
  outside <- x$outside
  length <- x$length
  region <- x$region
  n <- nrow(data)

  if (!grid::is.unit(length)) {
    length <- unit(length, "mm")
  }
  lv <- convert_height(length, "native", TRUE)
  lh <- convert_width(length, "native", TRUE)

  vertical <- data[data$sides %in% c("t", "b"), , drop = FALSE]
  horizon <- data[data$sides %in% c("l", "r"), , drop = FALSE]

  if (!empty(vertical)) {
    if (isTRUE(outside)) {
      yvars <- rlang::set_names(sort(region$y.range), c("b", "t")) + c(-lv, lv)
    } else {
      yvars <- rlang::set_names(sort(region$y.range), c("b", "t")) + c(lv, -lv)
    }

    if (isTRUE(outside)) {
      yvars2 <- rlang::set_names(sort(region$y.range), c("b", "t"))
    } else {
      yvars2 <- rlang::set_names(sort(region$y.range), c("b", "t"))
    }

    vertical$xend <- vertical$x
    vertical$y <- yvars2[vertical$sides]
    vertical$yend <- yvars[vertical$sides]
    vertical <- polar2cartesian(vertical)
    vertical <- grid::segmentsGrob(
      x0 = vertical$x,
      y0 = vertical$y,
      x1 = vertical$xend,
      y1 = vertical$yend,
      gp = gpar(col = alpha(vertical$colour, vertical$alpha),
                fill = alpha(vertical$colour, vertical$alpha),
                lwd = vertical$linewidth * .pt,
                lty = vertical$linetype,
                lineend = x$lineend),
      default.units = "native"
    )
  }

  if (!empty(horizon)) {
    if (region$direct == "clockwise") {
      xvars <- rlang::set_names(c(region$start, region$end), c("l", "r"))
    } else {
      xvars <- rlang::set_names(c(region$start, region$end), c("r", "l"))
    }
    xvars2 <- if (isTRUE(outside)) {
      if (region$direct == "clockwise") {
        rlang::set_names(c(-lh, lh), c("l", "r"))
      } else {
        rlang::set_names(c(-lh, lh), c("r", "l"))
      }
    } else {
      if (region$direct == "clockwise") {
        rlang::set_names(c(lh, -lh), c("l", "r"))
      } else {
        rlang::set_names(c(lh, -lh), c("r", "l"))
      }
    }

    horizon$x <- xvars[horizon$sides]
    horizon$xend <- xvars2[horizon$sides]
    horizon <- lapply(split(horizon, horizon$sides), function(df) {
      vp <- grid::viewport(x = 0,
                           y = 0,
                           width = unit(2, "native"),
                           height = unit(1, "native"),
                           just = c(0.5, 0),
                           angle = (df$x[1] %% 360) - 90,
                           xscale = c(-1, 1),
                           yscale = c(0, 1),
                           default.units = "native")

      grid::segmentsGrob(
        x0 = 0,
        y0 = df$y,
        x1 = df$xend,
        y1 = df$y,
        gp = gpar(col = alpha(df$colour, df$alpha),
                  fill = alpha(df$colour, df$alpha),
                  lwd = df$linewidth * .pt,
                  lty = df$linetype,
                  lineend = x$lineend),
        default.units = "native",
        vp = vp
      )
    })

    horizon <- do.call(grid::grobTree, horizon)
  }
  grid::setChildren(x, grid::gList(if (!empty(vertical)) vertical,
                                   if (!empty(horizon)) horizon))
}

#' @noRd
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
ArcPanelgridGrob <- function(x_breaks = NULL,
                             y_breaks = NULL,
                             x_minor_breaks = NULL,
                             y_minor_breaks = NULL,
                             x.major.gp = element_line(),
                             x.minor.gp = element_line(),
                             y.major.gp = element_line(),
                             y.minor.gp = element_line(),
                             region = CELL(),
                             steps = 0.01,
                             ...) {
  if (!is.null(x_breaks)) {
    xmajor <- ArcVlineGrob(
      xintercept = x_breaks,
      colour = x.major.gp$colour %||% "grey60",
      linewidth = (x.major.gp$linewidth %||% x.major.gp$size %||% 0.5) / .pt,
      linetype = x.major.gp$linetype %||% 1,
      lineend = x.major.gp$lineend %||% "butt",
      region = region,
      steps = steps
    )
  }

  if (!is.null(x_minor_breaks)) {
    xminor <- ArcVlineGrob(
      xintercept = x_minor_breaks,
      colour = x.minor.gp$colour %||% "grey60",
      linewidth = (x.minor.gp$linewidth %||% x.minor.gp$size %||% 0.25) / .pt,
      linetype = x.minor.gp$linetype %||% 1,
      lineend = x.minor.gp$lineend %||% "butt",
      region = region,
      steps = steps
    )
  }

  if (!is.null(y_breaks)) {
    ymajor <- ArcHlineGrob(
      yintercept = y_breaks,
      colour = y.major.gp$colour %||% "grey60",
      linewidth = (y.major.gp$linewidth %||% y.major.gp$size %||% 0.5) / .pt,
      linetype = y.major.gp$linetype %||% 1,
      lineend = y.major.gp$lineend %||% "butt",
      region = region,
      simplify = TRUE
    )
  }

  if (!is.null(y_minor_breaks)) {
    yminor <- ArcVlineGrob(
      yintercept = y_minor_breaks,
      colour = y.minor.gp$colour %||% "grey60",
      linewidth = (y.minor.gp$linewidth %||% y.minor.gp$size %||% 0.25) / .pt,
      linetype = y.minor.gp$linetype %||% 1,
      lineend = y.minor.gp$lineend %||% "butt",
      region = region,
      simplify = TRUE
    )
  }

  grid::gList(
    if (!is.null(x_breaks)) xmajor,
    if (!is.null(x_minor_breaks)) xminor,
    if (!is.null(y_breaks)) ymajor,
    if (!is.null(y_minor_breaks)) yminor
  )
}

#' @noRd
segments2path <- function(data) {
  if (empty(data)) {
    return(data)
  }

  data$group <- 1:nrow(data)
  start <- data[setdiff(names(data), c("xend", "yend"))]
  end <- data[setdiff(names(data), c("x", "y"))]
  end <- rename(end, "x" = "xend", "y" = "yend")

  vec_rbind0(start, end)
}

#' @noRd
#' @note modify from ggplot2:::stairstep()
steps2path <- function(data, direction) {
  direction <- match.arg(direction, c("hv", "vh", "mid"))
  data <- data.frame(data)[order(data$x), , drop = FALSE]
  n <- nrow(data)
  if (n <= 1) {
    return(data[0, , drop = FALSE])
  }
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  } else {
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)
  }

  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps/2
    x <- c(data$x[1], mid_x[xs], data$x[n])
    y <- c(data$y[ys])
    data_attr <- data[c(1, xs, n), setdiff(names(data), c("x", "y")), drop = FALSE]
  } else {
    x <- data$x[xs]
    y <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y")), drop = FALSE]
  }
  data_frame0(x = x, y = y, data_attr)
}
