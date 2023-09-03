## TODO: Auto
#' Arc Polygon Grob
#'
#' @description These functions can draw polygon on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of polygon.
#' @param fill background of polygon.
#' @param linewidth border size of polygon.
#' @param alpha transparency of polygon.
#' @param group numeric vector used to separate locations in x and y
#' into multiple polygons.
#' @param subgroup just be used when polygons have holes.
#' @param rule character value specifying the fill rule: one of "winding" or
#' "evenodd" (default).
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param arc logical, if TRUE will split each path in small pieces.
#' @param steps step length used to split data. A smaller value means a
#' smoother curve.
#' @param simplify logical, When TRUE, line segments equal to x will not be split.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcPolygonGrob
#' @author Hou Yun
#' @export
ArcPolygonGrob <- function(x = c(30, 150, 150, 30),
                           y = c(0.6, 0.6, 0.9, 0.9),
                           colour = NA,
                           fill = "grey20",
                           linewidth = 0.5,
                           linetype = 1,
                           alpha = NA,
                           group = 1L,
                           subgroup = NULL,
                           rule = "evenodd",
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
                      fill = fill,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha,
                      group = group %||% 1L,
                      subgroup = subgroup)

  if (!is.integer(data$group)) {
    data$group <- as.integer(factor(data$group))
  }
  if (!is.null(subgroup) && !is.integer(data$subgroup)) {
    data$subgroup <- as.integer(factor(data$subgroup))
  }

  data <- add_endpoint(data)
  data <- data[count_by_group(data) >= 3, , drop = FALSE]
  if (empty(data)) {
    return(zeroGrob())
  }

  if (isTRUE(arc)) {
    data <- pieces_data(data, steps = steps, simplify = simplify)
  }
  data <- polar2cartesian(data)

  first_rows <- data[!duplicated(data$group), , drop = FALSE]
  if (empty(subgroup)) {
    grid::polygonGrob(x = data$x,
                      y = data$y,
                      id = data$group,
                      default.units = "native",
                      gp = gpar(col = alpha(first_rows$colour, first_rows$alpha),
                                fill = alpha(first_rows$fill, first_rows$alpha),
                                lwd = first_rows$linewidth * .pt,
                                lty = first_rows$linetype,
                                lineend = lineend,
                                linejoin = linejoin,
                                linemitre = linemitre))
  } else {
    grid::pathGrob(x = data$x,
                   y = data$y,
                   id = data$subgroup,
                   pathId = data$group,
                   rule = rule,
                   default.units = "native",
                   gp = gpar(col = alpha(first_rows$colour, first_rows$alpha),
                             fill = alpha(first_rows$fill, first_rows$alpha),
                             lwd = first_rows$linewidth * .pt,
                             lty = first_rows$linetype,
                             lineend = lineend,
                             linejoin = linejoin,
                             linemitre = linemitre))
  }
}

#' Arc Rect Grob
#'
#' @description These functions can draw rect on polar coordinate.
#'
#' @param xmin,xmax numeric vector (in degree) specifying x-values of
#' start and end points.
#' @param ymin,ymax positive numeric vector (in radius) specifying y-values
#' of start and end points.
#' @param colour color of rect.
#' @param fill background of rect.
#' @param linewidth border size of rect.
#' @param alpha transparency of rect.
#' @param ... other parameters passing to `ArcPolygonGrob()`.
#' @return a grob object.
#' @rdname ArcRectGrob
#' @author Hou Yun
#' @export
ArcRectGrob <- function(xmin = 30,
                        ymin = 0.6,
                        xmax = 150,
                        ymax = 0.9,
                        colour = NA,
                        fill = "grey20",
                        linewidth = 0.5,
                        linetype = 1,
                        alpha = NA,
                        ...) {
  data <- data_frame0(xmin = xmin,
                      ymin = ymin,
                      xmax = xmax,
                      ymax = ymax,
                      colour = colour,
                      fill = fill,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)

  if (empty(data)) {
    return(zeroGrob())
  }

  data <- rect2polygon(data)
  ArcPolygonGrob(x = data$x,
                 y = data$y,
                 group = data$group,
                 colour = data$colour,
                 fill = data$fill,
                 linewidth = data$linewidth,
                 linetype = data$linetype,
                 alpha = data$alpha,
                 ...)
}

#' Arc Tile Grob
#'
#' @description These functions can draw tile on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param height one-length numeric specifying tile height.
#' @param width one-length numeric specifying tile width.
#' @param colour color of tile.
#' @param fill background of tile.
#' @param linewidth border size of tile.
#' @param alpha transparency of rect.
#' @param ... other parameters passing to `ArcPolygonGrob()`.
#' @return a grob object.
#' @rdname ArcTileGrob
#' @author Hou Yun
#' @export
ArcTileGrob <- function(x = 90,
                        y = 0.8,
                        width = NULL,
                        height = NULL,
                        colour = NA,
                        fill = "grey20",
                        linewidth = 0.5,
                        linetype = 1,
                        alpha = NA,
                        ...) {
  data <- data_frame0(x = (x %% 360) + 360,
                      y = y,
                      colour = colour,
                      fill = fill,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)

if (empty(data)) {
    return(zeroGrob())
  }

  width <- width %||% ggplot2::resolution(data$x, zero = FALSE)
  height <- height %||% ggplot2::resolution(data$y, zero = FALSE)
  ArcRectGrob(xmin = ddata$x - width/2,
              ymin = data$y - height/2,
              xmax = data$x + width/2,
              ymax = ata$y + height/2,
              colour = data$colour,
              fill = data$fill,
              linewidth = data$linewidth,
              linetype = data$linetype,
              alpha = data$alpha,
              ...)
}

#' Arc Area Grob
#'
#' @description These functions can draw polygon based on path and baseline
#' on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of polygon.
#' @param fill background of polygon.
#' @param linewidth border size of polygon.
#' @param alpha transparency of polygon.
#' @param group numeric vector used to separate locations in x and y
#' into multiple polygons.
#' @param base_line positive numeric in [0, 1].
#' @param ... other parameters passing to `ArcPolygonGrob()`.
#' @return a grob object.
#' @rdname ArcAreaGrob
#' @author Hou Yun
#' @export
ArcAreaGrob <- function(x = c(120, 90, 60),
                        y = c(0.5, 0.8, 0.5),
                        colour = "black",
                        fill = "grey20",
                        linewidth = 0.5,
                        linetype = 1,
                        alpha = NA,
                        group = 1L,
                        base_line = NULL,
                        ...) {
  data <- data_frame0(x = x,
                      y = y,
                      colour = colour,
                      fill = fill,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha,
                      group = group %||% 1L)

 data <- data[count_by_group(data) >= 2, , drop = FALSE]
  if (empty(data)) {
    return(zeroGrob())
  }

 n <- nrow(data)
  base_line <- base_line %||% min(data$y, na.rm = TRUE)
  data <- data[order(data$group, decreasing = TRUE), , drop = FALSE]

  first <- which(!duplicated(data$group))
  start <- data[first, , drop = FALSE]
  start$y <- base_line
  end <- data[c(first[-1] - 1L, n), , drop = FALSE]
  end$y <- base_line

  data <- vec_rbind0(start, data, end, start)

  ArcPolygonGrob(x = data$x,
                 y = data$y,
                 colour = data$colour,
                 fill = data$fill,
                 linewidth = data$linewidth,
                 linetype = data$linetype,
                 alpha = data$alpha,
                 group = data$group,
                 ...)
}

#' Arc Ribbon Grob
#'
#' @description These functions can draw ribbon on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param xmin,xmax numeric vector (in degree) specifying left and right border
#' of vertical ribbon.
#' @param ymin,ymax positive numeric vector (in radius) specifying bottom and top
#' border of horizontal ribbon.
#' @param colour color of ribbon.
#' @param fill background of ribbon.
#' @param linewidth border size of ribbon.
#' @param alpha transparency of ribbon.
#' @param group numeric vector used to separate locations in x and y
#' into multiple ribbons.
#' @param ... other parameters passing to `ArcPolygonGrob()`.
#' @return a grob object.
#' @rdname ArcRibbonGrob
#' @author Hou Yun
#' @export
ArcRibbonGrob <- function(x = NULL,
                          ymin = NULL,
                          ymax = NULL,
                          y = NULL,
                          xmin = NULL,
                          xmax = NULL,
                          colour = NA,
                          fill = "grey20",
                          linewidth = 0.5,
                          linetype = 1,
                          alpha = NA,
                          group = 1L,
                          ...) {
  if (!any(is.null(x), is.null(ymin), is.null(ymax))) {
    data <- data_frame0(x = x,
                        ymin = ymin,
                        ymax = ymax,
                        colour = colour,
                        fill = fill,
                        linewidth = linewidth,
                        linetype = linetype,
                        alpha = alpha,
                        group = group)

    data <- data[order(data$x), , drop = FALSE]
    direct <- "horizon"
  } else {
    if (any(is.null(y), is.null(xmin), is.null(xmax))) {
      nm <- names(data_frame0(x = x, y = y, xmin = xmin, ymin = ymin,
                              xmax = xmax, ymax = ymax))
      msg <- if (length(nm) == 0) {
        "You didn't pass any of this."
      } else {
        "You passed {.field {nm}} of this."
      }
      cli::cli_warn(c("`ArcribbonGrob()` needs [`x`, `ymin`, `ymax`] or [`y`, `xmin`, `xmax`] position vars.",
                      msg))
      return(zeroGrob())
    }

    data <- data_frame0(y = y,
                        xmin = xmin,
                        xmax = xmax,
                        colour = colour,
                        fill = fill,
                        linewidth = linewidth,
                        linetype = linetype,
                        alpha = alpha,
                        group = group)

    data <- data[order(data$y), , drop = FALSE]
    direct <- "vertical"
  }

  data <- data[count_by_group(data) >= 2, , drop = FALSE]
  if (empty(data)) {
    return(zeroGrob())
  }

  if (direct == "horizon") {
    data <- lapply_dfr(split(data, data$group), function(d) {
      y <- c(d$ymin, rev(d$ymax), d$ymin[1])
      id <- c(1:nrow(d), nrow(d):1, 1)
      d <- d[id, , drop = FALSE]
      d$y <- y
      d[setdiff(names(d), c("ymin", "ymax"))]
    })
  } else {
    data <- lapply_dfr(split(data, data$group), function(d) {
      x <- c(d$xmin, rev(d$xmax), d$xmin[1])
      id <- c(1:nrow(d), nrow(d):1, 1)
      d <- d[id, , drop = FALSE]
      d$x <- x
      d[setdiff(names(d), c("xmin", "xmax"))]
    })
  }

  ArcPolygonGrob(x = data$x,
                 y = data$y,
                 colour = data$colour,
                 fill = data$fill,
                 linewidth = data$linewidth,
                 linetype = data$linetype,
                 alpha = data$alpha,
                 group = data$group,
                 subgroup = NULL,
                 ...)
}

#' Cell Region Rect
#' @description Helper function to add background based on cell region.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param fill background of panel.
#' @param colour border of panel.
#' @param ... other parameters passing to `ArcPolygonGrob()`.
#' @rdname ArcRectGrob
#' @author Hou Yun
#' @export
ArcPanelGrob <- function(region = CELL(),
                         fill = "grey85",
                         colour = NA,
                         ...) {
  ## if area of cell equal to zero, will return nullGrob
  if (empty(region)) {
    return(zeroGrob())
  }

  ArcRectGrob(xmin = region$start,
              xmax = region$end,
              ymin = region$r0,
              ymax = region$r1,
              fill = fill,
              colour = colour,
              ...)
}

#' @noRd
rect2polygon <- function(data) {
  if (empty(data)) {
    return(data)
  }

  data$group <- 1:nrow(data)
  extra <- setdiff(names(data), c("xmin", "ymin", "xmax", "ymax"))

  data_attr <- data[rep(data$group, 5L), extra]

  data_frame0(x = c(data$xmin, data$xmax, data$xmax, data$xmin, data$xmin),
              y = c(data$ymin, data$ymin, data$ymax, data$ymax, data$ymin),
              data_attr)
}

#' @noRd
add_endpoint <- function(data) {
  if (empty(data)) {
    return(data)
  }

  if (!"subgroup" %in% names(data)) {
    data <- lapply_dfr(split(data, data$group), function(df) {
      if (!identical(df[1, c("x", "y"), drop = FALSE],
                     df[nrow(df), c("x", "y"), drop = FALSE])) {
        vec_rbind0(df, df[1, , drop = FALSE])
      } else {
        df
      }
    })
  } else {
    lapply_dfr(split(data, data$group), function(df) {
      lapply_dfr(split(df, df$subgroup), function(subdf) {
        if (!identical(subdf[1, c("x", "y"), drop = FALSE],
                       subdf[nrow(df), c("x", "y"), drop = FALSE])) {
          vec_rbind0(subdf, subdf[1, , drop = FALSE])
        } else {
          subdf
        }
      })
    })
  }

  data
}
