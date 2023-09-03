#' Arc Chord Grob
#'
#' @description These functions can draw chord on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values of start points.
#' @param y positive numeric vector (in radius) specifying y-values of start points.
#' @param xend numeric vector (in degree) specifying x-values of end points.
#' @param yend positive numeric vector (in radius) specifying y-values of end points.
#' @param width width of start (in degree), should be less than 360.
#' @param width_end width of end (in degree), should be less than 360.
#' @param colour color of lines.
#' @param fill fill colour of lines
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @param height numeric value in [0, 1].
#' @param n integer value. The larger the value, the smoother the curve.
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param ... other parameters passing to `pieces_data()`.
#' @details `ArcLinkGrob()` used to drawing segment links, and `ArcBezierGrob()`
#' used to drawing bezier links on center.
#' @return a grob object.
#' @rdname ArcChordGrob
#' @author Hou Yun
#' @export
ArcChordGrob <- function(x = 20,
                         y = 0.5,
                         xend = 70,
                         yend = 0.5,
                         width = 10,
                         width_end = 20,
                         colour = NA,
                         fill = "grey25",
                         linewidth = 0.5,
                         linetype = 1,
                         alpha = NA,
                         height = 0.6,
                         n = 100,
                         lineend = "butt",
                         linejoin = "round",
                         linemitre = 10,
                         ...) {
  data <- data_frame0(x = (x %% 360) + 360,
                      y = y,
                      xend = (xend %% 360) + 360,
                      yend = yend,
                      width = abs(width) %% 360,
                      width_end = abs(width_end) %% 360,
                      colour = colour,
                      fill = fill,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)

  large_than_360 <- (data$width + data$width_end) > 360
  if (any(large_than_360)) {
    cli::cli_warn(c("Sum of `width` and `width_end` must be less than 360,",
                    "Removed {sum(large_than_360)} rows with too large width."))
    data <- data[!large_than_360, , drop = FALSE]
  }

  if (empty(data)) {
    return(zeroGrob())
  }

  data <- gen_chord(data = data, height = height, n = n, ...)
  first_row <- data[!duplicated(data$group), , drop = FALSE]

  grid::polygonGrob(x = data$x,
                    y = data$y,
                    id = data$group,
                    default.units = "native",
                    gp = gpar(col = alpha(first_row$colour, first_row$alpha),
                              fill = alpha(first_row$fill, first_row$alpha),
                              lwd = first_row$linewidth * .pt,
                              lty = first_row$linetype,
                              lineend = lineend,
                              linejoin = linejoin,
                              linemitre = linemitre))
}

#' Arc Link Grob
#'
#' @description These functions can draw segments or bezier curve on polar coordinate.
#'
#' @param x numeric vector (in degree) specifying x-values of start points.
#' @param y positive numeric vector (in radius) specifying y-values of start points.
#' @param xend numeric vector (in degree) specifying x-values of end points.
#' @param yend positive numeric vector (in radius) specifying y-values of end points.
#' @param colour color of line.
#' @param linewidth line width in pt.
#' @param linetype line type, same as `lty` in `gpar()`.
#' @param alpha transparency of lines.
#' @param arrow NULL or arrow object created by `arrow()`.
#' @param height numeric value in [0, 1].
#' @param n integer value. The larger the value, the smoother the curve.
#' @param lineend line end style (round, butt, square).
#' @param linejoin line join style (round, mitre, bevel).
#' @param linemitre line mitre limit (number greater than 1).
#' @param ... not used.
#' @details `ArcLinkGrob()` used to drawing segment links, and `ArcBezierGrob()`
#' used to drawing bezier links on center.
#' @return a grob object.
#' @rdname ArcBezierGrob
#' @author Hou Yun
#' @export
ArcBezierGrob <- function(x = 20,
                          y = 0.5,
                          xend = 70,
                          yend = 0.5,
                          height = 0.6,
                          n = 100,
                          colour = "black",
                          linewidth = 0.5,
                          linetype = 1,
                          alpha = NA,
                          arrow = NULL,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 10,
                          ...) {
  data <- data_frame0(x = x,
                      y = y,
                      xend = xend,
                      yend = yend,
                      colour = colour,
                      linewidth = linewidth,
                      linetype = linetype,
                      alpha = alpha)

  if (empty(data)) {
    return(zeroGrob())
  }

  data <- gen_bezier(data = data, height = height, n = n, ...)
  first_row <- data[!duplicated(data$group), , drop = FALSE]

  grid::polylineGrob(x = data$x,
                     y = data$y,
                     id = data$group,
                     arrow = arrow,
                     default.units = "native",
                     gp = gpar(col = alpha(first_row$colour, first_row$alpha),
                               lwd = first_row$linewidth * .pt,
                               lty = first_row$linetype,
                               lineend = lineend,
                               linejoin = linejoin,
                               linemitre = linemitre))
}

#' @rdname ArcBezierGrob
#' @export
ArcLinkGrob <- function(x = 20,
                        y = 0.5,
                        xend = 70,
                        yend = 0.7,
                        arc = TRUE,
                        colour = "black",
                        linewidth = 0.5,
                        linetype = 1,
                        alpha = NA,
                        ...) {
  ArcSegmentsGrob(x = x,
                  y = y,
                  xend = xend,
                  yend = yend,
                  arc = arc,
                  colour = colour,
                  linewidth = linewidth,
                  linetype = linetype,
                  alpha = alpha,
                  ...)
}

#' @noRd
gen_bezier <- function(data, height = 0.6, n = 100, ...) {
  data$group <- 1:nrow(data)
  same_line <- data$x == data$xend

  df <- data[same_line, , drop = FALSE]
  df2 <- data[!same_line, , drop = FALSE]

  nm <- names(data)
  position <- c("x", "y", "xend", "yend")

  if (!empty(df)) {
    df <- polar2cartesian(pieces_data(segments2path(df), ...))
  } else {
    df <- NULL
  }

  if (!empty(df2)) {
    df2 <- polar2cartesian(df2)
    df2 <- lapply(split(df2, df2$group), function(row) {
      pt <- bezier_points(x = row$x,
                          y = row$y,
                          xend = row$xend,
                          yend = row$yend,
                          height = height,
                          n = n)
      data_frame0(pt, row[setdiff(nm, position)])
    })
    df2 <- Reduce("rbind", df2)
  } else {
    df2 <- NULL
  }

  rbind(df, df2)
}

#' @noRd
gen_chord <- function(data, height = 0.6, n = 100, ...) {
  data$group <- 1:nrow(data)
  data$same_line <- data$x == data$xend

  nm <- names(data)
  position <- c("x", "y", "xend", "yend", "width", "width_end", "same_line")
  data <- lapply_dfr(split(data, data$group), function(row) {
    if (row$same_line) {
      pt <- data_frame0(x = c(row$x - row$width/2, row$x + row$width/2,
                              row$x + row$width_end/2, row$x - row$width/2,
                              row$x - row$width/2),
                        y = c(row$y, row$y, row$yend, row$yend, row$y),
                        row[setdiff(nm, position)])

      polar2cartesian(pieces_data(pt, ...))
    } else {
      pt1 <- data_frame0(x = c(row$x + row$width/2, row$x - row$width/2),
                         y = c(row$y, row$y),
                         group = row$group)
      pt1 <- polar2cartesian(pieces_data(pt1, ...))
      pt2 <- gen_bezier(data_frame0(x = row$x - row$width/2,
                                    y = row$y,
                                    xend = row$xend + row$width_end/2,
                                    yend = row$yend,
                                    group = row$group),
                        height = height,
                        n = n)
      pt3 <- data_frame0(x = c(row$xend + row$width_end/2, row$xend - row$width_end/2),
                         y = c(row$yend, row$yend),
                         group = row$group)
      pt3 <- polar2cartesian(pieces_data(pt3, ...))
      pt4 <- gen_bezier(data_frame0(x = row$xend - row$width_end/2,
                                    y = row$yend,
                                    xend = row$x + row$width/2,
                                    yend = row$y,
                                    group = row$group),
                        height = height,
                        n = n)

      pos <- vec_rbind0(pt1, pt2[-1, , drop = FALSE], pt3[-1, , drop = FALSE], pt4)
      data_frame0(pos, row[setdiff(nm, c(position, "group"))])
    }
  })

  data
}

#' @noRd
bezier_points <- function(x, y, xend, yend, height = 0.6, n = 100) {
  dist <- euclid_dist(x, y, xend, yend)
  vx <- (x*yend - xend*y)*(yend - y)/((yend - y)^2 + (xend - x)^2)
  vy <- ifelse(y == yend, (y + yend)/2, vx * ((x - xend)/(yend - y)))
  r0 <- euclid_dist(x, y, vx, vy)
  r1 <- euclid_dist(xend, yend, vx, vy)
  theata <- degree(atan2(vy, vx)) %% 360
  theata0 <- degree(atan2(y, x)) %% 360
  theata2 <- degree(atan2(yend, xend)) %% 360
  height <- euclid_dist(0, 0, vx, vy) * height * 2

  if (vy > 0) {
    flip <- TRUE
  } else {
    flip <- FALSE
  }

  reverse <- FALSE

  if (r0 < dist && r1 < dist) {
    if (theata == 0) {
      theata <- 360
    }
    if (theata < 180) {
      theata <- theata - 90
      if (x > xend) {
        reverse <- TRUE
        exchange(r0, r1)
      }
    } else {
      if ((theata == 180 && y <= yend) ||
          (theata == 360 && y >= yend) ||
          (x > xend)) {
        reverse <- TRUE
        exchange(r0, r1)
      }
      theata <- theata - 270
    }

    r0 <- -r0
  } else {
    if (theata == 0) {
      flip <- FALSE
      theata <- 90

      if (y >= yend) {
        reverse <- TRUE
        exchange(r0, r1)
      }

      if (y <= 0 && yend <= 0) {
        r0 <- -r0
        r1 <- -r1
      }
    } else if (theata <= 180) {
      flip <- TRUE
      if (x > xend || (x == xend && y <= yend)) {
        reverse <- TRUE
        exchange(r0, r1)
      }

      if (theata0 >= theata && theata2 >= theata) {
        r0 <- -r0
        r1 <- -r1
      }
      theata <- theata - 90
    } else {
      flip <- FALSE
      if (x > xend) {
        reverse <- TRUE
        exchange(r0, r1)
      }

      if (x <= vx && xend <= vx) {
        r0 <- -r0
        r1 <- -r1
      }

      theata <- theata - 270
    }

    vx <- (x + xend)/2
    vy <- (y + yend)/2
  }

  .gen_bezier(r0 = r0,
              r1 = r1,
              theata = theata,
              height = height,
              flip = flip,
              reverse = reverse,
              cx = vx,
              cy = vy,
              n = n)
}

#' @noRd
.gen_bezier <- function(r0 = 1,
                        r1 = 1,
                        theata = 0,
                        height = 0.6,
                        reverse = FALSE,
                        flip = FALSE,
                        cx = 0,
                        cy = 0,
                        n = 100) {
  if (r0 * r1 < 0) {
    if (r0 > 0) {
      exchange(r0, r1)
    }
    shift <- 0
    shear <- NULL
  } else {
    dist <- abs(r0 - r1)
    if (all(r0 <= 0, r1 <= 0)) {
      shift <- max(r0, r1) - dist/2
      shear <- degree(atan(-height/shift))
    } else {
      shift <- min(r0, r1) + dist/2
      shear <- 180 - degree(atan(height/shift))
    }

    r0 <- -dist/2
    r1 <- dist/2
  }

  tt <- seq(0, 1, length.out = n)
  p <- cbind(c(r0, 0, r1), c(0, height, 0))
  pt <- bezier::bezier(tt, p)

  out <- data_frame0(x = pt[, 1], y = pt[, 2])

  if (!is.null(shear)) {
    out <- shear_points(out, angle = shear)
  }

  if (isTRUE(flip)) {
    out$y <- -out$y
  }

  if (theata != 0) {
    out <- rotate_points(out, angle = theata)
  }

  if (isTRUE(reverse)) {
    out <- out[nrow(out):1, , drop = FALSE]
  }

  out$x <- out$x + cx
  out$y <- out$y + cy

  out
}

#' @noRd
shear_points <- function(data, angle = 0) {
  data$x <- data$x + data$y * cos(radian(angle))
  data
}
