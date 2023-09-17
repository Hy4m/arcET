
#' @noRd
gen_bezier <- function(data, height = 0.6, n = 100, ...) {
  if (empty(data)) {
    return(data)
  }

  data$group <- 1:nrow(data)
  data <- polar2cartesian(data)
  nm <- names(data)

  lapply_dfr(split(data, data$group), function(row) {
    pt <- bezier_points(x = row$x,
                        y = row$y,
                        xend = row$xend,
                        yend = row$yend,
                        height = height,
                        n = n)
    data_frame0(pt, row[setdiff(nm, c("x", "y"))])
  })
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
  distance <- euclid_dist(x, y, xend, yend)
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
  if (r0 < distance && r1 < distance) {
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
.gen_bezier <- function(r0 = -1,
                        r1 = 1,
                        theata = 0,
                        height = 0.6,
                        reverse = FALSE,
                        flip = FALSE,
                        cx = 0,
                        cy = 0,
                        n = 100) {
  if (r0 == r1) {
    out <- getBeziers(x = c(r0, r0 - height/2, r1 + height/2, r1),
                      y = c(0, height, height, 0),
                      id = rep(1, 4),
                      detail = n)
    out <- data_frame0(x = out$paths[, 1], y = out$paths[, 2])

    if (r0 == 0) {
      shear <- NULL
    } else if (r0 < 0) {
      shear <- degree(atan(-height/r0))
    } else {
      shear <- degree(atan(height/r0))
    }
  } else {
    if (r0 * r1 < 0) {
      if (r0 > 0) {
        exchange(r0, r1)
      }
      shift <- 0
      shear <- NULL
    } else {
      distance <- abs(r0 - r1)
      if (all(r0 <= 0, r1 <= 0)) {
        shift <- max(r0, r1) - distance/2
        shear <- degree(atan(-height/shift))
      } else {
        shift <- min(r0, r1) + distance/2
        shear <- 180 - degree(atan(height/shift))
      }

      r0 <- -distance/2
      r1 <- distance/2
    }

    out <- getBeziers(x = c(r0, 0, r1),
                      y = c(0, height, 0),
                      id = rep(1, 3),
                      detail = n)
    out <- data_frame0(x = out$paths[, 1], y = out$paths[, 2])
  }


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
rotate_points <- function(data, angle = 0, cx = 0, cy = 0) {
  if (empty(data) || angle == 0) {
    return(data)
  }

  angle <- radian(angle)
  nm <- names(data)
  all_x <- intersect(nm, x_aes)
  all_y <- intersect(nm, y_aes)
  ids <- intersect(match(all_x, x_aes), match(all_y, y_aes))
  all_x <- x_aes[ids]
  all_y <- y_aes[ids]

  for (ii in seq_along(all_x)) {
    tmp_x <- data[[all_x[ii]]] - cx
    tmp_y <- data[[all_y[ii]]] - cy

    data[[all_x[ii]]] <- cos(angle) * tmp_x - sin(angle) * tmp_y + cx
    data[[all_y[ii]]] <- sin(angle) * tmp_x + cos(angle) * tmp_y + cy
  }

  data
}

#' @noRd
shear_points <- function(data, angle = 0) {
  data$x <- data$x + data$y * cos(radian(angle))
  data
}

#' @noRd
getBeziers <- function(x, y, id, detail) {
  rlang::check_installed("ggforce", reason = "for `getBeziers`.")
  FUN <- utils::getFromNamespace("getBeziers", "ggforce")
  FUN(x = x, y = y, id = id, detail = detail)
}
