#' @noRd
#' @importFrom tibble tibble
pieces_data <- function(data,
                        steps = 0.01,
                        simplify = TRUE) {
  extra <- setdiff(names(data), c("x", "y"))
  if (! "subgroup" %in% names(data)) {
    data <- lapply_dfr(split(data, data$group), function(d) {
      n <- nrow(d)
      if (n < 2) return(d)
      len <- N_steps(d$x[-n], d$y[-n], d$x[-1], d$y[-1],
                     steps = steps, simplify = simplify)

      data_frame0(x = c(unlist(mapply(interp, d$x[-n], d$x[-1],
                                      len, SIMPLIFY = FALSE)), d$x[n]),
                  y = c(unlist(mapply(interp, d$y[-n], d$y[-1],
                                      len, SIMPLIFY = FALSE)), d$y[n]),
                  !!!d[c(rep(1:(n - 1), len), n), extra, drop = FALSE])
    })
  } else {
    data <- lapply_dfr(split(data, data$group), function(d) {
      lapply_dfr(split(d, d$subgroup), function(edge) {
        n <- nrow(edge)
        if (n < 2) return(edge)

        len <- N_steps(edge$x[-n], edge$y[-n], edge$x[-1], edge$y[-1],
                       steps = steps, simplify = simplify)
        data_frame0(x = c(unlist(mapply(interp, edge$x[-n], edge$x[-1],
                                        len, SIMPLIFY = FALSE)), edge$x[n]),
                    y = c(unlist(mapply(interp, edge$y[-n], edge$y[-1],
                                        len, SIMPLIFY = FALSE)), edge$y[n]),
                    !!!edge[c(rep(1:(n - 1), len), n), extra, drop = FALSE])
      })
    })
  }

  data
}

#' @noRd
#' @note copy from ggplot2
interp <- function(start, end, n = 10) {
  if (n == 1) {
    return(start)
  }

  start + seq(0, 1, length.out = n + 1)[-(n + 1)] * (end - start)
}

#' @noRd
discrete2numeric <- function(data, coord = PANEL()) {
  nm <- names(data)
  if (length(intersect(nm, x_aes)) > 0 && x_is_NULLcoord(coord)) {
    cli::cli_abort("Coordinate of `x` is uninitialized.")
  }

  if (length(intersect(nm, y_aes)) > 0 && y_is_NULLcoord(coord)) {
    cli::cli_abort("Coordinate of `y` is uninitialized.")
  }

  if (is_discrete(coord$x)) {
    for (ii in intersect(nm, x_aes)) {
      if (!is.numeric(data[[ii]])) {
        data[[ii]] <- as.integer(factor(data[[ii]], levels = coord$x$limits))
      }
    }
  }

  if (is_discrete(coord$y)) {
    for (jj in intersect(nm, y_aes)) {
      if (!is.numeric(data[[jj]])) {
        data[[jj]] <- as.integer(factor(data[[jj]], levels = coord$y$limits))
      }
    }
  }

  data
}

#' @noRd
cartesian2polar <- function(data,
                            coord = PANEL(),
                            region = CELL(),
                            clip = TRUE,
                            na.rm = FALSE) {
  stopifnot(is_PANEL(coord))
  stopifnot(is_CELL(region))

  nm <- names(data)
  all_x <- intersect(nm, x_aes)
  all_y <- intersect(nm, y_aes)

  if (length(all_x) > 0) {
    if (x_is_NULLcoord(coord)) {
      cli::cli_abort("x-axis is uninitialized")
    }
    data <- apply_if(data, nm %in% all_x, scales::rescale,
                     from = coord$x$range,
                     to = region$x.range)
  }

  if (length(all_y) > 0) {
    if (y_is_NULLcoord(coord)) {
      cli::cli_abort("y-axis is uninitialized")
    }
    data <- apply_if(data, nm %in% all_y, scales::rescale,
                     from = coord$y$range,
                     to = region$y.range)
  }

  if (isTRUE(clip)) {
    n <- nrow(data)
    data <- region$clip(data)

    if (nrow(data) < n) {
      cli::cli_inform("Removed {nrow(data) - n} rows overflowing the plot region.")
    }
  }

  ids <- stats::complete.cases(data[c(all_x, all_y)])
  if (isFALSE(na.rm) && sum(!ids) > 0) {
    cli::cli_inform("Removed {sum(!ids)} rows containing missing values.")
  }
  data[ids, , drop = FALSE]
}

#' @noRd
polar2cartesian <- function(data) {
  nm <- names(data)
  all_x <- intersect(nm, x_aes)
  all_y <- intersect(nm, y_aes)
  ids <- intersect(match(all_x, x_aes), match(all_y, y_aes))
  all_x <- x_aes[ids]
  all_y <- y_aes[ids]

  for (ii in seq_along(all_x)) {
    rr <- data[[all_y[ii]]]
    data[[all_y[ii]]] <- sin(radian(data[[all_x[ii]]])) * rr
    data[[all_x[ii]]] <- cos(radian(data[[all_x[ii]]])) * rr
  }

  data
}

#' @noRd
N_steps <- function(x0, y0, x1, y1, steps = 0.01, simplify = FALSE) {
  data <- data_frame0(x0 = x0, y0 = y0, x1 = x1, y1 = y1)
  out <- pmax(floor(euclid_dist2(data$x0, data$y0, data$x1, data$y1)/steps), 1)
  out <- ifelse(is.na(out), 1, out)

  if (isTRUE(simplify)) {
    out <- ifelse(data$x0 == data$x1, 1, out)
  }
  out
}

