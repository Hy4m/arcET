#' Cell Region
#' @description This function always used to setting panel region on
#' polar coordinate. Note that the start point needs to be larger than
#' the end point.
#' @param start panel start point in degree, should be in [0, 360].
#' @param end panel end point in degree, should be in [0, 360].
#' @param r0 panel start point in radius, and should be in [0, 1].
#' @param r1 panel end point in radius, and should be in [0, 1].
#' @param direct must be one of "clockwise" or "reverse.clockwise". If "clockwise",
#' x-axis is clockwise.
#' @param zero logical. When `zero` is FALSE, `start` equals to `end` meaning
#' that the X-axis spans the entire circle.
#' @return a `CELL` object.
#' @rdname CELL
#' @author Hou Yun
#' @export
CELL <- function(start = 360,
                 end = 0,
                 r0 = 0,
                 r1 = 1,
                 direct = "clockwise",
                 zero = FALSE) {
  direct <- match.arg(direct, c("clockwise", "reverse.clockwise"))
  if (any(!is.numeric(start), !is.numeric(end)) ||
      any(length(start) > 1, length(end) > 1) ||
      any(start > 360, start < 0, end > 360, end < 0)) {
    cli::cli_abort(c("Invalid CELL information:\n",
                     "`start` and `end` should be in [0, 360]."))
  }

  if (any(!is.numeric(r0), !is.numeric(r1)) ||
      any(length(r0) > 1, length(r1) > 1) ||
      any(r0 < 0, r1 < 0)) {
    cli::cli_abort(c("Invalid CELL information:\n",
         "`r0` and `r1` should be in [0, 1]."))
  }

  out <- structure(list(start = start,
                        end = end,
                        r0 = r0,
                        r1 = r1,
                        direct = direct),
                   class = "CELL")

  if (direct == "clockwise") {
    clip <- function(data) {
      start <- force(out$start)
      end <- force(out$end)
      r0 <- force(out$r0)
      r1 <- force(out$r1)

      if (empty(data)) {
        return(data)
      }

      xvars <- intersect(names(data), x_aes)
      yvars <- intersect(names(data), y_aes)

      if (length(xvars) > 0) {
        x_in <- lapply(data[xvars], function(x) {
          if (start == end) {
            if (isTRUE(zero)) {
              (((x %% 360) - start) %% 360) == 0
            } else {
              rep(TRUE, nrow(data))
            }
          } else {
            if (start > end) {
              (x %% 360) <= start & (x %% 360) >= end
            } else {
              (x %% 360) <= start | (x %% 360) >= end
            }
          }
        })

        data <- data[Reduce("&", x_in), , drop = FALSE]
      }

      if (length(yvars) > 0 && !empty(data)) {
        y_in <- lapply(data[yvars], function(y) {
          y >= min(r0, r1) & y <= max(r0, r1)
        })

        data <- data[Reduce("&", y_in), , drop = FALSE]
      }

      data
    }

    if (start == end) {
      if (isFALSE(zero)) {
        start <- start + 360
      }
    } else {
      if (start < end) {
        start <- start + 360
      }
    }
  } else {
    clip <- function(data) {
      start <- force(out$start)
      end <- force(out$end)
      r0 <- force(out$r0)
      r1 <- force(out$r1)

      if (empty(data)) {
        return(data)
      }

      xvars <- intersect(names(data), x_aes)
      yvars <- intersect(names(data), y_aes)

      if (length(xvars) > 0) {
        x_in <- lapply(data[xvars], function(x) {
          if (start == end) {
            if (isTRUE(zero)) {
              (((x %% 360) - start) %% 360) == 0
            } else {
              rep(TRUE, nrow(data))
            }
          } else {
            if (start > end) {
              (x %% 360) >= start | (x %% 360) <= end
            } else {
              (x %% 360) >= start & (x %% 360) <= end
            }
          }
        })

        data <- data[Reduce("&", x_in), , drop = FALSE]
      }

      if (length(yvars) > 0 && !empty(data)) {
        y_in <- lapply(data[yvars], function(y) {
          y >= min(r0, r1) & y <= max(r0, r1)
        })

        data <- data[Reduce("&", y_in), , drop = FALSE]
      }

      data
    }

    if (start == end) {
      if (isFALSE(zero)) {
        end <- end + 360
      }
    } else {
      if (start > end) {
        end <- end + 360
      }
    }
  }

  out$mid_x <- mean(start, end)
  out$mid_y <- mean(r0, r1)
  out$x.range <- c(start, end)
  out$y.range <- c(r0, r1)
  out$clip <- clip
  out
}

#' @export
print.CELL <- function(x, ...) {
  cli::cli_h1("--------CELLMETA information:--------")
  cli::cli_inform("  start -> {x$start} (in degree)")
  cli::cli_inform("  end -> {x$end} (in degree)")
  cli::cli_inform("  r0 -> {x$r0} (in [0, 1])")
  cli::cli_inform("  r0 -> {x$r1} (in [0, 1])")
  cli::cli_inform("  direct -> {x$direct} ('clockwise' or 'reverse.clockwise')")
  cli::cli_inform("  x.range -> [{x$x.range[1]}, {x$x.range[2]}]")
  cli::cli_inform("  y.range -> [{x$y.range[1]}, {x$y.range[2]}]")
  cli::cli_inform("  clip -> {.cls {class(x$clip)[1]}}")
  cli::cli_h1("------------------------------------")
}

#' Panel Meta Information
#' @description This function always used to setting panel meta information on
#' cartesian coordinate.
#' the end point.
#' @param x_limits,y_limits For continuous coordinates, it is a numerical
#' vector of length 2, representing the value range of coordinate axes.
#' For discrete coordinates, it is an enumeration vector of all
#' that can take values.
#' @param x_breaks,y_breaks for continuous coordinates, it is a numerical
#' vector in the interval, representing the position of the ticks.
#' @param x_minor_breaks,y_minor_breaks minor breaks used to draw minor panel grid.
#' For discrete coordinates, it's anything within the limits.
#' @param x_labels,y_labels labels of ticks, should be same length as breaks.
#' @param x_range,y_range axis scale in numeric.
#' @param x_expand,y_expand a vector of range expansion constants used to add
#' some padding around the data to ensure that they are placed some distance
#' away from the axes. It can be created by `ggplot2::expansion()`.
#' @param drop logical. If TRUE, the levels that does not appear will be removed.
#'
#' @return a `PANEL` object.
#' @rdname PANEL
#' @author Hou Yun
#' @export
PANEL <- function(x_limits = NULL,
                  y_limits = NULL,
                  x_breaks = NULL,
                  y_breaks = NULL,
                  x_minor_breaks = NULL,
                  y_minor_breaks = NULL,
                  x_labels = NULL,
                  y_labels = NULL,
                  x_range = NULL,
                  y_range = NULL,
                  x_expand = NULL,
                  y_expand = NULL,
                  drop = FALSE) {
 x <- .PANEL(limits = x_limits,
             breaks = x_breaks,
             minor_breaks = x_minor_breaks,
             labels = x_labels,
             range = x_range,
             expand = x_expand,
             drop = drop)
  y <- .PANEL(limits = y_limits,
              breaks = y_breaks,
              minor_breaks = y_minor_breaks,
              labels = y_labels,
              range = y_range,
              expand = y_expand,
              drop = drop)

  structure(list(x = x, y = y), class = "PANEL")
}

#' @noRd
.PANEL <- function(limits = NULL,
                   breaks = NULL,
                   minor_breaks = NULL,
                   labels = NULL,
                   range = NULL,
                   expand = NULL,
                   drop = FALSE,
                   ...) {
  if (empty(limits)) {
   out <-  structure(list(limits = NULL,
                          breaks = NULL,
                          minor_breaks = NULL,
                          labels = NULL,
                          range = NULL),
                     class = c("null_coord", "coord"))
   return(out)
  }

  if (length(breaks) != length(labels)) {
    cli::cli_abort(c("{.arg breaks} must have same length as {.arg labels},",
                     i = "your supplied {.arg breaks} is length {length(breaks)},",
                     i = "{.arg labels} is length {length(labels)}."))
  }

  if (is.numeric(limits)) {
    limits <- range(limits, na.rm = TRUE)

    if (is.null(range)) {
      if (is.null(expand)) {
        if (diff(limits) == 0) {
          expand <- ggplot2::expansion(0, 0.05)
        } else {
          expand <- ggplot2::expansion(0.05, 0)
        }
      } else {
        expand <- rep_len(expand, 4)
      }

      range <- c(limits[1] - diff(limits)*expand[1] - expand[2],
                 limits[2] + diff(limits)*expand[3] + expand[4])
    }

    labels <- labels %||% breaks
    id <- breaks >= range[1] & breaks <= range[2]
    breaks <- breaks[id & !is.na(id)]
    labels <- labels[id & !is.na(id)]
    if (empty(breaks)) {
      breaks <- labels <- NULL
    }

    if (!is.null(minor_breaks)) {
      id <- minor_breaks >= range[1] & minor_breaks <= range[2]
      minor_breaks <- minor_breaks[id & !is.na(id)]
      if (length(minor_breaks) == 0) {
        minor_breaks <- NULL
      }
    }

    clss <- c("continuous_coord", "coord")
  } else {
    if (!is.character(limits) && !is.factor(limits)) {
      cli::cli_abort("Axis with class {.cls {class(limits)}} are not yet implemented.")
    }

    if (!is.factor(limits)) {
      limits <- factor(limits)
    }
    if (isTRUE(drop)) {
      limits <- droplevels(limits)
    }
    limits <- levels(limits)

    if (is.null(range)) {
      if (is.null(expand)) {
        expand <- ggplot2::expansion(0, 0.6)
      } else {
        expand <- rep_len(expand, 4)
      }

      rng <- c(1, length(limits))
      range <- c(rng[1] - diff(rng)*expand[1] - expand[2],
                 rng[2] + diff(rng)*expand[3] + expand[4])
    }

    breaks <- breaks %||% limits
    labels <- labels %||% breaks
    if (is.character(breaks)) {
      breaks <- as.integer(factor(breaks, levels = limits))
    }

    id <- breaks >= range[1] & breaks <= range[2]
    breaks <- breaks[id & !is.na(id)]
    labels <- labels[id & !is.na(id)]
    if (length(breaks) == 0) {
      breaks <- labels <- NULL
    }

    if (!is.null(minor_breaks)) {
      if (is.character(breaks)) {
        minor_breaks <- as.integer(factor(minor_breaks, levels = limits))
      }

      id <- minor_breaks >= range[1] & minor_breaks <= range[2]
      minor_breaks <- minor_breaks[id & !is.na(id)]
      if (length(minor_breaks) == 0) {
        minor_breaks <- NULL
      }
    }

    clss <- c("discrete_coord", "coord")
  }

  structure(list(limits = limits,
                 breaks = breaks,
                 minor_breaks = minor_breaks,
                 labels = labels,
                 range = range,
                 ...),
            class = clss)
}

#' @export
print.null_coord <- function(x, ...) {
  cat(paste0(x$position, "axis's information:\n"))

  cat("Uninitialized...\n")
}

#' @export
print.continuous_coord <- function(x, width = 60, ...) {
  cat(paste0(x$position, "axis's information:\n"))

  cat_line("continuous coordinate", "Type")
  cat_line(round(x$limits, 3), "Limits", " => ", width = width)
  cat_line(if (is.null(x$breaks)) "NULL" else round(x$breaks, 3), "Breaks", ", ",
           width = width)
  cat_line(x$labels, "Labels", ", ", width = width)
  cat_line(round(x$range, 3), "Range", " => ", width = width)
}

#' @export
print.discrete_coord <- function(x, width = 60, ...) {
  cat(paste0(x$position, "axis's information:\n"))

  cat_line("discrete coordinate", "Type")
  cat_line(x$limits, "Limits", ", ", width = width)
  cat_line(if (is.null(x$breaks)) "NULL" else round(x$breaks, 3), "Breaks", ", ",
           width = width)
  cat_line(x$labels, "Labels", ", ", width = width)
  cat_line(round(x$range, 3), "Range", " => ", width = width)
}

#' @export
print.PANEL <- function(x, ...) {
  cat("--------PANEL's information:--------\n")
  print(x$x, ...)
  cat("\n")
  print(x$y, ...)
  cat("------------------------------------\n")
}

#' @noRd
x_is_NULLcoord <- function(x) {
  inherits(x$x, "null_coord")
}

#' @noRd
y_is_NULLcoord <- function(x) {
  inherits(x$y, "null_coord")
}

#' @noRd
any_is_NULLcoord <- function(x) {
  inherits(x$x, "null_coord") || inherits(x$y, "null_coord")
}

#' @noRd
all_is_NULLcoord <- function(x) {
  inherits(x$x, "null_coord") && inherits(x$y, "null_coord")
}

#' @noRd
is_discrete <- function(x) {
  inherits(x, "discrete_coord")
}

#' @noRd
is_PANEL <- function(x) {
  inherits(x, "PANEL")
}

#' @noRd
is_CELL <- function(x) {
  inherits(x, "CELL")
}

