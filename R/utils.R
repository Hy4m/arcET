#' @noRd
`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

#' @noRd
radian <- function(x) x / 180 * pi

#' @noRd
degree <- function(x) x * 180 / pi

#' @noRd
euclid_dist <- function(x, y, xend, yend) {
  sqrt((xend - x)^2 + (yend - y)^2)
}

#' @noRd
euclid_dist2 <- function(x, y, xend, yend) {
  data <- data_frame0(x = x,
                      y = y,
                      xend = xend,
                      yend = yend)
  vapply(split(data, seq_len(nrow(data))), function(d) {
    if (!stats::complete.cases(d)) {
      return(NA)
    }
    if (identical0(d$x, d$xend) && identical0(d$y, d$yend)) {
      0
    } else if (identical0(d$y, d$yend)) {
      radian(abs(d$x - d$xend)) * abs(d$y)
    } else {
      angle <- radian(abs(d$x - d$xend))
      sqrt((d$y - d$yend)^2 + angle * abs(d$y) * angle * abs(d$yend))
    }
  }, numeric(1), USE.NAMES = FALSE)
}

#' @noRd
empty <- function(x) {
  if (length(x) == 0) {
    return(TRUE)
  }

  if (inherits(x, "CELL")) {
    x$r1 == x$r0
  } else {
    if (is.data.frame(x) || is.matrix(x)) {
      nrow(x) == 0 || ncol(x) == 0
    } else {
      length(x) == 0
    }
  }
}

#' @noRd
rand_chr <- function (n, max_len = 10, min_len = 2, lambda = 5)
{
  ll <- stats::rpois(n, lambda)
  ll <- ifelse(ll > max_len, ll %% max_len, ll)
  ll <- ifelse(ll < min_len, min_len, ll)
  vapply_chr(ll, function(n) {
    paste(sample(letters, n, TRUE), collapse = "")
  })
}

#' @noRd
parse_safe <- getFromNamespace("parse_safe", "ggplot2")

#' @noRd
exchange <- function(x, y, env = parent.frame()) {
  x_name <- rlang::as_name(rlang::enquo(x))
  y_name <- rlang::as_name(rlang::enquo(y))
  temp <- x
  assign(x_name, y, envir = env)
  assign(y_name, temp, envir = env)
}

#' @noRd
count_by_group <- function(data) {
  if (empty(data)) {
    return(integer(0))
  }

  if (!"subgroup" %in% names(data)) {
    stats::ave(seq_len(nrow(data)), data$group, FUN = length)
  } else {
    stats::ave(seq_len(nrow(data)), data$group, data$subgroup,
               FUN = length)
  }
}

#' @noRd
pretty2 <- function (x) {
  if (length(x) < 2) {
    x <- rep(x, 2)
  }

  rng <- range(x, na.rm = TRUE)
  bb <- pretty(x)
  bb[bb >= rng[1] & bb <= rng[2]]
}

#' @noRd
cat_line <- function(msg,
                     prefix = NULL,
                     sep = ",",
                     width = 60,
                     msg_of_null = "NULL",
                     digits = 3) {
  msg <- msg %||% msg_of_null
  if (!is.character(msg)) {
    if (is.numeric(msg)) {
      msg <- as.character(round(msg, digits = digits))
    } else {
      msg <- as.character(msg)
    }
  }
  prefix <- prefix %||% ""
  if (prefix != "") {
    prefix <- paste0(prefix, ":")
  }

  cat(prefix, glue::glue_collapse(msg, sep = sep, width = width), "\n")
}

#' @noRd
data_frame0 <- function(...) {
  vctrs::data_frame(..., .name_repair = "minimal")
}


#' @noRd
rename <- function (data, ...) {
  nm <- names(data)
  ll <- rlang::list2(...)
  ll <- ll[unlist(ll) %in% nm]

  if (length(ll) >= 0) {
    old <- unname(unlist(ll))
    new <- names(ll)

    nm[match(old, nm)] <- new
    names(data) <- nm
  }
  data
}


#' @noRd
split_by_rows <- function(data) {
  if (empty(data)) {
    return(data)
  }

  split(data, 1:nrow(data))
}

#' @noRd
lapply_dfr <- function(x, FUN, ...) {
  data <- lapply(x, FUN, ...)

  vec_rbind0(!!!data)
}

#' @noRd
lapply_dfc <- function(x, FUN, ...) {
  data <- lapply(x, FUN, ...)

  vec_cbind0(!!!data)
}

#' @noRd
vapply_dbl <- function(x, FUN, ..., USE.NAMES = FALSE) {
  vapply(X = x, FUN = FUN, FUN.VALUE = numeric(1),
         USE.NAMES = USE.NAMES, ...)
}

#' @noRd
vapply_lgl <- function(x, FUN, ..., USE.NAMES = FALSE) {
  vapply(X = x, FUN = FUN, FUN.VALUE = logical(1),
         USE.NAMES = USE.NAMES, ...)
}

#' @noRd
vapply_chr <- function(x, FUN, ..., USE.NAMES = FALSE) {
  vapply(X = x, FUN = FUN, FUN.VALUE = character(1),
         USE.NAMES = USE.NAMES, ...)
}

#' @noRd
apply_at <- function(x, at, FUN, ...) {
  if (empty(x)) {
    return(x)
  }
  for (ii in at) {
    x[[ii]] <- FUN(x[[ii]], ...)
  }

  x
}

#' @noRd
apply_if <- function(x, test, FUN, ...) {
  if (empty(x) || !any(test)) {
    return(x)
  }

  ids <- which(test)
  apply_at(x, at = ids, FUN = FUN, ...)
}

#' @noRd
count <- function(data, ...) {
  if (empty(data)) {
    return(data)
  }

  dots <- rlang::enquos(...)
  dots <- lapply(dots, rlang::eval_tidy, data = data)
  rlang::inject(stats::ave(1:nrow(data), !!!dots, FUN = length))
}

#' @noRd
vec_rbind0 <- function(...,
                       .ptype = NULL,
                       .names_to = rlang::zap(),
                       .name_repair = c("unique", "universal", "check_unique",
                                        "unique_quiet", "universal_quiet"),
                       .name_spec = NULL,
                       .error_call = current_env()) {
  dots <- rlang::list2(...)
  vctrs::vec_rbind(!!!dots, .ptype = .ptype, .names_to = .names_to,
                   .name_repair = .name_repair, .name_spec = .name_spec,
                   .error_call = .error_call)
}

#' @noRd
vec_cbind0 <- function(...,
                       .ptype = NULL,
                       .size = NULL,
                       .name_repair = c("unique", "universal", "check_unique",
                                        "unique_quiet", "universal_quiet"),
                       .name_spec = NULL,
                       .error_call = current_env()) {
  dots <- rlang::list2(...)
  vctrs::vec_cbind(!!!dots, .ptype = .ptype, .size = .size,
                   .name_repair = .name_repair, .name_spec = .name_spec,
                   .error_call = .error_call)
}


#' @noRd
normalize <- function (data) {
  if (empty(data)) {
    return(data)
  }

  x <- data$x
  y <- data$y
  rng_x <- range(x, na.rm = TRUE)
  rng_y <- range(y, na.rm = TRUE)
  if (any(is.infinite(rng_x), is.infinite(rng_y))) {
    cli::cli_warn("Contain infinite value.")
    return(data)
  }

  if (diff(rng_x) == 0) {
    x <- 0.5
    y <- scales::rescale(y, c(0, 1), rng_y)
  } else if (diff(rng_y) == 0) {
    x <- scales::rescale(x, c(0, 1), rng_x)
    y <- 0.5
  } else {
    ratio <- diff(rng_x)/diff(rng_y)
    if (ratio > 1) {
      x <- scales::rescale(x, c(0, 1), rng_x)
      y <- scales::rescale(y, c(0, 1)/ratio, rng_y)
    } else {
      x <- scales::rescale(x, c(0, 1) * ratio, rng_x)
      y <- scales::rescale(y, c(0, 1), rng_y)
    }
  }

  data_frame0(x = x, y = y, data[setdiff(names(data), c("x", "y"))])
}

#' @noRd
identical0 <- function(x, y) {
  abs(x - y) < 10^(-16)
}

diff_degree <- function(x, y) {
  (x %% 360) - (y %% 360)
}

#' @noRd
safe_mode <- function(x, zero = FALSE) {
  if (isTRUE(zero)) {
    x %% 360
  } else {
    ifelse(x == 360, x, x %% 360)
  }
}

#' @noRd
adjust_margin <- function(x, height, upper = NULL, lower = NULL, arc = FALSE) {
  if (length(x) <= 1) {
    return(x)
  }

  if (is.null(upper)) {
    if (isTRUE(arc)) {
      upper <- 360
    } else {
      upper <- 1
    }
  }
  if (is.null(lower)) {
    if (isTRUE(arc)) {
      lower <- 0
    } else {
      lower <- 0
    }
  }

  if (sum(height) > (upper - lower)) {
    if (isTRUE(arc)) {
      height <- height/sum(height)*(upper - lower)
    } else {
      upper <- upper + (sum(height) - upper + lower)/2
      lower <- lower - (sum(height) - upper + lower)/2
    }
  }

  n <- length(x)
  top <- c(upper, x[-n]) - x - c(height[1]/2, (height[-n] + height[-1])/2)
  less_than_zero <- top < 0
  top_new <- ifelse(less_than_zero, top, 0)
  x <- x + cumsum(top_new)
  top[less_than_zero] <- 0

  space_t <- upper - x[1] - height[1]/2
  space_b <- x[n] - lower - height[n]/2
  if (space_b < 0) {
    if (space_t >= -space_b) {
      x <- x - space_b
    } else {
      space <- cumsum(top)
      id <- which(space > -space_b)
      if (empty(id)) {
        x[1:n] <- x[1:n] + space
      } else {
        id <- id[1]
        if (id == 1) {
          x[2:n] <- x[2:n] - space_b
        } else {
          space[id] <- -space_b
          x[1:id] <- x[1:id] + space[1:id]

          if (id == n) {
            x[n] <- x[n] - space_b
          } else {
            x[(id + 1):n] <- x[(id + 1):n] - space_b
          }
        }
      }
    }
  }

  x
}

#' @noRd
modify_aes <- function(aes, aes2) {
  aes <- as.list(aes)
  aes2 <- as.list(aes2)
  aes <- utils::modifyList(aes, aes2)
  class(aes) <- "uneval"
  aes
}
