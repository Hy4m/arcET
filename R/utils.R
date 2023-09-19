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
  vapply_dbl(split(data, seq_len(nrow(data))), function(d) {
    if (identical0(d$x, d$xend) && identical0(d$y, d$yend)) {
      0
    } else if (identical0(d$y, d$yend)) {
      radian(abs(d$x - d$xend)) * abs(d$y)
    } else {
      temp <- xs <- seq(d$x, d$xend, length.out = 200)
      ys <- seq(d$y, d$yend, length.out = 200)

      xs <- cos(radian(temp)) * ys
      ys <- sin(radian(temp)) * ys
      sum(euclid_dist(xs[-200], ys[-200], xs[-1], ys[-1]))
    }
  })
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
identical0 <- function(x, y) {
  abs(x - y) < 10^(-16)
}

#' @noRd
modify_aes <- function(aes, aes2) {
  aes <- as.list(aes)
  aes2 <- as.list(aes2)
  aes <- utils::modifyList(aes, aes2)
  class(aes) <- "uneval"
  aes
}

#' @noRd
match_ids <- function(x, y, fixed = TRUE, ignore.case = FALSE) {
  if (is.null(x) || is.null(y) || length(x) == 0 || length(y) == 0) {
    return(integer(0))
  }

  if (isTRUE(fixed)) {
    ids <- vctrs::vec_match(x, y)
    ids <- unique(ids[!is.na(ids)])
  } else {
    ids <- lapply(x, function(pattern) {
      lgl <- grepl(pattern = pattern, x = y, ignore.case = ignore.case)
      ifelse(is.na(lgl), FALSE, lgl)
    })

    ids <- Reduce("|", ids, init = FALSE)
    ids <- seq_along(y)[ids]
  }

  ids
}

#' @noRd
len0_null <- function(x) {
  if (length(x) == 0) NULL else x
}

#' @noRd
modify_list <- function(x, y) {
  for (ii in names(y)) {
    x[[ii]] <- y[[ii]]
  }

  x
}

#' @noRd
check_required <- function(needness,
                           data = NULL,
                           reason = NULL) {
  if (!all(needness %in% names(data))) {
    cli::cli_abort(c("Require {.var {needness}} {reason}",
                     "the fellowing required variables is missing: {.var {setdiff(needness, names(data))}}."))
  }
}
