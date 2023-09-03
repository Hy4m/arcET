#' Mapping Functions
#' @description  Helper Functions for Mapping.
#' @param x numeric or character vector, which will be mapped to aesthetic.
#' @inheritParams ggplot2::scale_colour_manual
#' @inheritParams scales::col_numeric
#' @inheritParams scales::area_pal
#' @param ... other parameters.
#' @return a mapped_aes vector.
#' @rdname mapping
#' @author Hou Yun
#' @export
map_colour <- function(x,
                       palette = NULL,
                       domain = NULL,
                       ...) {
  if (is.numeric(x)) {
    palette <- palette %||% c("blue", "white", "red")
    domain <- domain %||% range(x, na.rm = TRUE)
    pal <- scales::col_numeric(palette = palette,
                               domain = domain,
                               ...)
  } else {
    if (!is.factor(x)) {
      x <- as.factor(x)
    }

    if (is.null(palette)) {
      if (length(levels(x)) <= 8) {
        palette <- "Set2"
      } else {
        palette <- colorspace::qualitative_hcl(length(levels(x)))
      }
    }
    domain <- domain %||% levels(x)
    pal <- scales::col_factor(palette = palette,
                              domain = domain,
                              ...)
  }

  out <- pal(x)
  class(out) <- c("mapped_aes", class(out))
  out
}

#' @rdname mapping
#' @export
map_color <- map_colour

#' @rdname mapping
#' @export
map_colour_manual <- function(x,
                              values = NULL,
                              na.value = "grey60",
                              ...) {
  if (is.double(x)) {
    cli::cli_abort("`map_colour_manual()` don't support continuous scale.")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  limits <- unique(x, incomparables = TRUE)
  if (is.null(values)) {
    if (length(limits) <= 8) {
      palette <- RColorBrewer::brewer.pal(length(limits), "Set2")
    } else {
      palette <- colorspace::qualitative_hcl(length(limits))
    }

    values <- rlang::set_names(palette, limits)
  }

  x <- unname(values[x])
  x[is.na(x)] <- na.value

  class(x) <- c("mapped_aes", class(x))
  x
}

#' @rdname mapping
#' @export
map_color_manaul <- map_colour_manual

#' @rdname mapping
#' @export
map_fill <- function(x,
                     palette = NULL,
                     domain = NULL,
                     ...) {
  if (is.numeric(x)) {
    palette <- palette %||% c("blue", "white", "red")
    domain <- domain %||% range(x, na.rm = TRUE)
    pal <- scales::col_numeric(palette = palette,
                               domain = domain,
                               ...)
  } else {
    if (!is.factor(x)) {
      x <- as.factor(x)
    }

    if (is.null(palette)) {
      if (length(levels(x)) <= 8) {
        palette <- "Set2"
      } else {
        palette <- colorspace::qualitative_hcl(length(levels(x)))
      }
    }
    domain <- domain %||% levels(x)
    pal <- scales::col_factor(palette = palette,
                              domain = domain,
                              ...)
  }

  out <- pal(x)
  class(out) <- c("mapped_aes", class(out))
  out
}

#' @rdname mapping
#' @export
map_fill_manual <- function(x,
                            values = NULL,
                            na.value = "grey60",
                            ...) {
  if (is.double(x)) {
    cli::cli_abort("`map_fill_manual()` don't support continuous scale.")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  limits <- unique(x, incomparables = TRUE)
  if (is.null(values)) {
    if (length(limits) <= 8) {
      palette <- RColorBrewer::brewer.pal(length(limits), "Set2")
    } else {
      palette <- colorspace::qualitative_hcl(length(limits))
    }

    values <- rlang::set_names(palette, limits)
  }

  x <- unname(values[x])
  x[is.na(x)] <- na.value

  class(x) <- c("mapped_aes", class(x))
  x
}

#' @rdname mapping
#' @export
map_shape <- function(x, ...) {
  if (is.double(x)) {
    cli::cli_abort("A continuous variable cannot be mapped to the shape.")
  }

  if (!is.factor(x)) {
    x <- as.factor(x)
  }

  n <- length(levels(x))
  x <- as.integer(x)

  pal <- scales::shape_pal(solid = TRUE)(n)

  x <- pal[x]
  class(x) <- c("mapped_aes", class(x))

  x
}

#' @rdname mapping
#' @export
map_shape_manual <- function(x,
                             values = NULL,
                             na.value = NA,
                             ...) {
  if (is.double(x)) {
    cli::cli_abort("`map_shape_manual()` don't support continuous scale.")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  limits <- unique(x, incomparables = TRUE)
  if (is.null(values)) {
    shape <- scales::shape_pal(solid = TRUE)(length(limits))
    values <- rlang::set_names(shape, limits)
  }

  x <- unname(values[x])
  x[is.na(x)] <- na.value

  class(x) <- c("mapped_aes", class(x))
  x
}

#' @rdname mapping
#' @export
map_linetype <- function(x, ...) {
  if (is.numeric(x)) {
    cli::cli_abort("A continuous variable cannot be mapped to the linetype.")
  }

  if (!is.factor(x)) {
    x <- as.factor(x)
  }

  n <- length(levels(x))
  x <- as.integer(x)

  pal <- scales::linetype_pal()(n)

  x <- pal[x]
  class(x) <- c("mapped_aes", class(x))

  x
}

#' @rdname mapping
#' @export
map_linetype_manual <- function(x,
                                values = NULL,
                                na.value = NA,
                                ...) {
  if (is.double(x)) {
    cli::cli_abort("`map_linetype_manual()` don't support continuous scale.")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  limits <- unique(x, incomparables = TRUE)
  if (is.null(values)) {
    linetype <- scales::linetype_pal()(length(limits))
    values <- rlang::set_names(linetype, limits)
  }

  x <- unname(values[x])
  x[is.na(x)] <- na.value

  class(x) <- c("mapped_aes", class(x))
  x
}

#' @rdname mapping
#' @export
map_size <- function(x, range = c(1, 6), ...) {
  pal <- scales::area_pal(range = range)
  if (!is.numeric(x)) {
    if (!is.factor(x)) {
      x <- as.factor(x)
    }
    x <- as.integer(x)
  }

  x <- pal(scales::rescale(x, c(0, 1)))
  class(x) <- c("mapped_aes", class(x))

  x
}

#' @rdname mapping
#' @export
map_size_manual <- function(x,
                            values = NULL,
                            na.value = NA,
                            ...) {
  if (is.double(x)) {
    cli::cli_abort("`map_size_manual()` don't support continuous scale.")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  limits <- unique(x, incomparables = TRUE)
  if (is.null(values)) {
    values <- rlang::set_names(seq(limits), limits)
  }

  x <- unname(values[x])
  x[is.na(x)] <- na.value

  class(x) <- c("mapped_aes", class(x))
  x
}

#' @rdname mapping
#' @export
map_linewidth <- function(x, range = c(0.1, 1), ...) {
  pal <- scales::rescale_pal(range = range)
  if (!is.numeric(x)) {
    if (!is.factor(x)) {
      x <- as.factor(x)
    }
    x <- as.integer(x)
  }

  x <- pal(scales::rescale(x, c(0, 1)))
  class(x) <- c("mapped_aes", class(x))

  x
}

#' @rdname mapping
#' @export
map_linewidth_manual <- function(x,
                                 values = NULL,
                                 na.value = NA,
                                 ...) {
  if (is.double(x)) {
    cli::cli_abort("`map_linewidth_manual()` don't support continuous scale.")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  limits <- unique(x, incomparables = TRUE)
  if (is.null(values)) {
    values <- rlang::set_names(seq(limits), limits)
  }

  x <- unname(values[x])
  x[is.na(x)] <- na.value

  class(x) <- c("mapped_aes", class(x))
  x
}
