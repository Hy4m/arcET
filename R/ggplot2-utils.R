#' @noRd
flip_data <- getFromNamespace("flip_data", ns = "ggplot2")

#' @noRd
compute_just <- getFromNamespace("compute_just", ns = "ggplot2")

#' @noRd
plot_theme <- getFromNamespace("plot_theme", ns = "ggplot2")

#' @noRd
x_aes <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
           "xmax_final", "xlower", "xmiddle", "xupper", "x0")

#' @noRd
y_aes <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
           "ymax_final", "lower", "middle", "upper", "y0")

#' @noRd
all_aes <- c("adj", "alpha", "angle", "bg", "cex", "col", "color",
             "colour", "fg", "fill", "group", "hjust", "label", "linetype",
             "linewidth", "lower", "lty", "lwd", "max", "middle", "min",
             "pch", "radius", "sample", "shape", "size", "srt", "upper",
             "vjust", "weight", "width", "x", "xend", "xmax", "xmin",
             "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z",
             "family", "fontface", "lineheight", "outside", "image")

#' @importFrom ggplot2 ggplot_build
#' @export
extract_ggplot <- function(plot) {
  if (!inherits(plot, "ggplot_built")) {
    plot <- ggplot_build(plot)
  }

  if (inherits(plot$plot$coordinates, "CoordSf")) {
    cli::cli_warn("{CellPlot don't support CoordSf.")
  }
  layers <- plot$plot$layers
  panel_params <- plot$layout$panel_params
  if (length(panel_params) > 1) {
    cli::cli_warn("{CellPlot don't support facet.")
  }
  panel_params <- panel_params[[1]]

  data <- plot$data
  trans <- function() {
    parameters <- force(panel_params)
    FUN <- force(plot$plot$coordinates$transform)
    function(data) {
      FUN(data = data, panel_params = parameters)
    }
  }
  layer_name <- vapply_chr(layers, function(x) class(x$geom)[1])

  coord <- gg2coord(panel_params)
  layer_params <- lapply(layers, extract_params)
  guides <- tryCatch(extract_guides(plot), error = function(e) NULL)

  ## handle clip param
  flipped_aes <- inherits(plot$plot$coordinates, "CoordFlip")
  clip <- plot$plot$coordinates$clip
  if (is.character(clip)) {
    clip <- switch (clip,
      "on" = TRUE,
      FALSE
    )
  }

  for (ii in seq_along(layer_params)) {
    layer_params[[ii]]$flipped_aes <- flipped_aes
    layer_params[[ii]]$clip <- clip
  }

  structure(list(data = plot$data,
                 layer_name = layer_name,
                 layer_params = layer_params,
                 trans = trans(),
                 coord = coord,
                 theme = plot_theme(plot$plot),
                 guides = guides), class = "gg_element")
}

#' @export
print.gg_element <- function(x, ...) {
  layers <- data_frame0(layer = x$layer_name,
                        data = x$data,
                        parameters = x$layer_params)

  cli::cli_h1("--------elements of ggplot:--------")
  cat("Layer data: ")
  print(layers, ...)
  cat("\n")
  cli::cli_inform("trans: {.cls {class(x$trans)[1]}}")
  cat("\n")
  cat("coord:\n")
  print(x$coord)
  cli::cli_h1("------------------------------------")
}

#' @export
extract_params <- function(layer, ...) {
  UseMethod("extract_params")
}

#' @export
extract_params.default <- function(layer, ...) {
  out <- layer$computed_geom_params
  out[setdiff(names(out), all_aes)]
}

#' @export
extract_guides <- function(plot, ...) {
  UseMethod("extract_guides")
}

#' @export
extract_guides.default <- function(plot, ...) {
  if (!inherits(plot, "ggplot_built")) {
    plot <- ggplot_build(plot)
  }

  plot <- ggplot2::ggplot_gtable(plot)
  plot$grob[[match("guide-box", plot$layout$name)]]
}

#' @export
gg2coord <- function(panel_params) {
  x_limits <- panel_params$x$limits
  x_breaks <- panel_params$x$breaks
  x_minor_breaks <- panel_params$x$minor_breaks
  x_labels <- panel_params$x$get_labels()
  x_range <- panel_params$x.range

  if (is.numeric(x_limits)) {
    x_limits <- scales::rescale(x_limits, to = c(0, 1), from = x_range)
  }
  if (!is.null(x_breaks)) {
    if (is.character(x_breaks)) {
      x_breaks <- as.integer(factor(x_breaks, levels = x_limits))
    }

    x_breaks <- scales::rescale(x_breaks, to = c(0, 1), from = x_range)
  }
  if (!is.null(x_minor_breaks)) {
    if (is.character(x_minor_breaks)) {
      x_minor_breaks <- as.integer(factor(x_minor_breaks, levels = x_limits))
    }

    x_minor_breaks <- scales::rescale(x_minor_breaks, to = c(0, 1), from = x_range)
  }

  x <- .PANEL(limits = x_limits,
              breaks = x_breaks,
              minor_breaks = x_minor_breaks,
              labels = x_labels,
              range = c(0, 1),
              data.range = x_range,
              expand = NULL,
              drop = FALSE)

  y_limits <- panel_params$y$limits
  y_breaks <- panel_params$y$breaks
  y_minor_breaks <- panel_params$y$minor_breaks
  y_labels <- panel_params$y$get_labels()
  y_range <- panel_params$y.range

  if (is.numeric(y_limits)) {
    y_limits <- scales::rescale(y_limits, to = c(0, 1), from = y_range)
  }
  if (!is.null(y_breaks)) {
    if (is.character(y_breaks)) {
      y_breaks <- as.integer(factor(y_breaks, levels = y_limits))
    }
    y_breaks <- scales::rescale(y_breaks, to = c(0, 1), from = y_range)
  }
  if (!is.null(y_minor_breaks)) {
    if (is.character(y_minor_breaks)) {
      y_minor_breaks <- as.integer(factor(y_minor_breaks, levels = y_limits))
    }

    y_minor_breaks <- scales::rescale(y_minor_breaks, to = c(0, 1), from = y_range)
  }

  y <- .PANEL(limits = y_limits,
              breaks = y_breaks,
              minor_breaks = y_minor_breaks,
              labels = y_labels,
              range = c(0, 1),
              data.range = y_range,
              expand = NULL,
              drop = FALSE)

  structure(list(x = x, y = y), class = "PANEL")
}

#' @export
arc_test <- function(plot = ggplot2::last_plot(),
                     region = NULL,
                     ...) {
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a ggplot object.")
  }
  if (!inherits(plot$facet, "FacetNull")) {
    cli::cli_abort("Facet's plot has not be implemented yet.")
  }

  clss <- class(plot)[1]
  region <- region %||% CELL(120, 60, 0.4)
  plot <- init_cell(arcplot(), data = plot, region = region)

  plot <- tryCatch(ArcPlot_build(plot, ...),
                   error = function(e) {
                     "Connot convert {.cls {clss}} to arcplot..."
                   })

  grid::grid.newpage()
  grid::grid.draw(plot)

  invisible()
}
