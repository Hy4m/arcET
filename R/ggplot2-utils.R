#' @importFrom utils getFromNamespace
#' @noRd
flip_data <- getFromNamespace("flip_data", ns = "ggplot2")

#' @noRd
compute_just <- getFromNamespace("compute_just", ns = "ggplot2")

#' @noRd
plot_theme <- getFromNamespace("plot_theme", ns = "ggplot2")

#' @noRd
stairstep <- getFromNamespace("stairstep", ns = "ggplot2")

#' @noRd
justify_grobs <- getFromNamespace("justify_grobs", ns = "ggplot2")

#' @noRd
translate_shape_string <- getFromNamespace("translate_shape_string", ns = "ggplot2")

#' @noRd
check_linewidth <- getFromNamespace("check_linewidth", ns = "ggplot2")

#' @noRd
parse_safe <- getFromNamespace("parse_safe", "ggplot2")

#' @noRd
ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

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
             "family", "fontface", "lineheight", "outside")

#' Extract ggplot elements
#' @description Internal function to extract ggplot elements.
#' @param plot a ggplot object.
#' @param ... not used.
#' @return elements list of plot.
#' @rdname extract_ggplot
#' @author Hou Yun
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

  labels <- plot$plot$labels
  panel_params <- panel_params[[1]]
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
  if (flipped_aes) {
    labels <- rename(labels, x = "y", y = "x")
  }
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
                 labels = labels,
                 trans = trans(),
                 coord = coord,
                 theme = plot_theme(plot$plot),
                 guides = guides), class = "gg_element")
}

#' @export
print.gg_element <- function(x, ...) {
  layers <- tibble::tibble(layer = x$layer_name,
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

#' @param layer layers of ggplot object.
#' @rdname extract_ggplot
#' @export
extract_params <- function(layer, ...) {
  UseMethod("extract_params")
}

#' @rdname extract_ggplot
#' @export
extract_params.default <- function(layer, ...) {
  out <- layer$computed_geom_params
  clss <- class(layer$geom)[1]
  if (clss %in% c("GeomRoundrect", "GeomRoundtile", "GeomRoundbar",
                  "GeomRoundcol", "GeomRoundpolygon", "GeomShape")) {
    out[setdiff(names(out), all_aes[all_aes != "radius"])]
  } else {
    out[setdiff(names(out), all_aes)]
  }
}

#' @rdname extract_ggplot
#' @export
extract_guides <- function(plot, ...) {
  UseMethod("extract_guides")
}

#' @rdname extract_ggplot
#' @export
extract_guides.default <- function(plot, ...) {
  if (!inherits(plot, "ggplot_built")) {
    plot <- ggplot_build(plot)
  }

  plot <- ggplot2::ggplot_gtable(plot)
  plot$grob[[match("guide-box", plot$layout$name)]]
}

#' @noRd
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
              position = panel_params$x$position,
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
              position = panel_params$y$position,
              range = c(0, 1),
              data.range = y_range,
              expand = NULL,
              drop = FALSE)

  structure(list(x = x, y = y), class = "PANEL")
}

## modify from ggplot2::element_grob()
#' Create arc grob from theme element
#'
#' @param element Theme element, i.e. `element_rect` or similar.
#' @param ... Other arguments to control specific of rendering. This is
#'   usually at least position. See the source code for individual methods.
#' @keywords internal
#' @export
arc_element_grob <- function(element, ...) {
  UseMethod("arc_element_grob")
}

#' @export
arc_element_grob.element_blank <- function(element, ...) zeroGrob()

#' @export
arc_element_grob.element_line <- function(element,
                                          x = 360,
                                          y = 0.5,
                                          xend = 0,
                                          yend = 0.5,
                                          colour = NULL,
                                          linewidth = NULL,
                                          linetype = NULL,
                                          lineend = NULL,
                                          linejoin = "round",
                                          linemitre = 10,
                                          steps = 0.01,
                                          simplify = FALSE,
                                          ...,
                                          size) {
  if (!missing(size)) {
    linewidth <- size
  }

  # The gp settings can override element_gp
  gp <- gpar(col = colour,
             fill = colour,
             lwd = len0_null(linewidth),
             lty = linetype,
             lineend = lineend)
  element_gp <- gpar(col = element$colour,
                     fill = element$colour,
                     lwd = len0_null(element$linewidth),
                     lty = element$linetype,
                     lineend = element$lineend)
  gp <- unclass(modify_list(element_gp, gp))
  gp <- rename(gp, "colour" = "col", "linewidth" = "lwd", "linetype" = "lty")

  arrow <- if (is.logical(element$arrow) && !element$arrow) {
    NULL
  } else {
    element$arrow
  }

  exec(ArcSegmentsGrob,
       x = x,
       y = y,
       xend = xend,
       yend = yend,
       !!!gp,
       arrow = arrow,
       linejoin = linejoin,
       linemitre = linemitre,
       steps = steps,
       simplify = simplify,
       ...)
}

#' @export
arc_element_grob.element_rect <- function(element,
                                          xmin = 120,
                                          ymin = 0.5,
                                          xmax = 60,
                                          ymax = 1,
                                          colour = NULL,
                                          fill = NULL,
                                          linewidth = NULL,
                                          linetype = NULL,
                                          lineend = "butt",
                                          linejoin = "round",
                                          linemitre = 10,
                                          steps = 0.01,
                                          simplify = FALSE,
                                          ...,
                                          size) {
  if (!missing(size)) {
    linewidth <- size
  }

  # The gp settings can override element_gp
  gp <- gpar(col = colour,
             fill = fill,
             lwd = len0_null(linewidth),
             lty = linetype)
  element_gp <- gpar(col = element$colour,
                     fill = element$fill,
                     lwd = len0_null(element$linewidth),
                     lty = element$linetype)

  gp <- unclass(modify_list(element_gp, gp))
  gp <- rename(gp, "colour" = "col", "linewidth" = "lwd", "linetype" = "lty")

  exec(ArcRectGrob,
       xmin = xmin,
       ymin = ymin,
       xmax = xmax,
       ymax = ymax,
       !!!gp,
       lineend = lineend,
       linejoin = linejoin,
       linemitre = linemitre,
       steps = steps,
       simplify = simplify,
       ...)
}

#' @noRd
arc_guide_grid <- function(theme,
                           x.minor = NULL,
                           x.major = NULL,
                           y.minor = NULL,
                           y.major = NULL,
                           region = CELL()) {
  x.minor <- setdiff(x.minor, x.major)
  y.minor <- setdiff(y.minor, y.major)
  x.range <- region$x.range
  y.range <- region$y.range

  panel.grid.minor.y <- calc_element("panel.grid.minor.y", theme)
  panel.grid.major.y <- calc_element("panel.grid.major.y", theme)
  panel.grid.minor.x <- calc_element("panel.grid.minor.x", theme)
  panel.grid.major.x <- calc_element("panel.grid.major.x", theme)

  grob <- grid::gTree(
    children = gList(if (length(y.minor) > 0) arc_element_grob(panel.grid.minor.y,
                                                               x = x.range[1],
                                                               xend = x.range[2],
                                                               y = y.minor,
                                                               yend = y.minor),
                     if (length(x.minor) > 0) arc_element_grob(panel.grid.minor.x,
                                                               x = x.minor,
                                                               xend = x.minor,
                                                               y = y.range[1],
                                                               yend = y.range[2]),
                     if (length(y.major) > 0) arc_element_grob(panel.grid.major.y,
                                                               x = x.range[1],
                                                               xend = x.range[2],
                                                               y = y.major,
                                                               yend = y.major),
                     if (length(x.major) > 0) arc_element_grob(panel.grid.major.x,
                                                               x = x.major,
                                                               xend = x.major,
                                                               y = y.range[1],
                                                               yend = y.range[2])),
    name = "grill"
  )
}
