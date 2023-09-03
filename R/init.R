#' @export
print.ArcPlot <- function(x, ..., verbose = FALSE) {
  if (isTRUE(verbose)) {
    NextMethod()
  }

  grobs <- ArcPlot_build(plot = x, ...)
  grid::grid.newpage()
  grid::grid.draw(grobs)
  invisible()
}

#' @export
plot.ArcPlot <- function(x, ...) {
  grobs <- ArcPlot_build(plot = x, ...)
  grid::grid.newpage()
  grid::grid.draw(grobs)
  invisible()
}

#' @export
ArcPlot_build <- function(plot, ...) {
  UseMethod("ArcPlot_build")
}

#' @importFrom ggplot2 theme_get
#' @export
ArcPlot_build.ArcPlot <- function(plot,
                                  theme = theme_get(),
                                  title = NULL,
                                  xlim = NULL,
                                  ylim = NULL,
                                  ...) {
  if (nrow(plot) < 1) {
    return(zeroGrob)
  }

  theme <- theme %||% attr(plot, "theme")
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  } else {
    theme <- ggplot2::theme_get() %+% theme
  }

  grobs <- list()
  legends <- list()

  for (row in split(plot, 1:nrow(plot))) {
    cli::cli_inform("Build {row$CellID} plot...")
    region <- row$region[[1]]
    p <- suppressWarnings(ggplot2::ggplot_build(row$plot[[1]]))

    params <- tryCatch(extract_ggplot(p), error = function(e) NULL)
    if (is.null(params)) next

    theme <- params$theme
    coord <- params$coord
    guides <- params$guides
    layers <- lapply(seq_along(params$data), function(ii) {
      FUN <- tryCatch(match.fun(paste0(params$layer_name[ii], "2grob")),
                      error = function(e) "not_implement")

      if (identical(FUN, "not_implement")) {
        cli::cli_warn(c("{.fun {paste0(params$layer_name[ii], '2grob')}} has not been implemented yet,",
                        i = "so this layer has been omitted."))
        zeroGrob()
      } else {
        rlang::inject(FUN(data = params$data[[ii]], region = region, coord = coord,
                          trans = params$trans, !!!params$layer_params[[ii]]))
      }
    })

    if (inherits(theme$panel.background, "element_blank")) {
      panel <- list()
    } else {
      if (inherits(theme$panel.background$linewidth, "rel")) {
        theme$panel.background$linewidth <- theme$panel.background$linewidth/.pt
      }
      panel <- list(ArcPanelGrob(region = region,
                                 fill = theme$panel.background$fill %||% "grey20",
                                 colour = theme$panel.background$colour %||% NA,
                                 linewidth = theme$panel.background$linewidth %||% 0.5,
                                 linetype = theme$panel.background$linetype %||% 1))
    }

    panel.grid.major.x <- calc_element("panel.grid.major.x", theme) %||%
      calc_element("panel.grid.major", theme) %||%
      calc_element("panel.grid", theme)
    panel.grid.minor.x <- calc_element("panel.grid.minor.x", theme) %||%
      calc_element("panel.grid.minor", theme) %||%
      calc_element("panel.grid", theme)
    if (inherits(panel.grid.major.x$linewidth, "rel")) {
      panel.grid.major.x$linewidth <- panel.grid.major.x$linewidth/.pt
    }
    if (inherits(panel.grid.minor.x$linewidth, "rel")) {
      panel.grid.minor.x$linewidth <- panel.grid.minor.x$linewidth/.pt
    }
    if ((is.null(coord$x$breaks) || inherits(panel.grid.major.x, "element_blank")) &&
        (is.null(coord$x$minor_breaks) || inherits(panel.grid.minor.x, "element_blank"))) {
      grid_x <- list()
    } else {
      x_breaks <- coord$x$breaks
      x_minor_breaks <- coord$x$minor_breaks
      colour <- c(rep(panel.grid.major.x$colour %||% "white", length(x_breaks)),
                  rep(panel.grid.minor.x$colour %||% "white", length(x_minor_breaks)))
      linewidth <- c(rep(panel.grid.major.x$linewidth %||% 0.5, length(x_breaks)),
                     rep(panel.grid.minor.x$linewidth %||% 0.25, length(x_minor_breaks)))
      linetype <- c(rep(panel.grid.major.x$linetype %||% 1, length(x_breaks)),
                    rep(panel.grid.minor.x$linetype %||% 1, length(x_minor_breaks)))
      grid_x <- data_frame0(x = c(x_breaks, x_minor_breaks),
                            colour = colour,
                            linewidth = linewidth,
                            linetype = linetype)
      grid_x <- cartesian2polar(data = grid_x, coord = coord, region = region,
                                clip = TRUE, na.rm = TRUE)
      grid_x <- list(ArcVlineGrob(xintercept = grid_x$x,
                                  region = region,
                                  colour = grid_x$colour,
                                  linewidth = grid_x$linewidth,
                                  linetype = grid_x$linetype))
    }

    panel.grid.major.y <- calc_element("panel.grid.major.y", theme) %||%
      calc_element("panel.grid.major", theme) %||%
      calc_element("panel.grid", theme)
    panel.grid.minor.y <- calc_element("panel.grid.minor.y", theme) %||%
      calc_element("panel.grid.minor", theme) %||%
      calc_element("panel.grid", theme)
    if (inherits(panel.grid.major.y$linewidth, "rel")) {
      panel.grid.major.y$linewidth <- panel.grid.major.y$linewidth/.pt
    }
    if (inherits(panel.grid.minor.y$linewidth, "rel")) {
      panel.grid.minor.y$linewidth <- panel.grid.minor.y$linewidth/.pt
    }
    if ((is.null(coord$y$breaks) || inherits(panel.grid.major.y, "element_blank")) &&
        (is.null(coord$y$minor_breaks) || inherits(panel.grid.minor.y, "element_blank"))) {
      grid_y <- list()
    } else {
      y_breaks <- coord$y$breaks
      y_minor_breaks <- coord$y$minor_breaks
      colour <- c(rep(panel.grid.major.y$colour %||% "white", length(y_breaks)),
                  rep(panel.grid.minor.y$colour %||% "white", length(y_minor_breaks)))
      linewidth <- c(rep(panel.grid.major.y$linewidth %||% 0.5, length(y_breaks)),
                     rep(panel.grid.minor.y$linewidth %||% 0.25, length(y_minor_breaks)))
      linetype <- c(rep(panel.grid.major.y$linetype %||% 1, length(y_breaks)),
                    rep(panel.grid.minor.y$linetype %||% 1, length(y_minor_breaks)))
      grid_y <- data_frame0(y = c(y_breaks, y_minor_breaks),
                            colour = colour,
                            linewidth = linewidth,
                            linetype = linetype)
      grid_y <- cartesian2polar(data = grid_y, coord = coord, region = region,
                                clip = TRUE, na.rm = TRUE)
      grid_y <- list(ArcHlineGrob(yintercept = grid_y$y,
                                  region = region,
                                  colour = grid_y$colour,
                                  linewidth = grid_y$linewidth,
                                  linetype = grid_y$linetype))
    }

    if (p$layout$panel_params[[1]]$x$position == "none") {
      xaxis <- list()
    } else {
      position <- p$layout$panel_params[[1]]$x$position
      line.gp <- calc_element(paste0("axis.line.x.", position), theme) %||%
        calc_element("axis.line.x", theme) %||%
        calc_element("axis.line", theme)
      tick.gp <- calc_element(paste0("axis.line.x.", position), theme) %||%
        calc_element("axis.ticks.x", theme) %||%
        calc_element("axis.ticks", theme)
      ticks.length <- calc_element(paste0("axis.ticks.length.x.", position), theme) %||%
        calc_element("axis.ticks.length.x", theme) %||%
        calc_element("axis.ticks.length", theme)
      text.gp <- calc_element(paste0("axis.text.x.", position), theme) %||%
        calc_element("axis.text.x", theme) %||%
        calc_element("axis.text", theme)
      if (!is.null(text.gp$size)) {
        text.gp$size <- text.gp$size/.pt
      }

      xaxis <- list(ArcxAxisGrob(coord = coord,
                                 region = region,
                                 position = position,
                                 line.gp = line.gp,
                                 tick.gp = tick.gp,
                                 text.gp = text.gp,
                                 ticks.length = ticks.length))
    }

    if (p$layout$panel_params[[1]]$y$position == "none") {
      yaxis <- list()
    } else {
      position <- p$layout$panel_params[[1]]$y$position
      line.gp <- calc_element(paste0("axis.line.y.", position), theme) %||%
        calc_element("axis.line.y", theme) %||%
        calc_element("axis.line", theme)
      tick.gp <- calc_element(paste0("axis.line.y.", position), theme) %||%
        calc_element("axis.ticks.y", theme) %||%
        calc_element("axis.ticks", theme)
      ticks.length <- calc_element(paste0("axis.ticks.length.y.", position), theme) %||%
        calc_element("axis.ticks.length.y", theme) %||%
        calc_element("axis.ticks.length", theme)
      text.gp <- calc_element(paste0("axis.text.y.", position), theme) %||%
        calc_element("axis.text.y", theme) %||%
        calc_element("axis.text", theme)
      if (!is.null(text.gp$size)) {
        text.gp$size <- text.gp$size/.pt
      }

      yaxis <- list(ArcyAxisGrob(coord = coord,
                                 region = region,
                                 position = position,
                                 line.gp = line.gp,
                                 tick.gp = tick.gp,
                                 text.gp = text.gp,
                                 ticks.length = ticks.length))
    }

    grobs <- c(grobs, c(panel, grid_x, grid_y, xaxis, yaxis, layers))
    if (!is.null(guides)) {
      legends <- c(legends, list(guides))
    }
  }

  xlim <- xlim %||% attr(plot, "xlim")
  ylim <- ylim %||% attr(plot, "ylim")
  if (any(is.null(xlim), is.null(ylim))) {
    lims <- get_xy_lim(plot$region)
    xlim <- xlim %||% lims$xlim
    ylim <- ylim %||% lims$ylim
  }

  aspect_ratio <- diff(xlim)/diff(ylim)
  gt <- gtable::gtable(widths = unit(aspect_ratio, "null"),
                       heights = unit(1, "null"),
                       respect = TRUE)
  if (length(grobs) > 0) {
    grobs <- gTree(children = do.call("gList", grobs),
                   vp = viewport(x = unit(0.5, "npc"),
                                 y = unit(0.5, "npc"),
                                 xscale = xlim,
                                 yscale = ylim))
    gt <- gtable::gtable_add_grob(gt, grobs, t = 1, b = 1, l = 1, r = 1)
  }

  position <- theme$legend.position
  if (position != "none" && length(legends) > 0) {
    legends <- merge_guide(legends, theme = theme, ...)
    legend_width <- gtable::gtable_width(legends)
    legend_height <- gtable::gtable_height(legends)
    just <- grid::valid.just(theme$legend.justification)
    xjust <- just[1]
    yjust <- just[2]
    if (is.numeric(position)) {
      xpos <- theme$legend.position[1]
      ypos <- theme$legend.position[2]
      legends <- grid::editGrob(legends,
                                vp = viewport(x = xpos,
                                              y = ypos,
                                              just = c(xjust, yjust),
                                              height = legend_height,
                                              width = legend_width))
    } else {
      if (identical(position, "bottom")) {
        gt <- gtable::gtable_add_rows(gt, heights = legend_height)
        gt <- gtable::gtable_add_grob(gt, legends, clip = "off", t = -1, b = -1,
                                      l = 1, r = 1, name = "guide-box")
        gt <- gtable::gtable_add_rows(gt, heights = unit(0, "cm"), pos = 0)
        gt <- gtable::gtable_add_cols(gt, widths = unit(0, "cm"), pos = 0)
        gt <- gtable::gtable_add_cols(gt, widths = unit(0, "cm"), pos = -1)
      } else if (identical(position, "top")) {
        gt <- gtable::gtable_add_rows(gt, heights = legend_height, pos = 0)
        gt <- gtable::gtable_add_grob(gt, legends, clip = "off", t = 1, b = 1,
                                      l = 1, r = 1, name = "guide-box")
        gt <- gtable::gtable_add_rows(gt, heights = unit(0, "cm"), pos = -1)
        gt <- gtable::gtable_add_cols(gt, widths = unit(0, "cm"), pos = 0)
        gt <- gtable::gtable_add_cols(gt, widths = unit(0, "cm"), pos = -1)
      } else if (identical(position, "left")) {
        gt <- gtable::gtable_add_cols(gt, widths = legend_width, pos = 0)
        gt <- gtable::gtable_add_grob(gt, legends, clip = "off", t = 1, b = 1,
                                      l = 1, r = 1, name = "guide-box")
        gt <- gtable::gtable_add_rows(gt, heights = unit(0, "cm"), pos = 0)
        gt <- gtable::gtable_add_rows(gt, heights = unit(0, "cm"), pos = -1)
        gt <- gtable::gtable_add_cols(gt, widths = unit(0, "cm"), pos = -1)
      } else {
        gt <- gtable::gtable_add_cols(gt, widths = legend_width, pos = -1)
        gt <- gtable::gtable_add_grob(gt, legends, clip = "off", t = 1, b = 1,
                                      l = -1, r = -1, name = "guide-box")
        gt <- gtable::gtable_add_rows(gt, heights = unit(0, "cm"), pos = 0)
        gt <- gtable::gtable_add_rows(gt, heights = unit(0, "cm"), pos = -1)
        gt <- gtable::gtable_add_cols(gt, widths = unit(0, "cm"), pos = 0)
      }
    }
  }

  title <- title %||% attr(plot, "title")
  if (!is.null(title)) {
    title <- ggplot2::element_render(theme, "plot.title", title, margin_y = TRUE)
    title_height <- grobHeight(title)

    gt <- gtable::gtable_add_rows(gt, heights = title_height, pos = 0)
    gt <- gtable::gtable_add_grob(gt, title, clip = "off", t = 1, b = 1,
                                  l = 2, r = 3, name = "title")
  }

  plot.margin <- theme$plot.margin %||% margin(0.5, 0.5, 0.5, 0.5, "lines")
  gt <- gtable::gtable_add_padding(gt, plot.margin)
  gt
}

#' @noRd
get_xy_lim <- function(regions) {
  df <- lapply_dfr(regions, function(x) {
    polar2cartesian(data_frame0(x = c(seq(x$x.range[1], x$x.range[2], length.out = 100),
                                      seq(x$x.range[1], x$x.range[2], length.out = 100)),
                                y = rep(x$y.range, each = 100)))
  })

  list(xlim = range(df$x) + c(-0.01, 0.01) * diff(range(df$x)),
       ylim = range(df$y) + c(-0.01, 0.01) * diff(range(df$y)))
}

#' @noRd
#' @importFrom grid unit.c unit.pmax
merge_guide <- function(guides,
                        nrow = NULL,
                        ncol = NULL,
                        byrow = FALSE,
                        theme = theme_get()) {
  if (empty(guides)) {
    return(NULL)
  }

  guides <- Reduce("rbind", guides)
  ids <- names(guides$grobs) != "" & !duplicated(gsub("^\\d+_", "", names(guides$grobs)))
  guides <- guides$grobs[ids]
  guides <- guides[order(names(guides))]
  n <- length(guides)

  legend.position <- theme$legend.position %||% "right"
  legend.box.margin <- theme$legend.box.margin %||% ggplot2::margin()
  legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
  legend.spacing.y <- theme$legend.spacing.y %||% legend.spacing
  legend.spacing.x <- theme$legend.spacing.x %||% legend.spacing

  guides <- lapply(guides, gtable::gtable_add_padding, padding = legend.box.margin)

  if (identical(legend.position, "left") || identical(legend.position, "right")) {
    if (is.null(ncol)) ncol <- 1

    ncol <- min(n, ncol)
    nrow <- nrow %||% ceiling(n/ncol)
  } else if (identical(legend.position, "top") || identical(legend.position, "bottom")) {
    if (is.null(nrow)) nrow <- 1
    nrow <- min(n, nrow)
    ncol <- ceiling(n/nrow)
  } else {
    if (is.null(ncol)) ncol <- 1

    ncol <- min(n, ncol)
    nrow <- nrow %||% ceiling(n/ncol)
  }

  nrow <- min(nrow, n)
  ncol <- ceiling(n/nrow)

  if (isTRUE(byrow)) {
    colid <- rep_len(1:ncol, n)
    rowid <- rep_len(rep(1:nrow, each = ncol), n)
  } else {
    colid <- rep_len(rep(1:ncol, each = nrow), n)
    rowid <- rep_len(1:nrow, n)
  }

  widths <- do.call("unit.c", lapply(1:ncol, function(ii) {
    w <- do.call("unit.c", lapply(guides[colid == ii], gtable::gtable_width))
    unit.pmax(w)
  }))

  heights <- do.call("unit.c", lapply(1:nrow, function(jj) {
    h <- do.call("unit.c", lapply(guides[rowid == jj], gtable::gtable_height))
    unit.pmax(h)
  }))

  gt <- gtable::gtable(widths = widths, heights = heights, name = "guide-box")
  gt <- gtable::gtable_add_grob(gt, guides, l = colid, t = rowid)

  if (identical(legend.position, "left")) {
    gt <- gtable::gtable_add_cols(gt, widths = legend.spacing.x)
  } else if (identical(legend.position, "right")) {
    gt <- gtable::gtable_add_cols(gt, widths = legend.spacing.x, pos = 0)
  } else if (identical(legend.position, "top")) {
    gt <- gtable::gtable_add_rows(gt, heights = legend.spacing.y, pos = -1)
  } else if (identical(legend.position, "bottom")) {
    gt <- gtable::gtable_add_rows(gt, heights = legend.spacing.y, pos = -0)
  }

  gt
}
