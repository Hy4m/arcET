#' @export
print.ArcPlot <- function(x,
                          newpage = is.null(vp),
                          vp = NULL,
                          ...) {
  set_current_plot(x)

  if (newpage) {
    grid::grid.newpage()
  }
  grDevices::recordGraphics(requireNamespace("arcET", quietly = TRUE),
                            list(), getNamespace("arcET"))

  grobs <- ArcPlot_build(plot = x, ...)
  if (is.null(vp)) {
    grid::grid.draw(grobs)
  } else {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
    }
    grid::grid.draw(grobs)

    grid::upViewport()
  }

  invisible(x)
}

#' @export
plot.ArcPlot <- function(x,
                         newpage = is.null(vp),
                         vp = NULL,
                         ...) {
  if (newpage) {
    grid::grid.newpage()
  }
  grDevices::recordGraphics(requireNamespace("arcET", quietly = TRUE),
                            list(), getNamespace("arcET"))

  grobs <- ArcPlot_build(plot = x, ...)
  if (is.null(vp)) {
    grid::grid.draw(grobs)
  } else {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
    }
    grid::grid.draw(grobs)

    grid::upViewport()
  }

  invisible(x)
}

#' @title Build ArcPlot
#' @description Internal function used to convert ArcPlot to grobs.
#' @param plot an ArcPlot object.
#' @param ... other parameters passing to `merge_guides()`.
#' @return grid grobs.
#' @author Hou Yun
#' @rdname ArcPlot_build
#' @export
ArcPlot_build <- function(plot, ...) {
  UseMethod("ArcPlot_build")
}

#' @importFrom ggplot2 theme_get
#' @rdname ArcPlot_build
#' @export
ArcPlot_build.ArcPlot <- function(plot, ...) {
  theme <- plot_theme(plot$theme, default = theme_get())

  if (length(plot$plot) < 1) {
    xlim <- attr(plot, "xlim") %||% c(-1, 1)
    ylim <- attr(plot, "ylim") %||% c(-1, 1)
  } else {
    xlim <- attr(plot, "xlim")
    ylim <- attr(plot, "ylim")
    if (any(is.null(xlim), is.null(ylim))) {
      lims <- get_xy_lim(plot$region)
      xlim <- xlim %||% lims$xlim
      ylim <- ylim %||% lims$ylim
    }
  }

  aspect_ratio <- diff(xlim)/diff(ylim)
  vp <- viewport(x = unit(0.5, "npc"),
                 y = unit(0.5, "npc"),
                 xscale = xlim,
                 yscale = ylim,
                 clip = "off")
  widths <- unit(c(0, 0, aspect_ratio, 0, 0),
                 c("cm", "cm", "null", "cm", "cm"))
  heights <- unit(c(0, 0, 0, 0, 1, 0, 0, 0),
                  c("cm", "cm", "cm", "cm", "null", "cm", "cm", "cm"))
  gt <- gtable(widths = widths, heights = heights, respect = TRUE)

  n <- length(plot$plot)
  guides <- list()
  for (ii in seq_len(n)) {
    gg_element <- tryCatch(extract_ggplot(plot$plot[[ii]]), error = function(e) NULL)
    ## gg_element is NULL means failure, should throw warnings?
    if (is.null(gg_element)) next

    cell <- CellPlot_build(gg_element,
                           region = plot$region[[ii]],
                           CellID = plot$CellID[ii],
                           process = paste(ii, n, sep = "/"))
    if (!is.null(cell$guides)) {
      guides <- c(guides, list(cell$guides))
    }

    cell <- c(cell[1:5], cell$layers)
    class(cell) <- "gList"
    cell <- grid::gTree(children = cell, vp = vp)
    gt <- gtable_add_grob(gt, grobs = cell, t = 5, l = 3, b = 5, r = 3,
                          clip = "off", name = paste(plot$CellID[ii], "panel", sep = "."))
  }

  position <- theme$legend.position
  if (!identical(position, "none") && length(guides) > 0) {
    guides <- merge_guide(guides, theme = theme, ...)
    guide_width <- gtable_width(guides)
    guide_height <- gtable_height(guides)
    just <- grid::valid.just(theme$legend.justification)
    xjust <- just[1]
    yjust <- just[2]

    if (is.numeric(position)) {
      xpos <- position[1]
      ypos <- position[2]
      guides <- grid::editGrob(guides,
                               vp = viewport(x = xpos,
                                             y = ypos,
                                             just = c(xjust, yjust),
                                             height = guide_height,
                                             width = guide_width))
      gt <- gtable_add_grob(gt, guides, t = 1, l = 1, b = 8, r = 5,
                            clip = "off", name = "guide-box")
    } else {
      if (identical(position, "bottom")) {
        gt$heights[6] <- guide_height
        gt <- gtable_add_grob(gt, guides, t = 6, l = 1, b = 6, r = 5,
                              clip = "off", name = "guide-box")
      } else if (identical(position, "top")) {
        gt$heights[4] <- guide_height
        gt <- gtable_add_grob(gt, guides, t = 4, l = 1, b = 4, r = 5,
                              clip = "off", name = "guide-box")
      } else if (identical(position, "left")) {
        gt$widths[2] <- guide_width
        gt <- gtable_add_grob(gt, guides, t = 1, l = 2, b = 8, r = 2,
                              clip = "off", name = "guide-box")
      } else {
        gt$widths[4] <- guide_width
        gt <- gtable_add_grob(gt, guides, t = 1, l = 4, b = 8, r = 4,
                              clip = "off", name = "guide-box")
      }
    }
  }

  title <- plot$labels$title
  plot.title.position <- theme$plot.title.position

  if (!is.null(title)) {
    title <- element_render(theme, "plot.title", title, margin_y = TRUE)
    gt$heights[2] <- grobHeight(title)

    if (identical(plot.title.position, "panel")) {
      gt <- gtable_add_grob(gt, title, t = 2, l = 3, clip = "off",
                            name = "title")
    } else {
      gt <- gtable_add_grob(gt, title, t = 2, l = 1, b = 2, r = 5,
                            clip = "off", name = "title")
    }
  }

  subtitle <- plot$labels$subtitle
  plot.subtitle.position <- theme$plot.subtitle.position %||% theme$plot.title.position

  if (!is.null(subtitle)) {
    subtitle <- element_render(theme, "plot.subtitle", subtitle, margin_y = TRUE)
    gt$heights[3] <- grobHeight(subtitle)

    if (identical(plot.subtitle.position, "panel")) {
      gt <- gtable_add_grob(gt, subtitle, t = 3, l = 3, clip = "off",
                            name = "subtitle")
    } else {
      gt <- gtable_add_grob(gt, subtitle, t = 3, l = 1, b = 3, r = 5,
                            clip = "off", name = "subtitle")
    }
  }

  caption <- plot$labels$caption
  plot.caption.position <- theme$plot.caption.position %||% theme$plot.title.position

  if (!is.null(caption)) {
    caption <- element_render(theme, "plot.caption", caption, margin_y = TRUE)
    gt$heights[7] <- grobHeight(caption)

    if (identical(plot.caption.position, "panel")) {
      gt <- gtable_add_grob(gt, caption, t = 7, l = 3, clip = "off",
                            name = "caption")
    } else {
      gt <- gtable_add_grob(gt, caption, t = 7, l = 1, b = 7, r = 5,
                            clip = "off", name = "caption")
    }
  }

  tag <- plot$labels$tag
  plot.tag.position <- theme$plot.tag.position

  if (!is.null(tag)) {
    tag <- element_render(theme, "plot.tag", tag, margin_y = TRUE,
                                   margin_x = TRUE)

    if (is.numeric(plot.tag.position)) {
      xpos <- plot.tag.position[1]
      ypos <- plot.tag.position[2]
      tag <- justify_grobs(tag, x = xpos, y = ypos,
                           hjust = theme$plot.tag$hjust, vjust = theme$plot.tag$vjust,
                           int_angle = theme$plot.tag$angle, debug = theme$plot.tag$debug)

      gt <- gtable_add_grob(gt, tag, t = 1, l = 1, b = 8, r = 5,
                            clip = "off", name = "tag")
    } else {
      tag_height <- grobHeight(tag)
      tag_width <- grobWidth(tag)

      if (plot.tag.position == "topleft") {
        gt$heights[1] <- tag_height
        gt$widths[1] <- tag_width
        gt <- gtable_add_grob(gt, tag, t = 1, l = 1, clip = "off",
                              name = "tag")
      } else if (plot.tag.position == "top") {
        gt$heights[1] <- tag_height
        gt <- gtable_add_grob(gt, tag, t = 1, l = 3, clip = "off",
                              name = "tag")
      } else if (plot.tag.position == "topright") {
        gt$heights[1] <- tag_height
        gt$widths[5] <- tag_width
        gt <- gtable_add_grob(gt, tag, t = 1, l = 5, clip = "off",
                              name = "tag")
      } else if (plot.tag.position == "left") {
        gt$widths[1] <- tag_width
        gt <- gtable_add_grob(gt, tag, t = 5, l = 1, clip = "off",
                              name = "tag")
      } else if (plot.tag.position == "right") {
        gt$widths[5] <- tag_width
        gt <- gtable_add_grob(gt, tag, t = 5, l = 5, clip = "off",
                              name = "tag")
      } else if (plot.tag.position == "bottomleft") {
        gt$heights[7] <- tag_height
        gt$widths[1] <- tag_width
        gt <- gtable_add_grob(gt, tag, t = 7, l = 1, clip = "off",
                              name = "tag")
      } else if (plot.tag.position == "bottom") {
        gt$heights[7] <- tag_height
        gt <- gtable_add_grob(gt, tag, t = 7, l = 3, clip = "off",
                              name = "tag")
      } else {
        gt$heights[7] <- tag_height
        gt$widths[5] <- tag_width
        gt <- gtable_add_grob(gt, tag, t = 7, l = 5, clip = "off",
                              name = "tag")
      }
    }
  }

  if (length(plot$annotate$anno) > 0) {
    anno_bottom <- plot$annotate$anno[plot$annotate$pos < 0]
    anno_top <- plot$annotate$anno[plot$annotate$pos >= 0]

    if (length(anno_bottom) > 0) {
      anno_bottom <- gTree(children = do.call("gList", anno_bottom), vp = vp)
      gt <- gtable_add_grob(gt, grobs = anno_bottom, t = 5, l = 3, b = 5, r = 3,
                            z = -Inf, clip = "off", name = "annotate-bottom")
    }

    if (length(anno_top) > 0) {
      anno_top <- gTree(children = do.call("gList", anno_top), vp = vp)
      gt <- gtable_add_grob(gt, grobs = anno_top, t = 5, l = 3, b = 5, r = 3,
                            z = Inf, clip = "off", name = "annotate-top")
    }
  }

  plot.margin <- theme$plot.margin %||% margin(0.5, 0.5, 0.5, 0.5, "lines")
  gt <- gtable_add_padding(gt, plot.margin)
  gt
}

#' @noRd
CellPlot_build <- function(gg_element,
                           region = CELL(),
                           CellID = cell_id(),
                           process = NULL,
                           ...) {
  cli::cli_inform("Build {CellID} plot......[{process}]")

  thm <- gg_element$theme
  coord <- gg_element$coord
  guides <- gg_element$guides
  layers <- lapply(seq_along(gg_element$data), function(ii) {
    FUN <- tryCatch(match.fun(paste0(gg_element$layer_name[ii], "2grob")),
                    error = function(e) "not_implement")

    if (identical(FUN, "not_implement")) {
      cli::cli_warn(c("{.fun {paste0(gg_element$layer_name[ii], '2grob')}} has not been implemented yet,",
                      i = "so this layer has been omitted."))
      zeroGrob()
    } else {
      rlang::inject(FUN(data = gg_element$data[[ii]], region = region, coord = coord,
                        trans = gg_element$trans, !!!gg_element$layer_params[[ii]]))
    }
  })

  ## build panel background
  panel.background <- calc_element("panel.background", thm)
  panel <- arc_element_grob(panel.background,
                            xmin  = region$x.range[1],
                            xmax = region$x.range[2],
                            ymin  = region$y.range[1],
                            ymax = region$y.range[2])

  ## build panel border
  panel.border <- calc_element("panel.border", thm)
  border <- arc_element_grob(panel.border,
                             xmin  = region$x.range[1],
                             xmax = region$x.range[2],
                             ymin  = region$y.range[1],
                             ymax = region$y.range[2])

  ## build panel grid
  x.major <- coord$x$breaks
  x.minor <- coord$x$minor_breaks
  y.major <- coord$y$breaks
  y.minor <- coord$y$minor_breaks

  if (length(x.major) > 0) {
    x.major <- scales::rescale(x.major, to = region$x.range, from = coord$x$range)
  }
  if (length(x.minor) > 0) {
    x.minor <- scales::rescale(x.minor, to = region$x.range, from = coord$x$range)
  }
  if (length(y.major) > 0) {
    y.major <- scales::rescale(y.major, to = region$y.range, from = coord$y$range)
  }
  if (length(y.minor) > 0) {
    y.minor <- scales::rescale(y.minor, to = region$y.range, from = coord$y$range)
  }

  guide_grid <- arc_guide_grid(theme = thm,
                               x.minor = x.minor,
                               x.major = x.major,
                               y.minor = y.minor,
                               y.major = y.major,
                               region = region)

  ## build x-axis guide
  if (coord$x$position == "none") {
    xaxis <- NULL
  } else {
    position <- coord$x$position
    title.gp <- calc_element(paste0("axis.title.x.", position), thm)
    line.gp <- calc_element(paste0("axis.line.x.", position), thm)
    tick.gp <- calc_element(paste0("axis.ticks.x.", position), thm)
    ticks.length <- calc_element(paste0("axis.ticks.length.x.", position), thm)
    text.gp <- calc_element(paste0("axis.text.x.", position), thm)

    xaxis <- ArcxAxisGrob(title = gg_element$labels$x,
                          coord = coord,
                          region = region,
                          position = position,
                          title.gp = title.gp,
                          line.gp = line.gp,
                          tick.gp = tick.gp,
                          text.gp = text.gp,
                          ticks.length = ticks.length)
  }

  ## build x-axis guide
  if (coord$y$position == "none") {
    yaxis <- NULL
  } else {
    position <- coord$y$position
    title.gp <- calc_element(paste0("axis.title.y.", position), thm)
    line.gp <- calc_element(paste0("axis.line.y.", position), thm)
    tick.gp <- calc_element(paste0("axis.ticks.y.", position), thm)
    ticks.length <- calc_element(paste0("axis.ticks.length.y.", position), thm)
    text.gp <- calc_element(paste0("axis.text.y.", position), thm)

    yaxis <- ArcyAxisGrob(title = gg_element$labels$y,
                          coord = coord,
                          region = region,
                          position = position,
                          title.gp = title.gp,
                          line.gp = line.gp,
                          tick.gp = tick.gp,
                          text.gp = text.gp,
                          ticks.length = ticks.length)
  }

  list(panel = panel,
       border = border,
       guide_grid = guide_grid,
       xaxis = xaxis,
       yaxis = yaxis,
       layers = layers,
       guides = guides)
}

#' @noRd
get_xy_lim <- function(regions) {
  df <- lapply_dfr(regions, function(x) {
    polar2cartesian(data_frame0(x = c(seq(x$x.range[1], x$x.range[2], length.out = 200),
                                      seq(x$x.range[1], x$x.range[2], length.out = 200)),
                                y = rep(x$y.range, each = 200)))
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

  guides <- lapply(guides, gtable_add_padding, padding = legend.box.margin)

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
    w <- do.call("unit.c", lapply(guides[colid == ii], gtable_width))
    unit.pmax(w)
  }))

  heights <- do.call("unit.c", lapply(1:nrow, function(jj) {
    h <- do.call("unit.c", lapply(guides[rowid == jj], gtable_height))
    unit.pmax(h)
  }))

  gt <- gtable(widths = widths, heights = heights, name = "guide-box")
  gt <- gtable_add_grob(gt, guides, l = colid, t = rowid)

  if (identical(legend.position, "left")) {
    gt <- gtable_add_cols(gt, widths = legend.spacing.x)
  } else if (identical(legend.position, "right")) {
    gt <- gtable_add_cols(gt, widths = legend.spacing.x, pos = 0)
  } else if (identical(legend.position, "top")) {
    gt <- gtable_add_rows(gt, heights = legend.spacing.y, pos = -1)
  } else if (identical(legend.position, "bottom")) {
    gt <- gtable_add_rows(gt, heights = legend.spacing.y, pos = -0)
  }

  gt
}

