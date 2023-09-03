
#' @export
layer_text <- function(..., show.legend = FALSE) {
  ggplot2::geom_text(..., show.legend = show.legend)
}

#' @export
layer_label <- function(..., show.legend = FALSE) {
  ggplot2::geom_label(..., show.legend = show.legend)
}

#' @importFrom ggplot2 layer ggproto
#' @export
layer_banner_text <- function(mapping = NULL,
                              data = NULL,
                              stat = "identity",
                              position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = FALSE,
                              inherit.aes = TRUE)
{
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomBannerText,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(na.rm = na.rm, ...))
}

#' @export
layer_margin_vtext <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = FALSE,
                               inherit.aes = TRUE)
{
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomMarginVtext,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(na.rm = na.rm, ...))
}

#' @export
layer_margin_htext <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = FALSE,
                               inherit.aes = TRUE)
{
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomMarginHtext,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(na.rm = na.rm, ...))
}

#' @export
GeomText2grob <- function(data,
                          trans = NULL,
                          coord = PANEL(),
                          region = CELL(),
                          ...,
                          parse = FALSE,
                          clip = FALSE,
                          na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }

  data <- trans(data)
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y, data$x,
                               data$angle)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x, data$y,
                               data$angle)
  }
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  exec(ArcTextGrob, !!!data, parse = parse, auto_adjust = TRUE, ...)
}

#' @export
GeomLabel2grob <- function(data,
                           trans = NULL,
                           coord = PANEL(),
                           region = CELL(),
                           ...,
                           parse = FALSE,
                           label.padding = unit(0.25, "lines"),
                           label.r = unit(0.15, "lines"),
                           label.size = 0.25,
                           clip = FALSE,
                           na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }

  data <- trans(data)
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y, data$x)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x, data$y)
  }

  ## can be removed?
  data$angle <- 0

  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  exec(ArcLabelGrob, !!!data, parse = parse, auto_adjust = TRUE, ...,
       label.padding = label.padding, label.r = label.r,
       label.size = label.size)
}

#' @export
GeomBannerText2grob <- function(data,
                                trans = NULL,
                                coord = PANEL(),
                                region = CELL(),
                                ...,
                                clip = FALSE,
                                na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }

  data <- trans(data)
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y, data$x)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x, data$y)
  }

  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  exec(ArcBannerTextGrob, !!!data, ...)
}

#' @export
GeomMarginVtext2grob <- function(data,
                                 trans = NULL,
                                 coord = PANEL(),
                                 region = CELL(),
                                 ...,
                                 flipped_aes = FALSE,
                                 clip = FALSE,
                                 na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }

  data <- trans(data)
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  if (isTRUE(flipped_aes)) {
    upper <- max(region$x.range)
    lower <- min(region$x.range)
    exec(ArcMarginHtextGrob, !!!data, ..., upper = upper, lower = lower)
  } else {
    upper <- max(region$y.range)
    lower <- min(region$y.range)
    exec(ArcMarginVtextGrob, !!!data, ..., upper = upper, lower = lower)
  }
}

#' @export
GeomMarginHtext2grob <- function(data,
                                 trans = NULL,
                                 coord = PANEL(),
                                 region = CELL(),
                                 ...,
                                 flipped_aes = FALSE,
                                 clip = FALSE,
                                 na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }

  data <- trans(data)
  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)
  if (isTRUE(flipped_aes)) {
    upper <- max(region$y.range)
    lower <- min(region$y.range)
    exec(ArcMarginVtextGrob, !!!data, ..., upper = upper, lower = lower)
  } else {
    upper <- max(region$x.range)
    lower <- min(region$x.range)
    exec(ArcMarginHtextGrob, !!!data, ..., upper = upper, lower = lower)
  }
}

#' @export
GeomBannerText <- ggproto(
  "GeomBannerText", GeomText,
  default_aes = aes(colour = "black", size = 3.88, hjust = 0.5,
                    vjust = 0.5, alpha = NA, family = "", fontface = 1,
                    lineheight = 1.2, outside = FALSE),
  draw_panel = function (data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                         check_overlap = FALSE) {
    if (empty(data)) {
      return(zeroGrob())
    }

    data$angle <- 0
    GeomText$draw_panel(data = data, panel_params = panel_params, coord = coord,
                        parse = parse, na.rm = na.rm, check_overlap = check_overlap)
  }
)

ArcLabelGrob <- function(label,
                         x = 90,
                         y = 0.5,
                         hjust = 0.5,
                         vjust = 0.5,
                         colour = "black",
                         fill = "white",
                         alpha = NA,
                         angle = 0,
                         size = 3.88,
                         family = "",
                         fontface = 1,
                         lineheight = 1.2,
                         parse = FALSE,
                         label.padding = unit(0.25, "lines"),
                         label.r = unit(0.15, "lines"),
                         label.size = 0.25,
                         auto_adjust = TRUE,
                         ...) {
  data <- data_frame0(label = label,
                      x = x,
                      y = y,
                      hjust = hjust,
                      vjust = vjust,
                      colour = colour,
                      fill = fill,
                      alpha = alpha,
                      angle = angle,
                      size = size,
                      family = family,
                      fontface = fontface,
                      lineheight = lineheight)

  if (empty(data)) {
    return(zeroGrob())
  }

  if (is.character(data$angle)) {
    data$angle <- text_angle(data$x, facing = data$angle)
  }
  if (isTRUE(auto_adjust)) {
    data$angle <- auto_adjust(data$x, data$angle)
  }
  data <- polar2cartesian(data)

  grobs <- lapply(split(data, 1:nrow(data)), function(row) {
    lab <- row$label
    if (isTRUE(parse)) {
      lab <- parse_safe(as.character(lab))
    }

    LabelGrob(label = lab,
              x = row$x,
              y = row$y,
              rot = row$angle,
              just = list(row$hjust, row$vjust),
              padding = label.padding,
              r = label.r,
              text.gp = gpar(col = row$colour,
                             fontsize = row$size * .pt,
                             fontfamily = row$family,
                             fontface = row$fontface,
                             lineheight = row$lineheight),
              rect.gp = gpar(col = if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
                             fill = alpha(row$fill, row$alpha),
                             lwd = label.size * .pt),
              default.units = "native")
  })

  do.call(grid::gList, grobs)
}

LabelGrob <- function(label,
                      x = unit(0.5, "npc"),
                      y = unit(0.5, "npc"),
                      rot = 0,
                      just = "center",
                      padding = unit(0.25, "lines"),
                      r = unit(0.1, "snpc"),
                      default.units = "npc",
                      name = NULL,
                      text.gp = gpar(),
                      rect.gp = gpar(fill = "white"),
                      vp = NULL) {
  if (length(label) != 1) {
    cli::cli_abort("{.arg label} must be of length 1")
  }
  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }

  gTree(label = label, x = x, y = y, rot = rot, just = just, padding = padding,
        r = r, name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp,
        default.units = default.units, cl = "LabelGrob")
}

#' @importFrom grid grobWidth grobHeight resolveHJust
#' @export
makeContent.LabelGrob <- function (x)
{
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)
  t <- grid::textGrob(x$label, gp = x$text.gp, name = "Label")
  vp <- grid::viewport(x = x$x,
                       y = x$y,
                       width = grobWidth(t) + 2 * x$padding,
                       height = grobHeight(t) + 2 * x$padding,
                       just = list(hj, vj),
                       angle = x$rot,
                       default.units = x$default.units)
  r <- grid::roundrectGrob(r = x$r, gp = x$rect.gp, vp = vp, name = "Box")
  t <- grid::editGrob(t, vp = vp)
  setChildren(x, gList(r, t))
}

#' @export
GeomMarginVtext <- ggproto(
  "GeomMarginVtext", GeomText,
  default_aes = aes(colour = "black", size = 3.88, alpha = NA,
                    family = "", fontface = 1, lineheight = 1.2),
  require_aes = c("x", "y", "label"),
  draw_panel = function (data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                         sides = "r", length = unit(1, "cm"), tick.length = "10%") {
    if (empty(data)) {
      return(zeroGrob())
    }

    if (!coord$is_linear()) {
      cli::cli_abort("{.fun geom_margin_vtext} just support for linear coordinate.")
    }

    sides <- match.arg(sides, c("r", "l"))
    data <- coord$transform(data, panel_params = panel_params)
    data <- data[setdiff(names(data), c("group", "PANEL"))]
    if (inherits(coord, "CoordFlip")) {
      sides <- switch (sides, "r" = "t", "l" = "b")
      exec(MarginHtextGrob, !!!data, parse = parse, sides = sides,
           length = length, tick.length = tick.length)
    } else {
      exec(MarginVtextGrob, !!!data, parse = parse, sides = sides,
           length = length, tick.length = tick.length)
    }
  }
)

#' @export
GeomMarginHtext <- ggproto(
  "GeomMarginHtext", GeomText,
  default_aes = aes(colour = "black", size = 3.88, alpha = NA,
                    family = "", fontface = 1, lineheight = 1.2),
  require_aes = c("x", "y", "label"),
  draw_panel = function (data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                         sides = "t", length = unit(1, "cm"), tick.length = "10%") {
    if (empty(data)) {
      return(zeroGrob())
    }

    if (!coord$is_linear()) {
      cli::cli_abort("{.fun geom_margin_htext} just support for linear coordinate.")
    }

    sides <- match.arg(sides, c("t", "b"))
    data <- coord$transform(data, panel_params = panel_params)
    data <- data[setdiff(names(data), c("group", "PANEL"))]
    if (inherits(coord, "CoordFlip")) {
      sides <- switch (sides, "t" = "r", "b" = "l")
      exec(MarginVtextGrob, !!!data, parse = parse, sides = sides,
           length = length, tick.length = tick.length)
    } else {
      exec(MarginHtextGrob, !!!data, parse = parse, sides = sides,
           length = length, tick.length = tick.length)
    }
  }
)
