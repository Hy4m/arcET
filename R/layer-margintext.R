#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @param flipped_aes TRUE means that coordinates are inherit `CoordFlip`.
#' @inheritParams ggplot2::geom_text
#' @inheritParams ggplot2::geom_label
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomMarginText
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

#' @rdname GeomMarginText
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

#' @title Margin Text
#' @description The margin text geom is used to create margin text. In contrast
#' to `geom_text()`, `geom_margin_*text()` automatically adjusts its position
#' based on the graph size to avoid overlap.
#' @inheritParams ggplot2::geom_text
#' @return a ggplot layer object.
#' @rdname geom_margintext
#' @author Hou Yun
#' @export
geom_margin_vtext <- function(mapping = NULL,
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

#' @rdname geom_margintext
#' @export
geom_margin_htext <- function(mapping = NULL,
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

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMarginVtext <- ggproto(
  "GeomMarginVtext", GeomText,
  default_aes = aes(colour = "black", size = 3.88, alpha = NA,
                    family = "", fontface = 1, lineheight = 1.2),
  require_aes = c("x", "y", "label"),
  draw_panel = function (data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                         margin = unit(1, "pt"), sides = "r", length = unit(1, "cm"),
                         tick.length = "10%", link.colour = NULL, link.linetype = NULL,
                         link.linewidth = NULL, link.alpha = NULL) {
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
      exec(MarginHtextGrob, !!!data,
           parse = parse,
           sides = sides,
           margin = margin,
           length = length,
           tick.length = tick.length,
           line.gp = gpar(col = alpha(link.colour %||% data$colour %||% "black",
                                      link.alpha %||% data$alpha %||% NA),
                          lty = link.linetype %||% data$linetype %||% 1,
                          lwd = (link.linewidth %||% data$linewidth %||% 0.5) * .pt))
    } else {
      exec(MarginVtextGrob, !!!data,
           parse = parse,
           sides = sides,
           margin = margin,
           length = length,
           tick.length = tick.length,
           line.gp = gpar(col = alpha(link.colour %||% data$colour %||% "black",
                                      link.alpha %||% data$alpha %||% NA),
                          lty = link.linetype %||% data$linetype %||% 1,
                          lwd = (link.linewidth %||% data$linewidth %||% 0.5) * .pt))
    }
  }
)

#' @rdname arcET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMarginHtext <- ggproto(
  "GeomMarginHtext", GeomText,
  default_aes = aes(colour = "black", size = 3.88, alpha = NA, family = "",
                    fontface = 1, lineheight = 1.2, linewidth = 0.5,
                    linetype = 1),
  require_aes = c("x", "y", "label"),
  draw_panel = function (data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                         sides = "t", margin = unit(1, "pt"), length = unit(1, "cm"),
                         tick.length = "10%", link.colour = NULL, link.linetype = NULL,
                         link.linewidth = NULL, link.alpha = NULL) {
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
      exec(MarginVtextGrob, !!!data,
           parse = parse,
           sides = sides,
           margin = margin,
           length = length,
           tick.length = tick.length,
           line.gp = gpar(col = alpha(link.colour %||% data$colour %||% "black",
                                      link.alpha %||% data$alpha %||% NA),
                          lty = link.linetype %||% data$linetype %||% 1,
                          lwd = (link.linewidth %||% data$linewidth %||% 0.5) * .pt))
    } else {
      exec(MarginHtextGrob, !!!data,
           parse = parse,
           sides = sides,
           margin = margin,
           length = length,
           tick.length = tick.length,
           line.gp = gpar(col = alpha(link.colour %||% data$colour %||% "black",
                                      link.alpha %||% data$alpha %||% NA),
                          lty = link.linetype %||% data$linetype %||% 1,
                          lwd = (link.linewidth %||% data$linewidth %||% 0.5) * .pt))
    }
  }
)
