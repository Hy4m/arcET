#' Convert Layer to Grob
#' @description Convert a ggplot layer to arc grob.
#' @param data data frame object, which is extract from a ggplot object.
#' @param trans coordinate transform function.
#' @param coord coordinate specification, as created by `PANEL()` or extract from
#' ggplot object.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the drawing area.
#' @param ... other parameters passing to `Arc*Grob()` function.
#' @inheritParams ggtext::geom_richtext
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomRichtext
#' @export
GeomRichText2grob <- function(data,
                              trans = NULL,
                              coord = PANEL(),
                              region = CELL(),
                              ...,
                              label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
                              label.margin = unit(c(0, 0, 0, 0), "lines"),
                              label.r = unit(0.15, "lines"),
                              clip = FALSE,
                              na.rm = FALSE) {
  data <- trans(data)
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y, data$x,
                               data$angle)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x, data$y,
                               data$angle)
  }

  box_gp <- gpar(col = alpha(data$label.colour %||% data$colour, data$alpha),
                 fill = alpha(data$fill, data$alpha),
                 lwd = data$label.size * .pt)
  if (!is.null(data$text.colour)) {
    data$colour <- data$text.colour
  }

  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  exec(ArcRichtextGrob, !!!data,
       padding = label.padding,
       margin = label.margin,
       r = label.r,
       box_gp = box_gp,
       auto_adjust = TRUE,
       ...)
}

#' @rdname decorate
#' @export
decorate_richtext <- function(plot,
                              label,
                              x = mid_x,
                              y = mid_y,
                              CellID = NULL,
                              auto_adjust = TRUE,
                              fixed = TRUE,
                              ignore.case = FALSE,
                              ...) {
  stopifnot(is_ArcPlot(plot))

  if (missing(label)) {
    cli::cli_warn(c("{.arg label} is missing",
                    i = "did you forget to set the label parameter?"))
    return(plot)
  }

  ids <- match_ids(CellID, plot$CellID, fixed = fixed, ignore.case = ignore.case)
  if (length(ids) >= 1) {
    x <- rlang::enquo(x)
    y <- rlang::enquo(y)

    for (ii in ids) {
      region <- plot$region[[ii]]
      anno <-  ArcRichtextGrob(label = label,
                               x = rlang::eval_tidy(x, region),
                               y = rlang::eval_tidy(y, region),
                               auto_adjust = auto_adjust,
                               ...)
      plot$annotate <- c(plot$annotate, list(anno))
    }
  }

  set_current_plot(plot)
  plot
}

#' Arc Richtext Grob
#' @description These functions can draw richtext on polar coordinate.
#' @param label character vector containing Markdown/HTML strings to draw.
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param hjust numeric vector specifying horizontal justification.
#' @param vjust numeric vector specifying vertical justification.
#' @param colour color of text.
#' @param angle angle of text.
#' @param alpha transparency of text.
#' @param size font size of text.
#' @param family font family of text.
#' @param fontface font face of text.
#' @param lineheight height of a line as a multiple of the size of text.
#' @inheritParams gridtext::richtext_grob
#' @param auto_adjust logical, if TRUE will adjust angle based on x-axis value.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcRichTextGrob
#' @author Hou Yun
#' @export
 ArcRichtextGrob <- function(label,
                             x = 90,
                             y = 0.5,
                             hjust = 0.5,
                             vjust = 0.5,
                             halign = hjust,
                             valign = vjust,
                             colour = "black",
                             alpha = NA,
                             angle = 0,
                             size = 3.88,
                             family = "",
                             fontface = 1,
                             lineheight = 1.2,
                             margin = unit(c(0, 0, 0, 0), "pt"),
                             padding = unit(c(0, 0, 0, 0), "pt"),
                             r = unit(0, "pt"),
                             align_widths = FALSE,
                             align_heights = FALSE,
                             box_gp = gpar(col = NA),
                             use_markdown = TRUE,
                             auto_adjust = TRUE,
                             ...) {
   rlang::check_installed("ggtext", reason = "for `RichText`.")
  data <- data_frame0(label = label,
                      x = x,
                      y = y,
                      hjust = hjust,
                      vjust = vjust,
                      colour = colour,
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
  gridtext::richtext_grob(text = data$label,
                          x = data$x,
                          y = data$y,
                          rot = data$angle,
                          hjust = data$hjust,
                          vjust = data$vjust,
                          halign = halign,
                          valign = valign,
                          margin = margin,
                          padding = padding,
                          r = r,
                          align_widths = align_widths,
                          align_heights = align_heights,
                          box_gp = box_gp,
                          use_markdown = use_markdown,
                          default.units = "native",
                          gp = gpar(col = alpha(data$colour, data$alpha),
                                    fontsize = data$size * .pt,
                                    fontfamily = data$family,
                                    fontface = data$fontface,
                                    lineheight = data$lineheight))
}
