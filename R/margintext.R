## FIX ME: link aesthetic mapping is broken

#' @title Arc MarginText Grob
#'
#' @description These functions can draw margin text on polar coordinate.
#' @param label expression or character.
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of text.
#' @param alpha transparency of text.
#' @param size font size of text.
#' @param family font family of text.
#' @param fontface font face of text.
#' @param lineheight height of a line as a multiple of the size of text.
#' @param linewidth,linetype,line_colour,line_alpha attributes of line.
#' @param align logical indicating wheather align label.
#' @param parse logical, if TRUE will parse text to expression.
#' @param sides position of margin text. `sides` must be one of "l" or "r" for
#' `ArcMarginVtextGrob()`, and must be one of "t" or "b" for `ArcMarginHtextGrob()`.
#' @param margin a grid unit object specifying top and bottom text margin.
#' @param length length of link, should be a grid unit object.
#' @param tick.length tick length of link, should be a grid unit object or character
#' ratio.
#' @param upper,lower the upper and lower limits of allowable adjustments.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcMarginTextGrob
#' @author Hou Yun
#' @export
ArcMarginVtextGrob <- function(label,
                               x = NULL,
                               y = 0.5,
                               colour = "black",
                               alpha = NA,
                               size = 3.88,
                               family = "",
                               fontface = 1,
                               lineheight = 1.2,
                               linewidth = 0.5,
                               linetype = 1,
                               line_colour = NULL,
                               line_alpha = NULL,
                               align = FALSE, ## until have a few bugs
                               parse = FALSE,
                               sides = "r",
                               margin = unit(1, "pt"),
                               length = unit(1, "cm"),
                               tick.length = unit(1.5, "mm"),
                               upper = NULL,
                               lower = NULL,
                               ...) {
  sides <- match.arg(sides, c("l", "r"))
  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight,
                      linewidth = linewidth, linetype = linetype,
                      line_colour = line_colour %||% colour,
                      line_alpha = line_alpha %||% alpha)

  if (empty(data) || !"y" %in% names(data)) {
    return(zeroGrob())
  }

  if (!"x" %in% names(data)) {
    data$x <- 90
  }

  if (!is.unit(margin)) {
    margin <- unit(margin, "cm")
  }
  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.unit(tick.length)) {
    tick.length <- unit(tick.length, "mm")
  }

  grid::gTree(data = data,
              align = align,
              parse = parse,
              sides = sides,
              margin = margin,
              length = length,
              tick.length = tick.length,
              upper = upper,
              lower = lower,
              cl = "ArcMarginVtextGrob")
}

#' @rdname ArcMarginTextGrob
#' @export
ArcMarginHtextGrob <- function(label,
                               x = 90,
                               y = NULL,
                               colour = "black",
                               alpha = NA,
                               size = 3.88,
                               family = "",
                               fontface = 1,
                               lineheight = 1.2,
                               linewidth = 0.5,
                               linetype = 1,
                               line_colour = NULL,
                               line_alpha = NULL,
                               align = TRUE,
                               parse = FALSE,
                               sides = "t",
                               margin = unit(1, "pt"),
                               length = unit(1, "cm"),
                               tick.length = unit(1.5, "mm"),
                               upper = NULL,
                               lower = NULL,
                               ...) {
  sides <- match.arg(sides, c("t", "b"))

  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight,
                      linewidth = linewidth, linetype = linetype,
                      line_colour = line_colour %||% colour,
                      line_alpha = line_alpha %||% alpha)

  if (empty(data) || !"x" %in% names(data)) {
    return(zeroGrob())
  }

  if (!"y" %in% names(data)) {
    data$y <- 1
  }

  if (!is.unit(margin)) {
    margin <- unit(margin, "cm")
  }
  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.unit(tick.length)) {
    tick.length <- unit(tick.length, "mm")
  }

  grid::gTree(data = data,
              align = align,
              parse = parse,
              sides = sides,
              margin = margin,
              length = length,
              tick.length = tick.length,
              upper = upper,
              lower = lower,
              cl = "ArcMarginHtextGrob")
}

#' @export
makeContent.ArcMarginHtextGrob <- function(x) {
  data <- transform(x$data, x = x %% 360)
  data <- data[order(data$x, decreasing = TRUE), , drop = FALSE]
  margin <- convert_height(x$margin, "native", valueOnly = TRUE)
  length <- convert_height(x$length, "native", valueOnly = TRUE)
  tick.length <- convert_height(x$tick.length, "native", valueOnly = TRUE)
  tick.length <- ifelse(tick.length > length/2, 0, tick.length)

  one_mm <- convert_height(unit(1, "mm"), "native", valueOnly = TRUE)
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  height <- NULL
  width <- NULL
  for (ii in 1:n) {
    row <- data[ii, , drop = FALSE]
    g <- grid::textGrob(label = lab[ii],
                        gp = gpar(col = row$colour,
                                  fontsize = row$size * .pt,
                                  fontfamily = row$family,
                                  fontface = row$fontface,
                                  lineheight = row$lineheight))
    height <- c(height, convert_height(grid::grobHeight(g), "native", valueOnly = TRUE))
    width <- c(width, convert_width(grid::grobWidth(g), "native", valueOnly = TRUE))
  }

  extra <- setdiff(names(data), c("x", "y", "xend", "yend", "label", "size",
                                  "family", "fontface", "lineheight", "group"))
  if (x$sides == "t") {
    height <- degree(atan((height + 2*margin)/(max(data$y, na.rm = TRUE) + length + one_mm)/2) * 2)
    data$xend <- adjust_margin(data$x, height, upper = x$upper, lower = x$lower,
                               arc = TRUE)
    data$angle <- text_angle(data$xend, "clockwise")

    if (isTRUE(x$align)) {
      data$yend <- max(data$y, na.rm = TRUE) + length
    } else {
      data$yend <- data$y + length
    }

    text <- transform(data, x = xend, y = yend + one_mm)
    line <- data_frame0(y = c(data$y, data$yend - length + tick.length, data$yend - tick.length, data$yend),
                        x = c(data$x, data$x, data$xend, data$xend),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  } else {
    height <- degree(atan((height + 2*margin)/(min(data$y, na.rm = TRUE) - max(width) - length - one_mm)/2) * 2)
    data$xend <- adjust_margin(data$x, height, upper = x$upper, lower = x$lower,
                               arc = TRUE)
    data$angle <- text_angle(data$xend, "reverse.clockwise")

    if (isTRUE(x$align)) {
      data$yend <- min(data$y, na.rm = TRUE) - length
    } else {
      data$yend <- data$y - length
    }

    text <- transform(data, x = xend, y = yend - one_mm)
    line <- data_frame0(y = c(data$y, data$y - tick.length, data$yend + tick.length, data$yend),
                        x = c(data$x, data$x, data$xend, data$xend),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  }

  text <- polar2cartesian(text)
  line <- polar2cartesian(line)
  line <- grid::polylineGrob(x = line$x,
                             y = line$y,
                             id = line$group,
                             gp = gpar(col = alpha(data$line_colour, data$line_alpha),
                                       lwd = data$linewidth * .pt,
                                       lty = data$linetype,
                                       lineend = "butt",
                                       linejoin = "round",
                                       linemitre = 10),
                             default.units = "native")
  text <- grid::textGrob(label = lab,
                         x = text$x,
                         y = text$y,
                         hjust = 0,
                         vjust = 0.5,
                         rot = text$angle,
                         default.units = "native",
                         gp = gpar(col = alpha(text$colour, text$alpha),
                                   fontsize = text$size * .pt,
                                   fontfamily = text$family,
                                   fontface = text$fontface,
                                   lineheight = text$lineheight))

  grid::setChildren(x, grid::gList(line, text))
}

#' @export
makeContent.ArcMarginVtextGrob <- function(x) {
  data <- x$data
  data <- data[order(data$y, decreasing = TRUE), , drop = FALSE]
  margin <- convert_width(x$margin, "native", valueOnly = TRUE)
  length <- convert_width(x$length, "native", valueOnly = TRUE)
  tick.length <- convert_width(x$tick.length, "native", valueOnly = TRUE)
  tick.length <- ifelse(tick.length > length/2, 0, tick.length)

  one_mm <- convert_width(unit(1, "mm"), "native", valueOnly = TRUE)
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  if (x$sides == "r") {
    angle <- (min(data$x, na.rm = TRUE) %% 360) - 90
  } else {
    angle <- (max(data$x, na.rm = TRUE) %% 360) - 90
  }
  vp <- grid::viewport(x = 0,
                       y = 0,
                       width = unit(2, "native"),
                       height = unit(1, "native"),
                       just = c(0.5, 0),
                       angle = angle,
                       xscale = c(-1, 1),
                       yscale = c(0, 1),
                       default.units = "native",
                       clip = "off")

  height <- vapply_dbl(1:n, function(ii) {
    row <- data[ii, , drop = FALSE]
    g <- grid::textGrob(label = lab[ii],
                        gp = gpar(col = row$colour,
                                  fontsize = row$size * .pt,
                                  fontfamily = row$family,
                                  fontface = row$fontface,
                                  lineheight = row$lineheight))
    convert_height(grid::grobHeight(g), "native", valueOnly = TRUE, vp = vp)
  })
  data$yend <- adjust_margin(data$y, height + 2*margin, upper = x$upper, lower = x$lower,
                             arc = FALSE)

  extra <- setdiff(names(data), c("x", "y", "xend", "yend", "label", "size",
                                  "family", "fontface", "lineheight", "group"))
  if (x$sides == "r") {
    text <- transform(data, x = one_mm + length, y = yend, vjust = 0.5, hjust = 0)
    line <- data_frame0(y = c(data$y, data$y, data$yend, data$yend),
                        x = rep(c(0, tick.length, length - tick.length, length), each = n),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  } else {
    text <- transform(data, x = -one_mm - length, y = yend, vjust = 0.5, hjust = 1)
    line <- data_frame0(y = c(data$y, data$y, data$yend, data$yend),
                        x = rep(c(0, -tick.length, -length + tick.length, -length), each = n),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  }

  line <- grid::polylineGrob(x = line$x,
                             y = line$y,
                             id = line$group,
                             default.units = "native",
                             gp = gpar(col = alpha(data$line_colour, data$line_alpha),
                                       lwd = data$linewidth * .pt,
                                       lty = data$linetype,
                                       lineend = "butt",
                                       linejoin = "round",
                                       linemitre = 10),
                             vp = vp)
  text <- grid::textGrob(label = lab,
                         x = text$x,
                         y = text$y,
                         hjust = text$hjust,
                         vjust = text$vjust,
                         rot = 0,
                         default.units = "native",
                         gp = gpar(col = alpha(text$colour, text$alpha),
                                   fontsize = text$size * .pt,
                                   fontfamily = text$family,
                                   fontface = text$fontface,
                                   lineheight = text$lineheight),
                         vp = vp)

  has_expand <- FALSE
  if (isTRUE(x$align) && length(unique(data$x)) != 1) {
    has_expand <- TRUE
    if (x$sides == "r") {
      xend <- min(data$x, na.rm = TRUE)
      expand <- data[data$x > min(data$x, na.rm = TRUE), , drop = FALSE]

    } else {
      xend <- max(data$x, na.rm = TRUE)
      expand <- data[data$x < max(data$x, na.rm = TRUE), , drop = FALSE]
    }

    expand <- ArcSegmentsGrob(x = expand$x,
                              xend = xend,
                              y = expand$y,
                              yend = expand$y,
                              colour = expand$line_colour,
                              linewidth = expand$linewidth,
                              linetype = expand$linetype,
                              alpha = expand$line_alpha)
  }

  grid::setChildren(x, grid::gList(if (has_expand) expand, line, text))
}

#' @title MarginText Grob
#'
#' @description These functions can draw margin text on cartesian coordinate.
#'
#' @param label expression or character.
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param colour color of text.
#' @param alpha transparency of text.
#' @param size font size of text.
#' @param family font family of text.
#' @param fontface font face of text.
#' @param lineheight height of a line as a multiple of the size of text.
#' @param linewidth,linetype,line_colour,line_alpha attributes of line.
#' @param align logical indicating wheather align label.
#' @param parse logical, if TRUE will parse text to expression.
#' @param sides position of margin text. `sides` must be one of "l" or "r" for
#' `MarginVtextGrob()`, and must be one of "t" or "b" for `MarginHtextGrob()`.
#' @param margin a grid unit object specifying top and bottom text margin.
#' @param length length of link, should be a grid unit object.
#' @param tick.length tick length of link, should be a grid unit object or character
#' ratio.
#' @param ... not used.
#' @return a grob object.
#' @rdname MarginTextGrob
#' @author Hou Yun
#' @export
MarginVtextGrob <- function(label,
                            x = NULL,
                            y = 0.5,
                            colour = "black",
                            alpha = NA,
                            size = 3.88,
                            family = "",
                            fontface = 1,
                            lineheight = 1.2,
                            linewidth = 0.5,
                            linetype = 1,
                            line_colour = NULL,
                            line_alpha = NULL,
                            align = TRUE,
                            parse = FALSE,
                            sides = "r",
                            margin = unit(1, "pt"),
                            length = unit(1, "cm"),
                            tick.length = unit(1.5, "mm"),
                            ...) {
  sides <- match.arg(sides, c("r", "l"))
  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight,
                      linewidth = linewidth, linetype = linetype,
                      line_colour = line_colour %||% colour,
                      line_alpha = line_alpha %||% alpha)

  if (empty(data) || !"y" %in% names(data)) {
    return(zeroGrob())
  }

  if (!"x" %in% names(data)) {
    data$x <- if (sides == "r") 1 else 0
  }

  if (!is.unit(margin)) {
    margin <- unit(margin, "pt")
  }
  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.unit(tick.length)) {
    tick.length <- unit(tick.length, "mm")
  }

  grid::gTree(data = data, align = align, parse = parse, sides = sides,
              margin = margin, length = length, tick.length = tick.length,
              cl = "MarginVtextGrob")
}

#' @rdname MarginTextGrob
#' @export
MarginHtextGrob <- function(label,
                            x = 0.5,
                            y = NULL,
                            colour = "black",
                            alpha = NA,
                            size = 3.88,
                            family = "",
                            fontface = 1,
                            lineheight = 1.2,
                            linewidth = 0.5,
                            linetype = 1,
                            line_colour = NULL,
                            line_alpha = NULL,
                            align = TRUE,
                            parse = FALSE,
                            sides = "t",
                            margin = unit(1, "pt"),
                            length = unit(1, "cm"),
                            tick.length = unit(1.5, "mm"),
                            ...) {
  sides <- match.arg(sides, c("t", "b"))
  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight,
                      linewidth = linewidth, linetype = linetype,
                      line_colour = line_colour %||% colour,
                      line_alpha = line_alpha %||% alpha)

  if (empty(data) || !"x" %in% names(data)) {
    return(zeroGrob())
  }

  if (!is.unit(margin)) {
    margin <- unit(margin, "pt")
  }
  if (!"y" %in% names(data)) {
    data$y <- if (sides == "t") 1 else 0
  }
  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.unit(tick.length)) {
    tick.length <- unit(tick.length, "mm")
  }

  grid::gTree(data = data, align = align, parse = parse, sides = sides,
              margin = margin, length = length, tick.length = tick.length,
              cl = "MarginHtextGrob")
}

#' @export
makeContent.MarginVtextGrob <- function(x) {
  data <- x$data[order(x$data$y, decreasing = TRUE), , drop = FALSE]
  margin <- convertWidth(x$margin, "native", valueOnly = TRUE)
  length <- convertWidth(x$length, "native", valueOnly = TRUE)
  tick.length <- convertWidth(x$tick.length, "native", valueOnly = TRUE)
  tick.length <- ifelse(tick.length > length/2, 0, tick.length)

  one_mm <- convertWidth(unit(1, "mm"), "native", valueOnly = TRUE)
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  text <- grid::textGrob(label = lab,
                         default.units = "native",
                         gp = gpar(col = data$colour,
                                   fontsize = data$size * .pt,
                                   fontfamily = data$family,
                                   fontface = data$fontface,
                                   lineheight = data$lineheight))


  height <- vapply_dbl(1:n, function(ii) {
    row <- data[ii, , drop = FALSE]
    g <- grid::textGrob(label = lab[ii],
                        gp = gpar(col = row$colour,
                                  fontsize = row$size * .pt,
                                  fontfamily = row$family,
                                  fontface = row$fontface,
                                  lineheight = row$lineheight))
    convertHeight(grid::grobHeight(g), "native", valueOnly = TRUE)
  })

  data$yend <- adjust_margin(data$y, height + 2 * margin)

  extra <- setdiff(names(data), c("x", "y", "xend", "yend", "label", "size",
                                  "family", "fontface", "lineheight", "group"))
  if (x$sides == "r") {
    if (isTRUE(x$align)) {
      data$xend <- max(data$x, na.rm = TRUE) + length
    } else {
      data$xend <- data$x + length
    }

    text <- grid::editGrob(text,
                           x = unit(data$xend + one_mm, "native"),
                           y = unit(data$yend, "native"),
                           hjust = 0,
                           vjust = 0.5)

    data <- data_frame0(x = c(data$x, data$xend - length + tick.length, data$xend - tick.length, data$xend),
                        y = c(data$y, data$y, data$yend, data$yend),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  } else {
    if (isTRUE(x$align)) {
      data$xend <- min(data$x, na.rm = TRUE) - length
    } else {
      data$xend <- data$x - length
    }

    text <- grid::editGrob(text,
                           x = unit(data$xend - one_mm, "native"),
                           y = unit(data$yend, "native"),
                           hjust = 1,
                           vjust = 0.5)

    data <- data_frame0(x = c(data$x, data$xend + length - tick.length, data$xend + tick.length, data$xend),
                        y = c(data$y, data$y, data$yend, data$yend),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  }

  line <- grid::polylineGrob(x = data$x,
                             y = data$y,
                             id = data$group,
                             gp = gpar(col = alpha(data$line_colour, data$line_alpha),
                                       lwd = data$linewidth * .pt,
                                       lty = data$linetype,
                                       lineend = "butt",
                                       linejoin = "round",
                                       linemitre = 10),
                             default.units = "native")

  grid::setChildren(x, grid::gList(line, text))
}

#' @export
makeContent.MarginHtextGrob <- function(x) {
  data <- x$data[order(x$data$x, decreasing = TRUE), , drop = FALSE]
  margin <- convertWidth(x$margin, "native", valueOnly = TRUE)
  length <- convertWidth(x$length, "native", valueOnly = TRUE)
  tick.length <- convertWidth(x$tick.length, "native", valueOnly = TRUE)
  tick.length <- ifelse(tick.length > length/2, 0, tick.length)

  one_mm <- convertWidth(unit(1, "mm"), "native", valueOnly = TRUE)
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  text <- grid::textGrob(label = lab,
                         default.units = "native",
                         gp = gpar(col = data$colour,
                                   fontsize = data$size * .pt,
                                   fontfamily = data$family,
                                   fontface = data$fontface,
                                   lineheight = data$lineheight))

  width <- vapply_dbl(1:n, function(ii) {
    row <- data[ii, , drop = FALSE]
    g <- grid::textGrob(label = lab[ii],
                        gp = gpar(col = row$colour,
                                  fontsize = row$size * .pt,
                                  fontfamily = row$family,
                                  fontface = row$fontface,
                                  lineheight = row$lineheight))
    convertWidth(grid::grobHeight(g), "native", valueOnly = TRUE)
  })

  data$xend <- adjust_margin(data$x, width + 2*margin)

  extra <- setdiff(names(data), c("x", "y", "xend", "yend", "label", "size",
                                  "family", "fontface", "lineheight", "group"))
  if (x$sides == "t") {
    if (isTRUE(x$align)) {
      data$yend <- max(data$y, na.rm = TRUE) + length
    } else {
      data$yend <- data$y + length
    }

    text <- grid::editGrob(text,
                           y = unit(data$yend + one_mm, "native"),
                           x = unit(data$xend, "native"),
                           rot = 90,
                           hjust = 0,
                           vjust = 0.5)

    data <- data_frame0(y = c(data$y, data$yend - length + tick.length, data$yend - tick.length, data$yend),
                        x = c(data$x, data$x, data$xend, data$xend),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  } else {
    if (isTRUE(x$align)) {
      data$yend <- min(data$y, na.rm = TRUE) - length
    } else {
      data$yend <- data$y - length
    }

    text <- grid::editGrob(text,
                           y = unit(data$yend - one_mm, "native"),
                           x = unit(data$xend, "native"),
                           rot = -90,
                           hjust = 0,
                           vjust = 0.5)

    data <- data_frame0(y = c(data$y, data$yend + length - tick.length, data$yend + tick.length, data$yend),
                        x = c(data$x, data$x, data$xend, data$xend),
                        data[rep(1:n, 4), extra, drop = FALSE],
                        group = rep(1:n, 4))
  }

  line <- grid::polylineGrob(x = data$x,
                             y = data$y,
                             id = data$group,
                             gp = gpar(col = alpha(data$line_colour, data$line_alpha),
                                       lwd = data$linewidth * .pt,
                                       lty = data$linetype,
                                       lineend = "butt",
                                       linejoin = "round",
                                       linemitre = 10),
                             default.units = "native")

  grid::setChildren(x, grid::gList(line, text))
}

#' @noRd
adjust_margin <- function(x, height, upper = NULL, lower = NULL, arc = FALSE) {
  if (empty(x)) {
    return(x)
  }

  if (is.null(upper)) {
    if (isTRUE(arc)) {
      upper <- 360
    } else {
      upper <- 1
    }
  }

  lower <- lower %||% 0

  if (sum(height) > (upper - lower)) {
    height <- height/sum(height)*(upper - lower)
  }

  ## adjust the x position to ensure that each element does not overlap
  n <- length(x)
  space <- upper - lower - sum(height)
  if (space <= 0) {
    return(upper - cumsum(height) + height/2)
  }

  space_t <- c(upper, x[-n]) - x - c(height[1]/2, (height[-n] + height[-1])/2)
#  space_b <- x - c(x[-1], lower) - c(height + c(height[-1], 0))/2
  negative <- space_t < 0
  x <- x + cumsum(ifelse(negative, space_t, 0))
  space_t[negative] <- 0

  if (sum(space_t) > space) {
    space_t <- space_t/sum(space_t)*space
    x <- upper - cumsum(height) + height/2 - cumsum(space_t)
  }

  x
}


