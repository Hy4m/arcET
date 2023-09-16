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
#' @param parse logical, if TRUE will parse text to expression.
#' @param sides position of margin text. `sides` must be one of "l" or "r" for
#' `ArcMarginVtextGrob()`, and must be one of "t" or "b" for `ArcMarginHtextGrob()`.
#' @param margin a grid unit object specifying top and bottom text margin.
#' @param length length of link, should be a grid unit object.
#' @param tick.length tick length of link, should be a grid unit object or character
#' ratio.
#' @param line.gp other parameters of link, see \code{?grid::gpar} for details.
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
                               parse = FALSE,
                               sides = "l",
                               margin = unit(1, "pt"),
                               length = unit(1, "cm"),
                               tick.length = "10%",
                               line.gp = gpar(),
                               upper = NULL,
                               lower = NULL,
                               ...) {
  sides <- match.arg(sides, c("l", "r"))
  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight)

  if (empty(data) || !"y" %in% names(data)) {
    return(zeroGrob())
  }

  if (!"x" %in% names(data)) {
    data$x <- 90
  }
  if (length(unique(data$x)) != 1){
    cli::cli_abort("{.arg x} mush be must be NULL or one-length numeric.")
  }

  if (!is.unit(margin)) {
    margin <- unit(margin, "cm")
  }
  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }

  grid::gTree(data = data,
              sides = sides,
              parse = parse,
              margin = margin,
              length = length,
              tick.length = tick.length,
              line.gp = line.gp,
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
                               parse = FALSE,
                               margin = unit(1, "pt"),
                               length = unit(1, "cm"),
                               tick.length = "10%",
                               line.gp = gpar(),
                               upper = NULL,
                               lower = NULL,
                               ...) {
  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight)

  if (empty(data) || !"x" %in% names(data)) {
    return(zeroGrob())
  }

  if (!"y" %in% names(data)) {
    data$y <- 1
  }
  if (length(unique(data$y)) != 1){
    cli::cli_abort("{.arg y} mush be must be NULL or one-length numeric.")
  }

  if (!is.unit(margin)) {
    margin <- unit(margin, "cm")
  }
  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }

  grid::gTree(data = data,
              parse = parse,
              margin = margin,
              length = length,
              tick.length = tick.length,
              line.gp = line.gp,
              upper = upper,
              lower = lower,
              cl = "ArcMarginHtextGrob")
}

#' @export
makeContent.ArcMarginHtextGrob <- function(x) {
  data <- transform(x$data, x = x %% 360)
  data <- data[order(data$x, decreasing = TRUE), , drop = FALSE]
  length <- x$length
  tick.length <- x$tick.length
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  margin <- convert_height(x$margin, "native", valueOnly = TRUE)
  length <- convert_height(length, "native", valueOnly = TRUE)
  if (is.character(tick.length)) {
    tick.length <- as.numeric(gsub("%", "", tick.length, fixed = TRUE))/100
    tick.length <- length * tick.length
  } else {
    tick.length <- convert_height(tick.length, "native", valueOnly = TRUE)
  }

  one_mm <- convert_height(unit(1, "mm"), "native", valueOnly = TRUE)
  data$yend <- data$y + length

  height <- vapply_dbl(1:n, function(ii) {
    row <- data[ii, , drop = FALSE]
    g <- grid::textGrob(label = lab[ii],
                        gp = gpar(col = row$colour,
                                  fontsize = row$size * .pt,
                                  fontfamily = row$family,
                                  fontface = row$fontface,
                                  lineheight = row$lineheight))
    convert_height(grid::grobHeight(g), "native", valueOnly = TRUE)
  })

  height <- degree(atan((height + 2*margin)/(data$yend + one_mm)/2) * 2)
  data$xend <- adjust_margin(data$x, height, upper = x$upper, lower = x$lower,
                             arc = TRUE)
  data$yend <- data$y + length

  text <- transform(data, x = xend, y = yend + one_mm, angle = text_angle(xend, "clockwise"))
  if (tick.length < length) {
    line <- data_frame0(y = c(data$y, data$y + tick.length, data$yend - tick.length, data$yend),
                        x = c(data$x, data$x, data$xend, data$xend),
                        data[rep(1:n, 4), , drop = FALSE],
                        group = rep(1:n, 4))
  } else {
    line <- data_frame0(x = c(data$x, data$xend),
                        y = c(data$y, data$yend),
                        data[rep(1:n, 2), , drop = FALSE],
                        group = rep(1:n, 2))
  }

  text <- polar2cartesian(text)
  line <- polar2cartesian(line)

  line <- grid::polylineGrob(x = line$x,
                             y = line$y,
                             id = line$group,
                             gp = x$line.gp,
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
  sides <- x$sides
  length <- x$length
  tick.length <- x$tick.length
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  margin <- convert_width(x$margin, "native", valueOnly = TRUE)
  length <- convert_width(length, "native", valueOnly = TRUE)
  if (is.character(tick.length)) {
    tick.length <- as.numeric(gsub("%", "", tick.length, fixed = TRUE))/100
    tick.length <- length * tick.length
  } else {
    tick.length <- convert_width(tick.length, "native", valueOnly = TRUE)
  }

  one_mm <- convert_width(unit(1, "mm"), "native", valueOnly = TRUE)
  vp <- grid::viewport(x = 0,
                       y = 0,
                       width = unit(2, "native"),
                       height = unit(1, "native"),
                       just = c(0.5, 0),
                       angle = (data$x[1] %% 360) - 90,
                       xscale = c(-1, 1),
                       yscale = c(0, 1),
                       default.units = "native")

  height <- vapply_dbl(1:n, function(ii) {
    row <- data[ii, , drop = FALSE]
    g <- grid::textGrob(label = lab[ii],
                        gp = gpar(col = row$colour,
                                  fontsize = row$size * .pt,
                                  fontfamily = row$family,
                                  fontface = row$fontface,
                                  lineheight = row$lineheight))
    convert_width(grid::grobHeight(g), "native", valueOnly = TRUE)
  })

  data$yend <- adjust_margin(data$y, height + 2*margin, upper = x$upper, lower = x$lower,
                             arc = FALSE)
  if (sides == "r") {
    text <- transform(data, x = length + one_mm, y = yend, vjust = 0.5, hjust = 0)
    if (tick.length < length) {
      line <- data_frame0(y = c(data$y, data$y, data$yend, data$yend),
                          x = c(rep(c(0, tick.length, length - tick.length, length), each = n)),
                          data[rep(1:n, 4), , drop = FALSE],
                          group = rep(1:n, 4))
    } else {
      line <- data_frame0(x = rep(c(0, length), each = n),
                          y = c(data$y, yend),
                          data[rep(1:n, 2), , drop = FALSE],
                          group = rep(1:n, 2))
    }
  } else {
    text <- transform(data, x = -length - one_mm, y = yend, vjust = 0.5, hjust = 1)
    if (tick.length < length) {
      line <- data_frame0(y = c(data$y, data$y, data$yend, data$yend),
                          x = c(rep(c(0, -tick.length, -length + tick.length, -length), each = n)),
                          data[rep(1:n, 4), , drop = FALSE],
                          group = rep(1:n, 4))
    } else {
      line <- data_frame0(x = rep(c(0, -length), each = n),
                          y = c(data$y, yend),
                          data[rep(1:n, 2), , drop = FALSE],
                          group = rep(1:n, 2))
    }
  }

  line <- grid::polylineGrob(x = line$x,
                             y = line$y,
                             id = line$group,
                             gp = x$line.gp,
                             default.units = "native",
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

  grid::setChildren(x, grid::gList(line, text))
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
#' @param parse logical, if TRUE will parse text to expression.
#' @param sides position of margin text. `sides` must be one of "l" or "r" for
#' `MarginVtextGrob()`, and must be one of "t" or "b" for `MarginHtextGrob()`.
#' @param margin a grid unit object specifying top and bottom text margin.
#' @param length length of link, should be a grid unit object.
#' @param tick.length tick length of link, should be a grid unit object or character
#' ratio.
#' @param line.gp other parameters of link, see \code{?grid::gpar} for details.
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
                            parse = FALSE,
                            sides = "r",
                            margin = unit(1, "pt"),
                            length = unit(1, "cm"),
                            tick.length = "10%",
                            line.gp = gpar(),
                            ...) {
  sides <- match.arg(sides, c("r", "l"))
  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight)

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
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }

  grid::gTree(data = data, parse = parse, sides = sides, margin = margin,
              length = length, tick.length = tick.length, line.gp = line.gp,
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
                            parse = FALSE,
                            sides = "t",
                            margin = unit(1, "pt"),
                            length = unit(1, "cm"),
                            tick.length = "10%",
                            line.gp = gpar(),
                            ...) {
  sides <- match.arg(sides, c("t", "b"))
  data <- data_frame0(label = label, x = x, y = y, colour = colour,
                      alpha = alpha, size = size, family = family,
                      fontface = fontface, lineheight = lineheight)

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
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }

  grid::gTree(data = data, parse = parse, sides = sides, margin = margin,
              length = length, tick.length = tick.length, line.gp = line.gp,
              cl = "MarginHtextGrob")
}

#' @export
makeContent.MarginVtextGrob <- function(x) {
  data <- x$data[order(x$data$y, decreasing = TRUE), , drop = FALSE]
  sides <- x$sides
  margin <- x$margin
  length <- x$length
  tick.length <- x$tick.length
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  margin <- convertWidth(margin, "native", valueOnly = TRUE)
  length <- convertWidth(length, "native", valueOnly = TRUE)
  if (is.character(tick.length)) {
    tick.length <- as.numeric(gsub("%", "", tick.length, fixed = TRUE))/100
    tick.length <- length * tick.length
  } else {
    tick.length <- convertWidth(tick.length, "native", valueOnly = TRUE)
  }

  grob <- grid::textGrob(label = lab,
                         default.units = "native",
                         gp = gpar(col = data$colour,
                                   fontsize = data$size * .pt,
                                   fontfamily = data$family,
                                   fontface = data$fontface,
                                   lineheight = data$lineheight))
  one_mm <- convertWidth(unit(1, "mm"), "native", valueOnly = TRUE)

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

  extra <- setdiff(names(data), c("x", "y", "xend", "yend"))
  if (sides == "r") {
    data$xend <- data$x + length
    grob <- grid::editGrob(grob,
                           x = unit(data$xend + one_mm, "native"),
                           y = unit(data$yend, "native"),
                           hjust = 0,
                           vjust = 0.5)

    if (tick.length < length) {
      data <- data_frame0(x = c(data$x, data$x + tick.length, data$xend - tick.length, data$xend),
                          y = c(data$y, data$y, data$yend, data$yend),
                          data[rep(1:n, 4), extra, drop = FALSE],
                          group = rep(1:n, 4))
    } else {
      data <- data_frame0(x = c(data$x, data$xend),
                          y = c(data$y, data$yend),
                          data[rep(1:n, 2), extra, drop = FALSE],
                          group = rep(1:n, 2))
    }

  } else {
    data$xend <- data$x - length
    grob <- grid::editGrob(grob,
                           x = unit(data$xend - one_mm, "native"),
                           y = unit(data$yend, "native"),
                           hjust = 1,
                           vjust = 0.5)

    if (tick.length < length/2) {
      data <- data_frame0(x = c(data$x, data$x - tick.length, data$xend + tick.length, data$xend),
                          y = c(data$y, data$y, data$yend, data$yend),
                          data[rep(1:n, 4), extra, drop = FALSE],
                          group = rep(1:n, 4))
    } else {
      data <- data_frame0(x = c(data$x, data$xend),
                          y = c(data$y, data$yend),
                          data[rep(1:n, 2), extra, drop = FALSE],
                          group = rep(1:n, 2))
    }
  }

  line <- grid::polylineGrob(x = data$x,
                             y = data$y,
                             id = data$group,
                             gp = x$line.gp,
                             default.units = "native")

  grid::setChildren(x, grid::gList(line, grob))
}

#' @export
makeContent.MarginHtextGrob <- function(x) {
  data <- x$data[order(x$data$x, decreasing = TRUE), , drop = FALSE]
  sides <- x$sides
  margin <- x$margin
  length <- x$length
  tick.length <- x$tick.length
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

  margin <- convertHeight(margin, "native", valueOnly = TRUE)
  length <- convertHeight(length, "native", valueOnly = TRUE)
  if (is.character(tick.length)) {
    tick.length <- as.numeric(gsub("%", "", tick.length, fixed = TRUE))/100
    tick.length <- length * tick.length
  } else {
    tick.length <- convertHeight(tick.length, "native", valueOnly = TRUE)
  }

  grob <- grid::textGrob(label = lab,
                         default.units = "native",
                         gp = gpar(col = data$colour,
                                   fontsize = data$size * .pt,
                                   fontfamily = data$family,
                                   fontface = data$fontface,
                                   lineheight = data$lineheight))
  one_mm <- convertHeight(unit(1, "mm"), "native", valueOnly = TRUE)

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
  if (sides == "t") {
    data$yend <- data$y + length
    grob <- grid::editGrob(grob,
                           y = unit(data$yend + one_mm, "native"),
                           x = unit(data$xend, "native"),
                           rot = 90,
                           hjust = 0,
                           vjust = 0.5)

    if (tick.length < length) {
      data <- data_frame0(y = c(data$y, data$y + tick.length, data$yend - tick.length, data$yend),
                          x = c(data$x, data$x, data$xend, data$xend),
                          data[rep(1:n, 4), , drop = FALSE],
                          group = rep(1:n, 4))
    } else {
      data <- data_frame0(x = c(data$x, data$xend),
                          y = c(data$y, data$yend),
                          data[rep(1:n, 2), , drop = FALSE],
                          group = rep(1:n, 2))
    }
  } else {
    data$yend <- data$y - length
    grob <- grid::editGrob(grob,
                           y = unit(data$yend - one_mm, "native"),
                           x = unit(data$xend, "native"),
                           rot = -90,
                           hjust = 0,
                           vjust = 0.5)

    if (tick.length < length/2) {
      data <- data_frame0(y = c(data$y, data$y - tick.length, data$yend + tick.length, data$yend),
                          x = c(data$x, data$x, data$xend, data$xend),
                          data[rep(1:n, 4), , drop = FALSE],
                          group = rep(1:n, 4))
    } else {
      data <- data_frame0(x = c(data$x, data$xend),
                          y = c(data$y, data$yend),
                          data[rep(1:n, 2), , drop = FALSE],
                          group = rep(1:n, 2))
    }
  }

  line <- grid::polylineGrob(x = data$x,
                             y = data$y,
                             id = data$group,
                             gp = x$line.gp,
                             default.units = "native")

  grid::setChildren(x, grid::gList(line, grob))
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


