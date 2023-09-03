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

  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }

  grid::gTree(data = data,
              sides = sides,
              parse = parse,
              length = length,
              tick.length = tick.length,
              line.gp = line.gp,
              upper = upper,
              lower = lower,
              cl = "ArcMarginVtextGrob")
}


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

  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }

  grid::gTree(data = data,
              parse = parse,
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

  length <- convertHeight(length, "native", valueOnly = TRUE)
  if (is.character(tick.length)) {
    tick.length <- as.numeric(gsub("%", "", tick.length, fixed = TRUE))/100
    tick.length <- length * tick.length
  } else {
    tick.length <- convertHeight(tick.length, "native", valueOnly = TRUE)
  }

  one_mm <- convertHeight(unit(1, "mm"), "native", valueOnly = TRUE)
  data$yend <- data$y + length

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

  height <- degree(atan(height/(data$yend + one_mm)/2) * 2)
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

  data$yend <- adjust_margin(data$y, height, upper = x$upper, lower = x$lower,
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
