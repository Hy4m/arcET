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

  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }

  grid::gTree(data = data, parse = parse, sides = sides, length = length,
              tick.length = tick.length, line.gp = line.gp,
              cl = "MarginVtextGrob")
}

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

  if (!"y" %in% names(data)) {
    data$y <- if (sides == "t") 1 else 0
  }

  if (!is.unit(length)) {
    length <- unit(length, "cm")
  }
  if (!is.character(tick.length) && !is.unit(tick.length)) {
    cli::cli_abort("{.arg tick.length} must be an unit or character object.")
  }
  grid::gTree(data = data, parse = parse, sides = sides, length = length,
              tick.length = tick.length, line.gp = line.gp,
              cl = "MarginHtextGrob")
}

#' @export
makeContent.MarginVtextGrob <- function(x) {
  data <- x$data[order(x$data$y, decreasing = TRUE), , drop = FALSE]
  sides <- x$sides
  length <- x$length
  tick.length <- x$tick.length
  n <- nrow(data)

  lab <- data$label
  if (isTRUE(parse)) {
    lab <- parse_safe(lab)
  }

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

  data$yend <- adjust_margin(data$y, height)

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

  data$xend <- adjust_margin(data$x, width)
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
