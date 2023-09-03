#' Arc Text Grob
#' @description These functions can draw text on polar coordinate.
#' @param label expression or character.
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
#' @param check_overlap logical value to indicate whether to check for and omit
#' overlapping text.
#' @param parse logical, if TRUE will parse text to expression.
#' @param auto_adjust logical, if TRUE will adjust angle based on x-axis value.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcTextGrob
#' @author Hou Yun
#' @export
ArcTextGrob <- function(label,
                        x = 90,
                        y = 0.5,
                        hjust = 0.5,
                        vjust = 0.5,
                        colour = "black",
                        alpha = NA,
                        angle = 0,
                        size = 3.88,
                        family = "",
                        fontface = 1,
                        lineheight = 1.2,
                        check_overlap = FALSE,
                        parse = FALSE,
                        auto_adjust = TRUE,
                        ...) {
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

  lab <- data$label
  if (is.list(lab)) {
    lab <- as.expression(lab)
  }

  if (isTRUE(parse) && !is.expression(lab)) {
    lab <- parse_safe(as.character(lab))
  }

  if (is.character(data$angle)) {
    data$angle <- text_angle(data$x, facing = data$angle)
  }

  if (isTRUE(auto_adjust)) {
    data$angle <- auto_adjust(data$x, data$angle)
  }

  data <- polar2cartesian(data)
  grid::textGrob(label = lab,
                 x = data$x,
                 y = data$y,
                 rot = data$angle,
                 hjust = data$hjust,
                 vjust = data$vjust,
                 default.units = "native",
                 gp = gpar(col = alpha(data$colour, data$alpha),
                           fontsize = data$size * .pt,
                           fontfamily = data$family,
                           fontface = data$fontface,
                           lineheight = data$lineheight),
                 check.overlap = check_overlap)
}

#' Arc Text Banner Grob
#' @description These functions can draw text banner on polar coordinate.
#'
#' @param label expression or character.
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param hjust numeric vector specifying horizontal justification.
#' @param vjust numeric vector specifying vertical justification.
#' @param colour color of text.
#' @param alpha transparency of text.
#' @param size font size of text.
#' @param family font family of text.
#' @param fontface font face of text.
#' @param lineheight height of a line as a multiple of the size of text.
#' @param outside logical.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcBannerTextGrob
#' @author Hou Yun
#' @export
ArcBannerTextGrob <- function(label,
                              x = 90,
                              y = 0.5,
                              hjust = 0.5,
                              vjust = 0.5,
                              colour = "black",
                              alpha = NA,
                              size = 3.88,
                              family = "",
                              fontface = 1,
                              lineheight = 1.1,
                              outside = FALSE,
                              ...) {
  if (is.expression(label)) {
    cli::cli_warn("`ArcBannerTextGrob()` don't support expression text.")
  }
  label <- as.character(label)
  if (any(grepl("\n", label, fixed = TRUE))) {
    cli::cli_warn(c("`ArcBannerTextGrob()` don't support multi-line text.",
                    i = "'\\n' will be replaced width \" \"."))
    label <- gsub("\n", " ", label, fixed = TRUE)
  }
  data <- data_frame0(label = label,
                      x = x,
                      y = y,
                      hjust = hjust,
                      vjust = vjust,
                      colour = colour,
                      alpha = alpha,
                      size = size,
                      family = family,
                      fontface = fontface,
                      lineheight = lineheight,
                      outside = outside)

  if (empty(data)) {
    return(zeroGrob())
  }

  grid::gTree(data = data, cl = "ArcBannerTextGrob")
}

#' @export
makeContent.ArcBannerTextGrob <- function(x) {
  data <- x$data
  n <- nrow(data)
  nm <- names(data)

  data <- lapply_dfr(split(data, seq_len(n)), function(row) {
    gp <- gpar(fontsize = row$size * .pt,
               fontfamily = row$family,
               fontface = row$fontface)
    height <- text_height(row$label, "native", gp = gp)

    if (isTRUE(row$outside)) {
      row$y <- row$y + (row$vjust - 0.5) * height
    } else {
      row$y <- row$y + (0.5 - row$vjust) * height
    }

    label <- unlist(strsplit(row$label, ""))
    width <- text_width(label, "native", gp = gp)

    angle <- 2 * degree(asin(width / (2 * row$y - height)))
    shift <- max(ceiling(sum(angle) / 360), 1) * 360

    if (isFALSE(row$outside)) {
      start <- row$x + sum(angle) * row$hjust + shift
      char_x <- start - cumsum(angle) + 0.5 * angle
      char_angle <- (char_x %% 360) - 90
    } else {
      start <- row$x - sum(angle) * (1 - row$hjust) + shift
      char_x <- start + cumsum(angle) - 0.5 * angle
      char_angle <- (char_x %% 360) + 90
    }

    data_frame0(label = label, x = char_x, angle = char_angle, hjust = 0.5,
                row[setdiff(nm, c("x", "label", "angle", "hjust"))])
  })

  data <- polar2cartesian(data)
  text <- grid::textGrob(label = data$label,
                         x = data$x,
                         y = data$y,
                         rot = data$angle,
                         hjust = data$hjust,
                         vjust = 0.5,
                         default.units = "native",
                         gp = gpar(col = alpha(data$colour, data$alpha),
                                   fontsize = data$size * .pt,
                                   fontfamily = data$family,
                                   fontface = data$fontface))

  grid::setChildren(x, grid::gList(text))
}

#' @noRd
text_angle <- function(x, facing = "downward") {
  n <- max(length(x), length(facing))
  x <- rep_len(x, n)
  facing <- rep_len(facing, n)

  all_facing <- c("reverse.clockwise", "clockwise", "downward",
                  "inside", "outside")
  facing <- ifelse(facing %in% all_facing, facing, "downward")

  out <- numeric(length(x))

  for (ii in seq_along(x)) {
    if (facing[ii] == "downward") {
      out[ii] <- 0
    } else if (facing[ii] == "outside") {
      out[ii] <- (x[ii] + 90) %% 360
    } else if (facing[ii] == "inside") {
      out[ii] <- (x[ii] + 270) %% 360
    } else if (facing[ii] == "reverse.clockwise") {
      out[ii] <- (x[ii] %% 360) - 180
    } else {
      out[ii] <- x[ii] %% 360
    }
  }

  out
}

#' @noRd
auto_adjust <- function(x, angle) {
  (angle %% 360) + ((x %% 360) - 90)
}

#' @noRd
label_data <- function(data, r0 = 0.5, r1 = 1, shift = 0) {
  n <- nrow(data)
  text <- data[setdiff(names(data), c("linewidth", "linetype", "x"))]
  text$x <- text$xend
  text$y <- r1 + shift

  tick <- 0.1 * (r1 - r0)
  line <- data[setdiff(names(data), c("alpha", "size", "fontface", "family",
                                      "angle", "group"))]
  line <- data_frame0(x = c(data$x, data$x, data$xend, data$xend),
                      y = c(rep(c(r0, r0 + tick, r1 - tick, r1), each = n)),
                      group = rep(1:n, 4),
                      line[rep(1:n, 4), setdiff(names(line), c("x", "xend")), drop = FALSE])

  list(text = text, line = line)
}
