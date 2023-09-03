#' @title Violin
#' @description These functions are an encapsulation of relative functions in
#' the ggplot2 package, and the only difference is that these functions do
#' not add legends by default.
#'
#' @param ... extra parameters passing to `geom_*()` function.
#' @inheritParams ggplot2::geom_violin
#' @return a gg layer object.
#' @family layer
#' @rdname layer_violin
#' @author Hou Yun
#' @export
layer_violin <- function(..., show.legend = FALSE) {
  ggplot2::geom_violin(..., show.legend = show.legend)
}

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
#' @param clip logical. Allows points to overflow outside the drawing area when
#' `clip` is FALSE.
#' @inheritParams ggplot2::geom_violin
#' @return a grid grob object.
#' @family transform
#' @author Hou Yun
#' @rdname GeomViolin
#' @export
GeomViolin2grob <- function(data,
                            trans = NULL,
                            coord = PANEL(),
                            region = CELL(),
                            ...,
                            flipped_aes = FALSE,
                            draw_quantiles = NULL,
                            clip = FALSE,
                            na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }
  if ("size" %in% names(data)) {
    data <- rename(data, "linewidth" = "size")
  }

  flipped <- "flipped_aes" %in% names(data) && any(data$flipped_aes)
  data <- flip_data(data, flipped)
  data <- transform(data, xminv = x - violinwidth * (x - xmin),
                    xmaxv = x + violinwidth * (xmax - x))

  extra <- c("y", "colour", "fill", "linewidth", "linetype", "alpha", "group")
  violin <- lapply_dfr(split(data, data$group), function(df) {
    df <- df[order(df$y), , drop = FALSE]
    new_x <- c(df$xminv, rev(df$xmaxv), df$xminv[1])
    data_frame0(x = new_x, !!!df[c(1:nrow(df), nrow(df):1, 1), extra, drop = FALSE])
  })

  violin <- flip_data(violin, flipped)
  violin <- GeomPolygon2grob(data = violin, trans = trans, coord = coord,
                             region = region, clip = clip, na.rm = na.rm, ...)

  if (length(draw_quantiles) > 0) {
    draw_quantiles <- draw_quantiles[draw_quantiles >= 0 & draw_quantiles <= 1]
  }

  has_segment <- FALSE
  if (length(draw_quantiles) > 0) {
    segs <- lapply_dfr(split(data, data$group), function(df) {
      if (scales::zero_range(range(df$y))) {
        return(NULL)
      }

      ## copy from ggplot2:::create_quantile_segment_frame
      dens <- cumsum(df$density)/sum(df$density)
      ecdf <- stats::approxfun(dens, df$y, ties = "ordered")
      ys <- ecdf(draw_quantiles)
      violin.xminvs <- (stats::approxfun(df$y, df$xminv))(ys)
      violin.xmaxvs <- (stats::approxfun(df$y, df$xmaxv))(ys)
      data_frame0(x = violin.xminvs, xend = violin.xmaxvs, y = ys, yend = ys,
                  df[1, c("colour", "linewidth", "linetype", "alpha"), drop = FALSE])
    })

    if (!empty(segs)) {
      has_segment <- TRUE
      segs <- flip_data(segs, flipped)
      segs <- GeomSegment2grob(data = segs, trans = trans, coord = coord,
                               region = region, clip = clip, na.rm = na.rm,
                               ...)
    }
  }

  grid::gList(violin, if (has_segment) segs)
}
