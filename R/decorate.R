#' Annotate Plot
#' @description Useful functions to add annotate based on polar coordinate.
#' @param plot an ArcPlot object.
#' @param x,xend,xmin,xmax,xintercept numeric vector (in degree) specifying x-values.
#' @param y,yend,ymin,ymax,yintercept positive numeric vector (in radius) specifying y-values.
#' @param label expression or character.
#' @param CellID character IDs indicating which element will be modified.
#' @param auto_adjust logical, if TRUE will adjust angle based on x-axis value.
#' @param ... other parameters passing `Arc*Grob()` function.
#' @return a modified ArcPlot object.
#' @author Hou Yun
#' @rdname decorate
#' @export
decorate_text <- function(plot,
                          label,
                          x = mid_x,
                          y = mid_y,
                          CellID = NULL,
                          auto_adjust = TRUE,
                          ...) {
  stopifnot(is_ArcPlot(plot))
  if (missing(label) || empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcTextGrob(label = label,
                        x = rlang::eval_tidy(x, region),
                        y = rlang::eval_tidy(y, region),
                        auto_adjust = auto_adjust,
                        ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_label <- function(plot,
                           label,
                           x = mid_x,
                           y = mid_y,
                           CellID = NULL,
                           auto_adjust = TRUE,
                           ...) {
  stopifnot(is_ArcPlot(plot))
  if (missing(label) || empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcLabelGrob(label = label,
                         x = rlang::eval_tidy(x, region),
                         y = rlang::eval_tidy(y, region),
                         auto_adjust = auto_adjust,
                         ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_bannertext <- function(plot,
                                label,
                                x = mid_x,
                                y = mid_y,
                                CellID = NULL,
                                ...) {
  stopifnot(is_ArcPlot(plot))
  if (missing(label) || empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcBannerTextGrob(label = label,
                              x = rlang::eval_tidy(x, region),
                              y = rlang::eval_tidy(y, region),
                              ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_rect <- function(plot,
                          xmin = x.range[1],
                          ymin = y.range[1],
                          xmax = x.range[2],
                          ymax = y.range[2],
                          CellID = NULL,
                          ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  xmin <- rlang::enquo(xmin)
  ymin <- rlang::enquo(ymin)
  xmax <- rlang::enquo(xmax)
  ymax <- rlang::enquo(ymax)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcRectGrob(xmin = rlang::eval_tidy(xmin, region),
                        ymin = rlang::eval_tidy(ymin, region),
                        xmax = rlang::eval_tidy(xmax, region),
                        ymax = rlang::eval_tidy(ymax, region),
                        ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_polygon <- function(plot,
                             x = NULL,
                             y = NULL,
                             CellID = NULL,
                             ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    xx <- rlang::eval_tidy(x, region)
    yy <- rlang::eval_tidy(y, region)
    if (any(is.null(xx), is.null(yy))) next

    anno <- ArcPolygonGrob(x = xx,
                           y = yy,
                           ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_point <- function(plot,
                           x = mid_x,
                           y = mid_y,
                           CellID = NULL,
                           ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcPointsGrob(x = rlang::eval_tidy(x, region),
                         y = rlang::eval_tidy(y, region),
                         ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_line <- function(plot,
                          x = x.range,
                          y = y.range,
                          CellID = NULL,
                          ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcLineGrob(x = rlang::eval_tidy(x, region),
                        y = rlang::eval_tidy(y, region),
                        ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_path <- function(plot,
                          x = x.range,
                          y = y.range,
                          CellID = NULL,
                          ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcPathGrob(x = rlang::eval_tidy(x, region),
                        y = rlang::eval_tidy(y, region),
                        ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_segment <- function(plot,
                             x = x.range[1],
                             y = y.range[2],
                             xend = x.range[2],
                             yend = x.range[2],
                             CellID = NULL,
                             ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  xend <- rlang::enquo(xend)
  yend <- rlang::enquo(yend)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcSegmentsGrob(x = rlang::eval_tidy(x, region),
                            y = rlang::eval_tidy(y, region),
                            xend = rlang::eval_tidy(xend, region),
                            yend = rlang::eval_tidy(yend, region),
                            ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_hline <- function(plot,
                           yintercept = mid_y,
                           CellID = NULL,
                           ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  yintercept <- rlang::enquo(yintercept)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcHlineGrob(yintercept = rlang::eval_tidy(yintercept, region),
                         region = region,
                         ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @rdname decorate
#' @export
decorate_vline <- function(plot,
                           xintercept = mid_x,
                           CellID = NULL,
                           ...) {
  stopifnot(is_ArcPlot(plot))
  if (empty(plot) || is.null(CellID) || !any(CellID %in% plot$CellID)) {
    return(plot)
  }

  xintercept <- rlang::enquo(xintercept)
  ids <- match(CellID, plot$CellID)

  annotate <- list()
  for (ii in ids) {
    region <- plot$region[[ii]]
    anno <- ArcVlineGrob(xintercept = rlang::eval_tidy(xintercept, region),
                         region = region,
                         ...)
    annotate <- c(annotate, list(anno))
  }

  if (is.null(attr(plot, "annotate"))) {
    attr(plot, "annotate") <- annotate
  } else {
    attr(plot, "annotate") <- c(attr(plot, "annotate"), annotate)
  }

  set_current_plot(plot)

  plot
}

#' @noRd
utils::globalVariables(
  c("mid_x",
    "mid_y",
    "x.range",
    "y.range"),
  add = TRUE
)
