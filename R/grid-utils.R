text_width <- function(text,
                       units = "native",
                       gp = gpar(),
                       vp = NULL) {
  if (is.null(vp)) {
    vp <- grid::viewport(width = unit(2, "native"),
                         height = unit(2, "native"),
                         xscale = c(-1L, 1L),
                         yscale = c(-1L, 1L),
                         angle = 0)
  }

  grid::pushViewport(vp)
  try(on.exit(grid::popViewport()))
  vapply(text, function(s) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = gp)),
                       unitTo = units, valueOnly = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

text_height <- function(text,
                        units = "native",
                        gp = gpar(),
                        vp = NULL) {
  if (is.null(vp)) {
    vp <- grid::viewport(width = unit(2, "native"),
                         height = unit(2, "native"),
                         xscale = c(-1L, 1L),
                         yscale = c(-1L, 1L),
                         angle = 0)
  }

  grid::pushViewport(vp)
  try(on.exit(grid::popViewport()))
  vapply(text, function(s) {
    grid::convertHeight(grid::grobHeight(grid::textGrob(s, gp = gp)),
                        unitTo = units, valueOnly = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

convert_width <- function(x,
                          unitTo = "native",
                          valueOnly = TRUE,
                          vp = NULL) {
  if (is.null(vp)) {
    vp <- grid::viewport(width = unit(2, "native"),
                         height = unit(2, "native"),
                         xscale = c(-1L, 1L),
                         yscale = c(-1L, 1L),
                         angle = 0)
  }

  grid::pushViewport(vp)
  try(on.exit(grid::popViewport()))

  x <- vapply(seq_along(x), function(idx) {
    grid::convertWidth(x[idx], unitTo = unitTo, valueOnly = TRUE)
  }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)

  if (!isTRUE(valueOnly)) {
    x <- unit(x, unitTo)
  }

  x
}

convert_height <- function(x,
                           unitTo = "native",
                           valueOnly = TRUE,
                           vp = NULL) {
  if (is.null(vp)) {
    vp <- grid::viewport(width = unit(2, "native"),
                         height = unit(2, "native"),
                         xscale = c(-1L, 1L),
                         yscale = c(-1L, 1L),
                         angle = 0)
  }

  grid::pushViewport(vp)
  try(on.exit(grid::popViewport()))


  x <- vapply(seq_along(x), function(idx) {
    grid::convertHeight(x[idx], unitTo = unitTo, valueOnly = TRUE)
  }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)

  if (!isTRUE(valueOnly)) {
    x <- unit(x, unitTo)
  }

  x
}

new_panel <- function(...,
                      xscale = c(-1L, 1L),
                      yscale = c(-1L, 1L),
                      width = unit(12, "cm"),
                      height = unit(12, "cm"),
                      reset = TRUE) {
  if (isTRUE(reset)) {
    grid::grid.newpage()
  }

  if (!is.unit(width)) {
    width <- unit(width, "cm")
  }
  if (!is.unit(height)) {
    height <- unit(height, "cm")
  }

  width <- unclass(width)[1]
  height <- unclass(height)[1]

  if (any(diff(xscale) == 0, diff(yscale) == 0) ||
      any(width == 0, height == 0)) {
    vp <- grid::viewport(
      width = 0,
      height = 0,
      xscale = xscale,
      yscale = yscale,
      angle = 0,
      ...
    )
  } else {
    r1 <- height/diff(yscale)
    r2 <- width/diff(xscale)
    if (r1 > r2) {
      width <- diff(xscale)*r1
    } else {
      height <- diff(yscale)*r2
    }

    vp <- grid::viewport(
      width = unit(width, "cm"),
      height = unit(height, "cm"),
      xscale = xscale,
      yscale = yscale,
      angle = 0,
      ...
    )
  }

  grid::pushViewport(vp)
}

ArcViewPort <- function(coord = PANEL(),
                        region = CELL(),
                        background = NA,
                        border = "black",
                        guide = "none",
                        clip = TRUE) {
  stopifnot(is_PANEL(coord))
  stopifnot(is_CELL(region))

  if (isFALSE(guide)) {
    guide <- "none"
  }
  if (isTRUE(guide)) {
    guide <- "all"
  }
  guide <- match.arg(guide, c("none", "x", "y", "all"))

  if (identical(tolower(guide), "on")) {
    clip <- TRUE
  }
  if (identical(tolower(guide), "off")) {
    clip <- FALSE
  }

  structure(.Data = rlang::as_environment(list(coord = coord,
                                               region = region,
                                               background = background,
                                               border = border,
                                               guide = guide,
                                               clip = clip)),
            class = "ArcViewPort")
}

zeroGrob <- ggplot2::zeroGrob

