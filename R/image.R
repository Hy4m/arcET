#' Arc Raster Grob
#' @description These functions can draw raster image on polar coordinate.
#'
#' @param image raster object or others can be converted.
#' @param x numeric vector (in degree) specifying x-values.
#' @param y positive numeric vector (in radius) specifying y-values.
#' @param angle used to specifying angle of image, and should be one of
#' "downward", "reverse.clockwise", "clockwise", "downward", "inside" or
#' "outside".
#' @param size image size in "pt".
#' @param vjust numeric vector specifying vertical justification.
#' @param keep_asp logical. If TRUE will re-compute `height` and `width`
#' based on image dimmension.
#' @param region a CELL object (created by `CELL()` function) used to set
#' the panel area.
#' @param clip logical, if TRUE will remove rows which overflow panel region.
#' @param steps step length used to split data. A smaller value means a
#' smoother curve.
#' @param simplify logical, When TRUE, line segments equal to x will not be split.
#' @param na.rm logical, if TRUE will remove NA's rows silently.
#' @param ... not used.
#' @return a grob object.
#' @rdname ArcRasterGrob
#' @author Hou Yun
#' @export
ArcRasterGrob <- function(image,
                          x = 90,
                          y = 0.5,
                          fill = "white",
                          angle = 0,
                          size = 12,
                          hjust = 0.5,
                          vjust = 0.5,
                          keep_asp = TRUE,
                          read_args = list(),
                          auto_adjust = TRUE,
                          ...) {
  if (!inherits(image, "IMAGE")) {
    image <- as_IMAGE(image, read_args = read_args)
  }
  data <- data_frame0(image = image,
                      x = x,
                      y = y,
                      fill = fill,
                      angle = angle,
                      size = size,
                      hjust = hjust,
                      vjust = vjust)

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
  exec(imageGrob, !!!data, default.units = "native", ...)
}

#' @importFrom rlang inject
imageGrob <- function(image,
                      x = 0.5,
                      y = 0.5,
                      fill = "white",
                      size = 12,
                      hjust = 0.5,
                      vjust = 0.5,
                      angle = 0,
                      keep_asp = TRUE,
                      read_args = list(),
                      default.units = "npc",
                      ...) {
  image <- as_IMAGE(image, ..., read_args = read_args)
  data <- data_frame0(image = image,
                      x = x,
                      y = y,
                      fill = fill,
                      width = size,
                      height = size,
                      angle = angle,
                      hjust = hjust,
                      vjust = vjust)
  if (any(data$width <= 0)) {
    cli::cli_warn(c("Size of imageGrob should be positive numeric",
                    i = "{sum(data$width <= 0)} rows with non-positive size will be removed"))
    data <- data[data$width > 0, , drop = FALSE]
  }

  ## remove rows with NA image
  data <- data[!is.na(data$image), , drop = FALSE]

  if (empty(data)) {
    return(zeroGrob())
  }

  grobs <- lapply(split_by_rows(data), function(row) {
    img <- attr(row$image, "image")
    mask <- attr(row$image, "mask")
    rows <- nrow(img)
    cols <- ncol(img)

    if (any(rows == 0, cols == 0)) {
      return(zeroGrob())
    } else {
      if (keep_asp) {
        r <- rows/cols
        row$width <- ifelse(r > 1, row$width/r, row$width)
        row$height <- ifelse(r > 1, row$height, row$height*r)
      }

      vp <- grid::viewport(x = row$x,
                           y = row$y,
                           width = unit(row$width*.pt, "pt"),
                           height = unit(row$height*.pt, "pt"),
                           angle = row$angle,
                           just = list(row$hjust, row$vjust),
                           default.units = default.units)
      img <- grid::rasterGrob(image = img,
                              x = unit(0.5, "npc"),
                              y = unit(0.5, "npc"),
                              width = unit(1, "npc"),
                              height = unit(1, "npc"),
                              just = "center",
                              vp = vp)
      if (!is.null(mask)) {
        has_mask <- TRUE
        mask <- grid::editGrob(mask, vp = vp, gp = gpar(fill = row$fill))
      } else {
        has_mask <- FALSE
        mask <- zeroGrob()
      }

      grid::gList(img, if (has_mask) mask)
    }
  })

  do.call(grid::gList, grobs)
}

#' @noRd
read_image <- function(image, read_args = list()) {
  if (is.null(image)) {
    return(NULL)
  }

  if (grDevices::is.raster(image)) {
    return(image)
  }

  if (inherits(image, "magick-image")) {
    return(grDevices::as.raster(image))
  }

  if (length(image) == 1 && is.na(image)) {
    return(NA)
  }

  if (is.character(image)) {
    rlang::check_installed("magick", reason = "for `read_image()`")
    read_args <- utils::modifyList(list(pdf = list(),
                                        svg = list(),
                                        other = list()),
                                   read_args)

    ext <- tolower(tools::file_ext(image))
    image <- if (identical(ext, "pdf")) {
      inject(magick::image_read_pdf(image, !!!read_args$pdf))
    } else if (identical(ext, "svg")) {
      inject(magick::image_read_svg(image, !!!read_args$svg))
    } else {
      inject(magick::image_read(image, !!!read_args$other))
    }
    grDevices::as.raster(image)
  } else {
    tryCatch(grDevices::as.raster(image), error = function(e) {
      cli::cli_abort("Cannot import a {.cls {class(image)[1]}} object to raster.")
    })
  }
}

#' @export
as_IMAGE <- function(x, ...) {
  UseMethod("as_IMAGE")
}

#' @export
as_IMAGE.raster <- function(x, mask = NULL, ...) {
  IMAGE(image = x, mask = mask)
}

#' @export
as_IMAGE.IMAGE <- function(x, mask = NULL, ...) {
  x
}

#' @export
`as_IMAGE.magick-image` <- function(x, mask = NULL, ...) {
  x <- grDevices::as.raster(x)
  IMAGE(image = x, mask = mask)
}

#' @export
as_IMAGE.character <- function(x, mask = NULL, ...) {
  as_IMAGE(x = as.list(x), mask = mask)
}

#' @export
as_IMAGE.list <- function(x, mask = NULL, ...) {
  img <- lapply(x, read_image, ...)
  Reduce("c", lapply(img, IMAGE, mask = mask))
}

#' @export
as_IMAGE.default <- function(x, ...) {
  cli::cli_abort("Cannot convert a {.cls {class(x)[1]}} object to IMAGE.")
}

IMAGE <- function(image = NULL, mask = NULL) {
  if (is.null(image)) {
    return(vctrs::new_vctr(character(0), image = NULL, mask = NULL,
                           class = "IMAGE"))
  }
  if (length(image) == 1 && is.na(image)) {
    return(vctrs::new_vctr(NA_character_, image = NULL, mask = NULL,
                           class = "IMAGE"))
  }

  if (!grDevices::is.raster(image)) {
    cli::cli_abort("{.arg image} must be a raster object.")
  }
  if (!is.null(mask) && !inherits(mask, "grob")) {
    cli::cli_abort("{.arg mask} must be a grid grob object.")
  }

  vctrs::new_vctr("", image = image, mask = mask, class = "IMAGE")
}

#' @export
format.IMAGE <- function(x, ...) {
  ifelse(is.na(x), NA, paste0("<IMAGE>"))
}
