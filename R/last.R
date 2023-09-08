## Note: This function is used in the same way as last_plot() in ggplot2,
## and the reason for not using last_plot() is to avoid changing the default
## settings of ggplot2.
## All functions copy from ggplot2

#' @noRd
.plot_store <- function() {
  .current_plot <- NULL

  list(get = function() .current_plot,
       set = function(value) .current_plot <<- value)
}

#' @noRd
.store <- .plot_store()

#' Set the current plot
#' @seealso  [current_plot()]
#' @export
#' @keywords internal
set_current_plot <- function(value) {
  .store$set(value)
}

#' Retrieve the current plot
#' @seealso  [arcsave()]
#' @export
#' @keywords internal
current_plot <- function() .store$get()

#' Export ArcPlot
#' @description This function is modified (copy?) from `ggplot2::ggsave`, and used
#' to export ArcPlot object.
#' @param plot an ArcPlot object.
#' @inheritParams ggplot2::ggsave
#' @export
#' @keywords internal
arcsave <- function(filename,
                    plot = current_plot(),
                    device = NULL,
                    path = NULL,
                    scale = 1,
                    width = NA,
                    height = NA,
                    units = c("in", "cm", "mm", "px"),
                    dpi = 300,
                    limitsize = TRUE,
                    bg = NULL,
                    ...) {
  if (length(filename) != 1) {
    if (length(filename) == 0) {
      cli::cli_abort("{.arg filename} cannot be empty.")
    }
    len <- length(filename)
    filename <- filename[1]
    cli::cli_warn(c("{.arg filename} must have length 1, not length {len}.",
                    `!` = "Only the first, {.file {filename}}, will be used."))
  }
  dpi <- parse_dpi(dpi)
  dev <- plot_dev(device, filename, dpi = dpi)
  dim <- plot_dim(c(width, height), scale = scale, units = units,
                  limitsize = limitsize, dpi = dpi)
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  if (is.null(bg)) {
    if (inherits(plot, "ggplot")) {
      theme <- plot$theme
    } else {
      theme <- attr(plot, "theme")
    }
    if (is.null(theme)) {
      theme <- theme_get()
    } else {
      theme <- plot_theme(list(theme = theme), default = theme_get())
    }

    bg <- theme$plot.background

    if (inherits(bg, "element_blank")) {
      bg <- "transparent"
    } else {
      bg <- bg$fill %||% "transparent"
    }

  }
  old_dev <- grDevices::dev.cur()
  dev(filename = filename, width = dim[1], height = dim[2], bg = bg, ...)
  on.exit(utils::capture.output({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  }))
  grid.draw(plot)
  invisible(filename)
}

#' @export
grid.draw.ArcPlot <- function(x, recording = TRUE) {
  print(x)
}
