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
#' @param filename file name to create on disk.
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
  ggplot2::ggsave(filename = filename,
                  plot = plot,
                  device = device,
                  path = path,
                  scale = scale,
                  width = width,
                  height = height,
                  units = units,
                  dpi = dpi,
                  limitsize = limitsize,
                  bg = bg,
                  ...)
}

#' @export
grid.draw.ArcPlot <- function(x, recording = TRUE) {
  print(x)
}
