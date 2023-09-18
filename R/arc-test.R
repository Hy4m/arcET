#' @title Test function
#' @description Helper funtion used to quickly preview the results of the conversion.
#' @param plot a ggplot object.
#' @param region a `CELL` object.
#' @param ... not used.
#' @param vp 	viewport to draw plot in.
#' @param newpage draw new (empty) page first?
#' @return return grob object invisibly.
#' @rdname arc_test
#' @author Hou Yun
#' @export
#' @examples
#' if (FALSE) { # Not Run
#'   library(ggplot2)
#'   library(arcET)
#'   ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'   arc_test()
#' }
arc_test <- function(plot = ggplot2::last_plot(),
                     region = NULL,
                     ...,
                     vp = NULL,
                     newpage = is.null(vp)) {
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a ggplot object.")
  }
  if (!inherits(plot$facet, "FacetNull")) {
    cli::cli_abort("Facet's plot has not be implemented yet.")
  }

  clss <- class(plot)[1]
  region <- region %||% CELL(120, 60, 0.4)
  plot <- init_cell(arcplot(), data = plot, region = region)

  plot <- tryCatch(ArcPlot_build(plot),
                   error = function(e) {
                     cli::cli_abort("Connot convert {.cls {clss}} to arcplot...")
                   })

  if (newpage) {
    grid::grid.newpage()
  }

  grDevices::recordGraphics(requireNamespace("arcET", quietly = TRUE),
                            list(), getNamespace("arcET"))
  if (is.null(vp)) {
    grid::grid.draw(plot)
  } else {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
    }
    grid::grid.draw(plot)

    grid::upViewport()
  }

  invisible(plot)
}
