
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ArcAnnotate <- function(object, plot, object_name) {
  if (!is_CellPlot(plot)) {
    cli::cli_warn("{.cls, {class(object)[1]}} can be added on CellPlot only.")
    return(plot)
  }

  attr(plot, "annotate") <- c(attr(plot, "annotate"), list(object))
  plot
}

#' @export
ggplot_add.Facet <- function(object, plot, object_name) {
  if (is_CellPlot(plot)) {
    cli::cli_warn("{CellPlot don't support facet.")
    return(plot)
  }

  ggplot_add(object, plot, object_name)
}



