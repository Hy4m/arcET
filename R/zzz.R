#' Import
#' @importFrom ggplot2 aes GeomText Geom calc_element %+% margin resolution ggplot
#' @importFrom rlang exec current_env
#' @importFrom grid polylineGrob gTree grid.draw segmentsGrob viewport
#' @importFrom grid setChildren gList resolveVJust convertHeight convertWidth
#' @noRd
NULL

#' @noRd
utils::globalVariables(
  c("colour",
    "fill",
    "linewidth",
    "shape",
    "size",
    "stroke",
    "violinwidth",
    "x",
    "xend",
    "xmax",
    "xmin",
    "y",
    "yend",
    "ymax",
    "ymin"),
  add = TRUE
)
