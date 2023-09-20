#' Import
#' @importFrom ggplot2 aes GeomText Geom calc_element %+% margin resolution ggplot
#' @importFrom ggplot2 element_blank GeomPolygon GeomBar GeomTile GeomCol GeomRect
#' @importFrom ggplot2 GeomStep GeomPath GeomLine element_text element_line element_rect
#' @importFrom ggplot2 GeomSegment ggproto  ggproto_parent layer element_render
#' @importFrom rlang exec current_env list2 caller_env
#' @importFrom grid polylineGrob gTree grid.draw segmentsGrob viewport gpar
#' @importFrom grid setChildren gList resolveVJust convertHeight convertWidth
#' @importFrom gtable gtable gtable_add_grob gtable_width gtable_height gtable_add_padding
#' @importFrom gtable gtable_add_cols gtable_add_rows
#' @importFrom scales alpha
#' @importFrom vctrs vec_unique
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
