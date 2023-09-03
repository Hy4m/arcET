#' @export
layer_images <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = FALSE,
                         inherit.aes = TRUE)
{
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomImages,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(na.rm = na.rm, ...))
}

#' @export
GeomImages2grob <- function(data,
                            trans = NULL,
                            coord = PANEL(),
                            region = CELL(),
                            ...,
                            clip = FALSE,
                            na.rm = FALSE) {
  if (empty(data)) {
    return(zeroGrob)
  }

  data <- trans(data)
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y, data$x,
                               data$angle)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x, data$y,
                               data$angle)
  }

  data <- cartesian2polar(data, coord = coord, region = region,
                          clip = clip, na.rm = na.rm)

  exec(ArcRasterGrob, !!!data, auto_adjust = auto_adjust, ...)
}

#' @export
GeomImages <- ggproto(
  "GeomImages", Geom,
  default_aes = aes(fill = "white", size = 12, angle = 0, hjust = 0.5,
                    vjust = 0.5),
  required_aes = c("x", "y", "image"),
  draw_panel = function (data, panel_params, coord, na.rm = FALSE, mask = NULL,
                         keep_asp = TRUE, read_args = list()) {
    if (empty(data)) {
      return(zeroGrob())
    }

    data <- coord$transform(data, panel_params = panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y, data$x,
                                 data$angle)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x, data$y,
                                 data$angle)
    }
    imageGrob(image = data$image,
              x = data$x,
              y = data$y,
              fill = data$fill,
              size = data$size,
              hjust = data$hjust,
              vjust = data$vjust,
              angle = data$angle,
              keep_asp = keep_asp,
              read_args = read_args,
              default.units = "native",
              mask = mask)
  },

  draw_key = function(data, params, size) {
      imageGrob(image = data$image,
                x = 0.5,
                y = 0.5,
                fill = data$fill,
                size = data$size,
                hjust = 0.5,
                vjust = 0.5,
                angle = data$angle %||% 0,
                default.units = "npc")
    }
)
