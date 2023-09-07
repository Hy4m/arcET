#' Modify CellPlot
#' @description Helper functions to modify ArcPlot elements by IDs.
#' @param plot an ArcPlot object.
#' @param ... elements will be added.
#' @param CellID,TrackID,SectorID character IDs indicating which element will be
#' modified.
#' @return an modified ArcPlot object.
#' @rdname within
#' @author Hou Yun
#' @export
within_cell <- function(plot,
                        ...,
                        CellID = NULL) {
  stopifnot(is_ArcPlot(plot))

  if (empty(plot)) {
    return(plot)
  }

  ids <- unique(match(CellID, plot$CellID))

  if (length(ids) < 1) {
    return(plot)
  }

  for (ii in ids) {
    plot$plot[[ii]] <- plot$plot[[ii]] + list2(...)
  }

  plot
}

#' @rdname within
#' @export
within_track <- function(plot,
                         ...,
                         TrackID = NULL) {
  stopifnot(is_ArcPlot(plot))

  if (empty(plot)) {
    return(plot)
  }

  ids <- unique(match(TrackID, plot$TrackID))

  if (length(ids) < 1) {
    return(plot)
  }

  for (ii in ids) {
    plot$plot[[ii]] <- plot$plot[[ii]] + list2(...)
  }

  plot
}

#' @rdname within
#' @export
within_sector <- function(plot,
                          ...,
                          SectorID = NULL) {
  stopifnot(is_ArcPlot(plot))

  if (empty(plot)) {
    return(plot)
  }

  ids <- unique(match(SectorID, plot$SectorID))

  if (length(ids) < 1) {
    return(plot)
  }

  for (ii in ids) {
    plot$plot[[ii]] <- plot$plot[[ii]] + list2(...)
  }

  plot
}

#' @rdname within
#' @export
within_plot <- function(plot, ...) {
  dots <- rlang::dots_list(..., named = TRUE, .homonyms = "last")

  for (nm in names(dots)) {
    attr(plot, ii) <- dots[[nm]]
  }

  plot
}
