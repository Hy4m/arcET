#' Modify CellPlot
#' @description Helper functions to modify ArcPlot elements by IDs.
#' @param plot an ArcPlot object.
#' @param ... elements will be added.
#' @param CellID,TrackID,SectorID character IDs indicating which element will be
#' modified.
#' @inheritParams base::grepl
#' @return an modified ArcPlot object.
#' @rdname within
#' @author Hou Yun
#' @export
within_cell <- function(plot,
                        ...,
                        CellID = NULL,
                        fixed = TRUE,
                        ignore.case = FALSE) {
  stopifnot(is_ArcPlot(plot))

  ids <- match_ids(CellID, plot$CellID, fixed = fixed, ignore.case = ignore.case)
  if (length(ids) < 1) {
    return(plot)
  }

  for (ii in ids) {
    plot$plot[[ii]] <- plot$plot[[ii]] + list2(...)
  }

  set_current_plot(plot)
  plot
}

#' @rdname within
#' @export
within_track <- function(plot,
                         ...,
                         TrackID = NULL,
                         fixed = TRUE,
                         ignore.case = FALSE) {
  stopifnot(is_ArcPlot(plot))

  ids <- match_ids(TrackID, plot$TrackID, fixed = fixed, ignore.case = ignore.case)
  if (length(ids) < 1) {
    return(plot)
  }

  for (ii in ids) {
    plot$plot[[ii]] <- plot$plot[[ii]] + list2(...)
  }

  set_current_plot(plot)

  plot
}

#' @rdname within
#' @export
within_sector <- function(plot,
                          ...,
                          SectorID = NULL,
                          fixed = TRUE,
                          ignore.case = FALSE) {
  stopifnot(is_ArcPlot(plot))

  ids <- match_ids(SectorID, plot$SectorID, fixed = fixed, ignore.case = ignore.case)
  if (length(ids) < 1) {
    return(plot)
  }

  for (ii in ids) {
    plot$plot[[ii]] <- plot$plot[[ii]] + list2(...)
  }

  set_current_plot(plot)

  plot
}

#' @param title plot title.
#' @param subtitle plot subtitle.
#' @param tag plot tag.
#' @param caption plot caption.
#' @param theme a ggplot theme object.
#' @rdname within
#' @export
within_plot <- function(plot,
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        tag = NULL,
                        theme = NULL,
                        ...) {
  stopifnot(is_ArcPlot(plot))

  if (!is.null(theme)) {
    if (attr(theme, "complete")) {
      plot$theme <- theme
    } else {
      plot$theme <- plot$theme %+replace% theme
    }
  }

  labels <- list(title = title,
                 subtitle = subtitle,
                 caption = caption,
                 tag = tag)
  plot$labels <- utils::modifyList(plot$labels, labels[!vapply_lgl(labels, is.null)])

  dots <- rlang::dots_list(..., named = TRUE, .homonyms = "last")
  for (nm in names(dots)) {
    attr(plot, nm) <- dots[[nm]]
  }

  set_current_plot(plot)
  plot
}
