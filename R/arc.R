#' @title Create a new ArcPlot
#' @description `arcplot()` initializes a ArcPlot object, and It works similarly
#' to `ggplot()`.
#' @param data default dataset to use for arcplot. If not specified, must be
#' supplied in each cell plot.
#' @param mapping default list of aesthetic mappings to use for plot. If not
#' specified, must be supplied in each cell plot.
#' @return an ArcPlot object.
#' @rdname arcplot
#' @author Hou Yun
#' @export
arcplot <- function(data = NULL, mapping = aes(), ...) {
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }
  }

  ids <- NULL
  if (!missing(mapping)) {
    if (!inherits(mapping, "uneval")) {
      cli::cli_abort(c("{.arg mapping} should be created with {.fn aes}.",
                       x = "You've supplied a {.cls {class(mapping)[1]}} object"))
    }

    if (!is.null(data)) {
      id_aes <- mapping[intersect(names(mapping), c("TrackID", "SectorID", "CellID"))]
      ids <- lapply_dfc(id_aes, function(expr) {
        as.character(rlang::eval_tidy(expr, data = data))
      })

      if (all(c("TrackID", "SectorID") %in% names(ids)) && !"CellID" %in% names(ids)) {
        ids$CellID <- paste(ids$TrackID, ids$SectorID, sep = "-")
      }

      if (!empty(ids)) {
        ids <- data_frame0(!!!ids, .size = nrow(data))

        if ("CellID" %in% names(ids) && any(c("TrackID", "Sector"))) {
          uniq <- vctrs::vec_unique(ids)

          if (anyDuplicated(uniq$CellID)) {
            cli::cli_abort("Any `CellID` must have a unique `TrackID` and `SectorID`.")
          }
        }
      }
    }
    mapping <- mapping[setdiff(names(mapping), c("TrackID", "SectorID", "CellID"))]
  }

  out <- tibble::tibble(plot = list(),
                        region = list(),
                        TrackID = character(0),
                        SectorID = character(0),
                        CellID = character(0))

  structure(.Data = out,
            data = data,
            mapping = mapping,
            ids = ids,
            class = c("ArcPlot", class(out)))
}

#' @importFrom ggplot2 aes ggplot
#' @export
init_cell <- function(plot,
                      data = NULL,
                      mapping = aes(),
                      region = CELL(),
                      CellID = NULL,
                      TrackID = NULL,
                      SectorID = NULL,
                      inherits_aes = TRUE) {
  if (!is_ArcPlot(plot)) {
    cli::cli_abort(c("`plot` should be created with `arcplot()`.",
                     x = "You've supplied a {.cls {class(plot)[1]}} object."))
  }
  if (!is_CELL(region)) {
    cli::cli_abort(c("`region` should be created with `CELL()`.",
                     x = "You've supplied a {.cls {class(region)[1]}} object."))
  }

  if (!inherits(data, "ggplot")) {
    data <- get_data(plot = plot, CellID = CellID, TrackID = TrackID,
                     SectorID = SectorID)
    if (inherits_aes) {
      mapping <- modify_aes(plot$mapping, mapping)
    }
    cell <- tryCatch(ggplot(data = data, mapping = mapping),
                     error = function(e) {
                       cli::cli_abort("{.arg data} must be a `ggplot` or `dataframe` object.")
                     })
  } else {
    cell <- data
  }

  CellID <- CellID %||% cell_id()
  TrackID <- TrackID %||% track_id()
  SectorID <- SectorID %||% sector_id()

  out <- vec_rbind0(plot, tibble::tibble(plot = list(cell),
                                         region = list(region),
                                         TrackID = as.character(TrackID),
                                         SectorID = as.character(SectorID),
                                         CellID = as.character(CellID)))
  structure(.Data = out,
            data = attr(plot, "data"),
            mapping = attr(plot, "mapping"),
            ids = attr(plot, "ids"),
            class = c("ArcPlot", class(out)))
}

#' @title Get ids
#' @description Helper functions used to generate "CellID", "TrackID" and "SectorID".
#' @param id if "all" will reset all id's index.
#' @return `*_id()` return a character vector.
#' @rdname IDs
#' @author Hou Yun
#' @export
#' @examples
#' cell_id()
#' ## increment 1
#' cell_id()
#'
#' ## reset
#' reset_ids()
#' cell_id()
cell_id <- function() {
  old <- getOption("arcET.cell.id", 0)
  new <- old + 1
  options(arcET.cell.id = new)
  paste0("CellID_", new)
}

#' @rdname IDs
#' @export
track_id <- function() {
  old <- getOption("arcET.track.id", 0)
  new <- old + 1
  options(arcET.track.id = new)
  paste0("TrackID_", new)
}

#' @rdname IDs
#' @export
sector_id <- function() {
  old <- getOption("arcET.sector.id", 0)
  new <- old + 1
  options(arcET.sector.id = new)
  paste0("SectorID_", new)
}

#' @rdname IDs
#' @export
reset_ids <- function(id = c("all", "CellID", "TrackID", "SectorID")) {
  id <- match.arg(id)
  if (id == "all") {
    options(arcET.cell.id = NULL)
    options(arcET.track.id = NULL)
    options(arcET.sector.id = NULL)
  } else {
    if (id == "CellID") {
      options(arcET.cell.id = NULL)
    } else if (id == "TrackID") {
      options(arcET.track.id = NULL)
    } else {
      options(arcET.sector.id = NULL)
    }
  }

  invisible()
}

#' @noRd
get_data <- function(plot,
                     CellID = NULL,
                     TrackID = NULL,
                     SectorID = NULL) {
  data <- attr(plot, "data")
  ids <- attr(plot, "ids")
  if (empty(data) || empty(ids)) {
    return(NULL)
  }

  if (!is.null(TrackID) && !is.null(SectorID)) {
    CellID <- CellID %||% paste(TrackID, SectorID, sep = "-")
  }

  if (!is.null(CellID)) {
    if ("CellID" %in% names(ids)) {
      out <- data[ids$CellID == as.character(CellID), , drop = FALSE]
    } else {
      out <- NULL
    }
  } else {
    if (!is.null(TrackID)) {
      if ("TrackID" %in% names(ids)) {
        out <- data[ids$TrackID == as.character(TrackID), , drop = FALSE]
      } else {
        out <- NULL
      }
    } else {
      if ("SectorID" %in% names(ids)) {
        out <- data[ids$SectorID == as.character(SectorID), , drop = FALSE]
      } else {
        out <- NULL
      }
    }
  }

  if (empty(out)) {
    out <- NULL
  }

  out
}

#' @export
reset_ids <- function(id = c("all", "CellID", "TrackID", "SectorID")) {
  id <- match.arg(id)
  if (id == "all") {
    options(arcET.cell.id = NULL)
    options(arcET.track.id = NULL)
    options(arcET.sector.id = NULL)
  } else {
    if (id == "CellID") {
      options(arcET.cell.id = NULL)
    } else if (id == "TrackID") {
      options(arcET.track.id = NULL)
    } else {
      options(arcET.sector.id = NULL)
    }
  }

  invisible()
}

#' @noRd
is_ArcPlot <- function(x) {
  inherits(x, "ArcPlot")
}
