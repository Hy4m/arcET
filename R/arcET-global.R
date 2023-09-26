## New environment that holds a set of global variables and settings for arcET
arcET_global <- new.env(parent = emptyenv())

## init index setting
arcET_global$cell_index <- 0
arcET_global$sector_index <- 0
arcET_global$track_index <- 0

## set global `steps` and `simplify`
arcET_global$steps <- 0.005
arcET_global$simplify <- TRUE

#' @title Get ids
#' @description Helper functions used to generate "CellID", "TrackID" and "SectorID".
#' @param which if "all" will reset all id's index.
#' @param env the environment in which IDs is to be generated.
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
cell_id <- function(env = NULL) {
  env <- env %||% arcET_global
  old <- get_arcET_global(which = "cell_index", default = 0, env = env)
  env$cell_index <- old + 1
  paste0("CellID_", old + 1)
}

#' @rdname IDs
#' @export
track_id <- function(env = NULL) {
  env <- env %||% arcET_global
  old <- get_arcET_global(which = "track_index", default = 0, env = env)
  env$track_index <- old + 1
  paste0("TrackID_", old + 1)
}

#' @rdname IDs
#' @export
sector_id <- function(env = NULL) {
  env <- env %||% arcET_global
  old <- get_arcET_global(which = "sector_index", default = 0, env = env)
  env$sector_index <- old + 1
  paste0("SectorID_", old + 1)
}

#' @rdname IDs
#' @export
reset_ids <- function(which = c("all", "CellID", "TrackID", "SectorID"), env = NULL) {
  env <- env %||% arcET_global
  which <- match.arg(which)
  if (which == "all") {
    set_arcET_global(cell_index = 0,
                     track_index = 0,
                     sector_index = 0,
                     env = env)
  } else {
    if (which == "CellID") {
      set_arcET_global(cell_index = 0, env = env)
    } else if (which == "TrackID") {
      set_arcET_global(track_index = 0, env = env)
    } else {
      set_arcET_global(sector_index = 0, env = env)
    }
  }

  invisible()
}

#' @title Get and set global options
#' @description Helper functions used to get and set global options.
#' @param ... any options can be defined, using name = value. Currently,
#' only the `cell_index`,`sector_index`,`track_index`,`steps` and `simplify`
#' options are used
#' @param which a character string indicating which global setting will be returned.
#' @param default if this global setting is not setting, the default will be returned.
#' @param env the environment in which IDs is to be generated. If is NULL (default), will
#' evaluated in `arcET_global` environment.
#' @return for `set_arcET_global()`, will return invisible `NULL`. And for `get_arcET_global()`,
#' will return global setting or default.
#' @rdname set_get_global
#' @author Hou Yun
#' @export
set_arcET_global <- function(..., env = NULL) {
  env <- env %||% arcET_global
  dots <- rlang::dots_list(..., .named = TRUE, .homonyms = "last")

  for (nm in names(dots)) {
    env[[nm]] <- dots[[nm]]
  }

  invisible()
}

#' @rdname set_get_global
#' @export
get_arcET_global <- function(which, default = NULL, env = NULL) {
  env <- env %||% arcET_global
  env[[which]] %||% default
}
