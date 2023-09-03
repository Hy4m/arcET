arcET <- function(data = NULL, mapping = aes(), ...) {

  if (!inherits(mapping, "uneval")) {
    cli::cli_abort(c("`mapping` should be created with `aes()`,",
                     i = "you have supplied a {.cls {class(mapping)}} object."))
  }

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }

    if ("trackID" %in% names(mapping)) {
      data$.trackID. <- rlang::eval_tidy(mapping$trackID, data = data)
    }
    if ("sectorID" %in% names(mapping)) {
      data$.sectorID. <- rlang::eval_tidy(mapping$sectorID, data = data)
    }
  }

  mapping <- mapping[setdiff(names(mapping), c("trackID", "sectorID"))]
  env <- new.env(parent = emptyenv())
  env$data <- data
  env$mapping <- mapping

  structure(.Data = data_frame0(trackID = character(0),
                                sectorID = character(0),
                                cellID = character(0),
                                plot = list()),
            plot_env = env,
            class = c("ArcPlot", class(data_frame0())))

}
