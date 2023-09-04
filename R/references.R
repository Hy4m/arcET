within_references <- function(plot,
                              ...,
                              CellID = NULL,
                              TrackID = NULL,
                              SectorID = NULL) {
  if (empty(plot)) {
    return(plot)
  }

  ids <- unique(match(CellID, plot$CellID), match(TrackID, plot$TrackID),
                match(SectorID, plot$SectorID))

  if (length(ids) < 1) {
    return(plot)
  }

  for (ii in ids) {
    plot$plot[[ii]] <- plot$plot[[ii]] + list2(...)
  }

  plot
}

within_cell <- function(plot,
                        ...,
                        CellID = NULL) {
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

within_track <- function(plot,
                         ...,
                         TrackID = NULL) {
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

within_sector <- function(plot,
                          ...,
                          SectorID = NULL) {
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
