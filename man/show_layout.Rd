% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show-cell.R
\name{show_layout}
\alias{show_layout}
\title{Show Layout}
\usage{
show_layout(
  plot,
  fill = "lightblue",
  colour = NA,
  background = "grey95",
  border = NA,
  ...,
  newpage = is.null(vp),
  vp = NULL
)
}
\arguments{
\item{plot}{an ArcPlot object.}

\item{fill}{fill colour of region.}

\item{colour}{border colour of region.}

\item{background, border}{fill and border colour of plot region (unit circle).}

\item{...}{other parameters passing to `ArcBannerTextGrob()`.}

\item{newpage}{logical, indicating whether a new graphics device needs to be opened.}

\item{vp}{grid viewport object.}
}
\value{
invisible grobs.
}
\description{
A quick way to show layout of ArcPlot.
}
\author{
Hou Yun
}
