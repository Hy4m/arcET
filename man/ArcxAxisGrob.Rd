% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis.R
\name{ArcxAxisGrob}
\alias{ArcxAxisGrob}
\title{Arc x-axis Grob}
\usage{
ArcxAxisGrob(
  title = NULL,
  coord = PANEL(),
  region = CELL(),
  position = "top",
  ticks.length = 1.5,
  title.gp = element_text(),
  line.gp = element_line(),
  tick.gp = element_line(),
  text.gp = element_text(),
  steps = 0.01,
  ...
)
}
\arguments{
\item{title}{charccter specifying axis title.}

\item{coord}{coordinate specification, as created by `PANEL()` or extract from
ggplot object.}

\item{region}{a CELL object (created by `CELL()` function) used to set
the drawing area.}

\item{position}{axis position specification, and default is "bottom".}

\item{ticks.length}{a grid unit object or numeric, and numerical value
means that the length of ticks is measured in millimeters.}

\item{title.gp}{should be created by `ggplot2::element_text()`.}

\item{line.gp}{should be created by `ggplot2::element_line()`.}

\item{tick.gp}{should be created by `ggplot2::element_line()`.}

\item{text.gp}{should be created by `ggplot2::element_text()`.}

\item{steps}{step length used to split data. A smaller value means a
smoother curve.}

\item{...}{not used.}
}
\value{
a `xAxisGrob` object.
}
\description{
These functions can draw x-axis on polar coordinate.
}
\author{
Hou Yun
}
