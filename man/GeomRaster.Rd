% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer-raster.R
\name{GeomRaster2grob}
\alias{GeomRaster2grob}
\title{Convert Layer to Grob}
\usage{
GeomRaster2grob(
  data,
  trans = NULL,
  coord = PANEL(),
  region = CELL(),
  ...,
  clip = FALSE,
  na.rm = FALSE
)
}
\arguments{
\item{data}{data frame object, which is extract from a ggplot object.}

\item{trans}{coordinate transform function.}

\item{coord}{coordinate specification, as created by `PANEL()` or extract from
ggplot object.}

\item{region}{a CELL object (created by `CELL()` function) used to set
the drawing area.}

\item{...}{other parameters passing to `Arc*Grob()` function.}

\item{clip}{logical. Allows points to overflow outside the drawing area when
`clip` is FALSE.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}
}
\value{
a grid grob object.
}
\description{
Convert a ggplot layer to arc grob.
}
\seealso{
Other transform: 
\code{\link{GeomBar2grob}()},
\code{\link{GeomBlank2grob}()},
\code{\link{GeomBoxplot2grob}()},
\code{\link{GeomLinerange2grob}()},
\code{\link{GeomMarginVtextArcET2grob}()},
\code{\link{GeomPath2grob}()},
\code{\link{GeomPathArcET2grob}()},
\code{\link{GeomPoint2grob}()},
\code{\link{GeomPolygon2grob}()},
\code{\link{GeomPolygonArcET2grob}()},
\code{\link{GeomRichText2grob}()},
\code{\link{GeomRoundcolArcET2grob}()},
\code{\link{GeomText2grob}()},
\code{\link{GeomViolin2grob}()}
}
\author{
Hou Yun
}
\concept{transform}
