% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_antarctica.R
\name{draw_antarctica}
\alias{draw_antarctica}
\title{Draw MEaSURES Antarctic outlines on maps}
\usage{
draw_antarctica(
  extent = "",
  sbcg = "cg",
  rectangularExtent = FALSE,
  simplify = 0,
  crs = NULL,
  newPlot = FALSE,
  extentArgs = list(),
  ...
)
}
\arguments{
\item{extent}{Define the area within which to draw Antarctica. This
argument is fed into \code{\link[=get_extent]{get_extent()}}; see there for details.}

\item{sbcg}{"string": Which parts of Antarctica should be drawn? Options
are:

\if{html}{\out{<div class="sourceCode">}}\preformatted{  * shelves          "s"
  * basins           "b"  # uses IMBIE basins
  * refined basins   "bb" # uses refined MEaSURES basins
  * coasts           "c"
  * grounding line   "g"
}\if{html}{\out{</div>}}}

\item{rectangularExtent}{BINARY: Should only the defined shelf/shelves be
included (TRUE), or can the outlines of all shelves within the bounding
box be included too (FALSE)?}

\item{simplify}{numeric: Should the outline be simplified? Uses the
\code{\link[terra:simplify]{terra::simplifyGeom()}} function, so this value is the tolerance - i.e.
nodes must be at least this far apart, defined in crs units. Larger
numbers are coarser; 0 (default) is no simplifying.}

\item{crs}{"string": Which projection should the outlines be drawn in?
See \code{\link[=use_crs]{use_crs()}} or \code{\link[terra:crs]{terra::crs()}}. By default (i.e. NULL), it
will match the first RCM data defined in the ".Rprofile".}

\item{newPlot}{BINARY: If TRUE, Antarctica is drawn on an empty window; if
FALSE (the default), it is drawn on top of the existing plot. If there is
no existing plot, this defaults to TRUE.}

\item{extentArgs}{An optional list of arguments to feed into
\code{\link[=get_extent]{get_extent()}}.}

\item{...}{Any arguments that can be used in \code{\link[terra:lines]{terra::lines()}}.}
}
\description{
Used after plotting a raster to add MEaSURES outlines. This
function is essentially a wrapper around \code{\link[=get_extent]{get_extent()}} and
\code{\link[terra:lines]{terra::lines()}} that makes it quick and easy to add different outlines
to a map.
}
