% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_fun_in_space.R
\name{calc_fun_in_space}
\alias{calc_fun_in_space}
\title{Apply a function to an RCM variable over a given extent}
\usage{
calc_fun_in_space(
  x,
  extent,
  FUN,
  ...,
  minArea = 0,
  weight = FALSE,
  extentArgs = list()
)
}
\arguments{
\item{x}{SpatRaster: The RCM data to use with 'FUN'. It must be an existing
SpatRaster.}

\item{extent}{Define the spatial extent used to apply 'FUN' over. Fed
directly into \code{\link[=get_extent]{get_extent()}}; see there for details. See also
'extentArgs'.}

\item{FUN}{Which function should be applied? Examples include "mean", "sd",
and "median". This function is applied across all of the pixels in each
single layer.

For example, "mean" gives the average pixel value across a shelf for each
date of 'x'; using "sd" would calculate the standard deviation across
those pixels.}

\item{...}{Any arguments that should be passed to the 'FUN' function.}

\item{minArea}{numeric: Threshold value to determine the minimum fraction
of a pixel that must be covered by the 'extent' to be included in the
calculation. Pixels with a lower fraction are simply excluded from the
calculation (Step 2 in the Description).}

\item{weight}{BINARY: Should each pixels' values be weighted by the pixels'
area in the 'extent'? Example: if a pixel has a value of 4 kg m-2, and is
75 \% covered by the 'extent', should the value be 4 (FALSE) or 3 (TRUE)?}

\item{extentArgs}{list: Any arguments that need to be passed to
\code{\link[=get_extent]{get_extent()}} to define the 'extent'. It is probably easier to define
the 'extent' elsewhere and then just feed it into this function as the
'extent' argument rather than define this list, but it is possible.}
}
\description{
Calculates a value of an RCM variable across a given extent
based on any R function (set as 'FUN'). A single value is returned for
each layer of the input 'x'.

Optionally, pixels can be discarded if the pixel's area within the
'extent' is too small (set with 'minArea'). Pixels can also be weighted
by their area within the 'extent'.

\if{html}{\out{<div class="sourceCode">}}\preformatted{This function simply:
   1) extracts the RCM value at each pixel within the extent
   2) discards any unnecessary pixels (i.e. area in extent < minArea)
   3) optionally, weights the pixel values with the pixel's extent area
   4) calculates the value across the remaining values using 'FUN'
   5) returns a dataframe, with each row containing the FUN value and
      corresponding date for each layer
}\if{html}{\out{</div>}}
}
\examples{
-----------------------------------------------------------------
\dontrun{
  # Example using monthly racmo data
  x <- subset_racmoM_by_summer("precip", 1991:1994) |>
    subset_racmoM_by_month(c(11, 12, 1, 2))

  # shelf extent
  y <- get_shelf_outline("Shackleton")

  # Average value across Shackleton each month
  zMean <- calc_fun_in_space(x = x, extent = y, FUN = "mean")
  zSd   <- calc_fun_in_space(x = x, extent = y, FUN = "sd")
  print(zMean)
  print(zSd)
}

}
