average_racmo_in_space <- function(racmoData,
                                   extent,
                                   minArea = 0,
                                   weight = FALSE,
                                   extentArgs = list()) {
  #' Calculate the mean value of a RACMO variable over a given extent
  #'
  #' @description Calculates the average value of a RACMO variable across a
  #'   given extent. A single value is returned for each layer of the input
  #'   'racmoData'. Optionally, pixels can be discarded if the pixel's area
  #'   within the 'extent' is too low (set with 'minArea'). Pixels can also be
  #'   weighted by their area within the 'extent'.
  #'
  #'     The function simply:
  #'        1) extracts the value at each pixel
  #'        2) discards any unnecessary pixels (i.e. area in extent < minArea)
  #'        3) optionally, weights the pixel values with the pixel's extent area
  #'        4) calculates the mean across the remaining values
  #'        5) returns a dataframe containing the average value for each layer,
  #'           and the corresponding date of the layer
  #'
  #' @param racmoData SpatRaster: The RACMO data to average. It must be an
  #'   existing SpatRaster.
  #' @param extent Define the spatial extent used to average over. Fed directly
  #'   into `get_extent()`; see there for details. See also 'extentArgs'.
  #' @param minArea numeric: Threshold value to determine the minimum fraction
  #'   of a pixel that must be covered by the 'extent' to be included in the
  #'   calculation. Pixels with a lower fraction are simply excluded from the
  #'   calculation (Step 2 in the Description).
  #' @param weight BINARY: Should each pixels' values be weighted by the pixels'
  #'   area in the 'extent'? Example: if a pixel has a value of 4 kg m-2, and is
  #'   75 % covered by the 'extent', should the value be 4 (FALSE) or 3 (TRUE)?
  #' @param extentArgs list: Any arguments that need to be passed to
  #'   `get_extent()` to define the 'extent'. It is probably easier to define
  #'   'extent' elsewhere and then just feed it into this function rather
  #'   than define this list.
  #'
  #' @examples -----------------------------------------------------------------
  #'   dontrun{
  #'     # racmoData
  #'     x <- subset_racmoM_by_summer("precip", 1991:1994) |>
  #'       subset_racmoM_by_month(c(11, 12, 1, 2))
  #'
  #'     # shelf extent
  #'     y <- get_shelf_outline("Shackleton")
  #'
  #'     # Average value across Shackleton each month
  #'     z <- average_racmo_in_space(racmoData = x, extent = y, minArea = 0.5)
  #'     print(z)
  #'   }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Extract data
  extentArgs$extent <- extent
  extent <- do.call(get_extent, extentArgs)
  pixelValues <- terra::extract(racmoData, extent, exact = TRUE)
  pixelValues <- pixelValues[pixelValues$fraction > minArea, ]

  # Weight the values?
  if (isTRUE(weight)) {
    pixelValues <- pixelValues * pixelValues$fraction
  }

  # Calculate the mean for each layer (each layer is a column - a separate date)
  racmoMeans <- colMeans(pixelValues)
  racmoMeans <- racmoMeans[-c(1, length(racmoMeans))] # remove fraction & ID

  # Create as a data frame
  racmoVar   <- paste0("mean_", terra::varnames(racmoData)[[1]])
  means      <- terrapin::get_terra_dates(racmoData)   # create df of dates
  means[[racmoVar]] <- racmoMeans                      # add mean values column

  return(means)
}
