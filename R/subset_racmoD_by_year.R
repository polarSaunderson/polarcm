##
subset_racmoD_by_year <- function(racmoData,
                                  years,
                                  version = NULL) {
  #' Subset daily RACMO data based on the year
  #'
  #' @description Subset daily RACMO data based on the year. This function is
  #'   essentially a RACMO-specific wrapper around `terrapin::subset_by_year()`.
  #'
  #' @param racmoData The daily RACMO data to subset. Can be either a variable
  #'   name, in which case raw daily RACMO data is read in; or an existing
  #'   SpatRaster of daily RACMO data.
  #' @param years numeric: Which year/s to return?
  #' @inheritParams read_racmoD
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_year(x = racmoData, years = years)
  return(racmoSubset)
}
