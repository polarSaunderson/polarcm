##
subset_racmoM_by_year <- function(racmoData, years) {
  #' Subset monthly RACMO data based on the year
  #'
  #' @description Subset monthly RACMO data using based on year. This function
  #'   is essentially a RACMO-specific wrapper around
  #'   `terrapin::subset_by_year()`.
  #'
  #' @param racmoData: The monthly RACMO data to subset. Can be either a
  #'   variable name, in which case raw monthly RACMO data is read in; or an
  #'   existing SpatRaster of monthly RACMO data.
  #' @param years vector: Which year/s to return?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoM_data(racmoData)
  racmoSubset <- terrapin::subset_by_year(racmoData, years)
  return(racmoSubset)
}
