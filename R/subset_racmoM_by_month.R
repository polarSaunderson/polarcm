##
subset_racmoM_by_month <- function(racmoData,
                                   months,
                                   removeIncomplete = 3) {
  #' Subset monthly RACMO data based on the month
  #'
  #' @description Subset monthly RACMO data based on month. This function is
  #'   essentially a RACMO-specific wrapper around
  #'   `terrapin::subset_by_month()`.
  #'
  #' @param racmoData: The monthly RACMO data to subset. Can be either a
  #'   variable name, in which case raw monthly RACMO data is read in; or an
  #'   existing spatRaster of monthly RACMO data.
  #' @param months vector: Which month/s to return? Input can be the month
  #'   number (e.g. 12) or the month name, either in full ("December",
  #'   "december") or abbreviated ("Dec", "dec"). Multiple months can be input
  #'   at once (e.g. c(12, 1, 2)), but do not try to mix strings and numbers in
  #'   the vector.
  #' @param removeIncomplete If the value is "years", the data is run through
  #'   `terrapin::remove_incomplete_years()`, and only months in years with all
  #'   requested months in are returned. If the value is numeric (between 1 and
  #'   12), the data is fed into the
  #'   `terrapin::remove_incomplete_austral_summers()` and the value is used as
  #'   the "australSplit" argument to return only months in austral summers that
  #'   contain all requested months. If any other value, all layers matching the
  #'   months argument are returned, regardless of the summer or year. See the
  #'   examples.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoM_data(racmoData)
  racmoSubset <- terrapin::subset_by_month(x                = racmoData,
                                           months           = months,
                                           removeIncomplete = removeIncomplete,
                                           dailyResolution  = FALSE)
  return(racmoSubset)
}
