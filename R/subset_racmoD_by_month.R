##
subset_racmoD_by_month <- function(racmoData,
                                   months,
                                   excludeIncomplete = FALSE,
                                   version = NULL) {
  #' Subset daily RACMO data based on the month
  #'
  #' @description Subset daily RACMO data based on the month. This function is
  #'   essentially a RACMO-specific wrapper around
  #'   `terrapin::subset_by_month()`.
  #'
  #' @param racmoData The daily RACMO data to subset. Can be either a variable
  #'   name, in which case raw daily RACMO data is read in; or an existing
  #'   SpatRaster of daily RACMO data.
  #' @param months vector: Which month/s to return? Input can be the month
  #'   number (e.g. 12) or the month name, either in full ("December",
  #'   "december") or abbreviated ("Dec", "dec"). Multiple months can be input
  #'   at once (e.g. c(12, 1, 2)), but do not try to mix strings and numbers in
  #'   the vector.
  #' @param excludeIncomplete If the value is "years", the data is run through
  #'   `terrapin::exclude_incomplete_years()`, and only data from months in
  #'   years with all requested months in are returned. If the value is numeric
  #'   (between 1 and 12), the data is fed into the
  #'   `terrapin::exclude_incomplete_summers()` and the value is used as the
  #'   "australSplit" argument to return only months in austral summers that
  #'   contain all requested months. If any other value (including the default,
  #'   FALSE), all layers matching the months argument are returned, regardless
  #'   of the summer or year.
  #' @inheritParams read_racmoD
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_month(x = racmoData,
                                           months = months,
                                           excludeIncomplete = excludeIncomplete,
                                           dailyResolution = TRUE)
  return(racmoSubset)
}
