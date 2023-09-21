##
subset_racmoD_by_month <- function(racmoData,
                                   months = NULL,
                                   excludeIncomplete = 3,
                                   after   = NULL,
                                   before  = NULL,
                                   except  = NULL,
                                   version = NULL) {
  #' Subset daily RACMO data based on the month
  #'
  #' @description Subset daily RACMO data based on the month. This function is
  #'   essentially a wrapper around [terrapin::subset_by_month()] for daily
  #'   RACMO data.
  #'
  #' @param racmoData The daily RACMO data to subset. Can be either a variable
  #'   name, in which case raw daily RACMO data is read in; or an existing
  #'   SpatRaster of daily RACMO data.
  #'
  #' @inheritParams subset_racmoD_by_day
  #' @inheritParams terrapin::subset_by_month
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_month(x = racmoData,
                                           months = months,
                                           after = after,
                                           before = before, except = except,
                                           excludeIncomplete = excludeIncomplete,
                                           dailyResolution = TRUE)
  return(racmoSubset)
}
