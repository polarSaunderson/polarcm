##
subset_racmoM_by_month <- function(racmoData,
                                   months = NULL,
                                   excludeIncomplete = 3,
                                   after   = NULL,
                                   before  = NULL,
                                   except  = NULL,
                                   version = NULL) {
  #' Subset monthly RACMO data based on the month
  #'
  #' @description Subset monthly RACMO data based on the month. This function is
  #'   essentially wrapper around `terrapin::subset_by_month()` for monthly
  #'   RACMO data.
  #'
  #' @param racmoData The monthly RACMO data to subset. Can be either a variable
  #'   name, in which case raw monthly RACMO data is read in; or an existing
  #'   SpatRaster of monthly RACMO data.
  #'
  #' @inheritParams read_racmoM
  #' @inheritParams terrapin::subset_by_month
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoM(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_month(x = racmoData,
                                           months = months,
                                           excludeIncomplete = excludeIncomplete,
                                           after = after,
                                           before = before,
                                           except = except,
                                           dailyResolution  = FALSE)
  return(racmoSubset)
}
