##
subset_racmoD_by_day <- function(racmoData,
                                 days,
                                 version = NULL) {
  #' Subset daily RACMO data based on the day of the month
  #'
  #' @description Subset the daily RACMO data based on the day of the month. It
  #'   only works with the daily part of the date, so there is no accounting for
  #'   months; asking for 31 in February doesn't return any data layer. It will
  #'   return the data for multiple months (i.e. if 10 months are input as
  #'   "racmoData", setting "days = 1" will return the first day of each month).
  #'
  #' @param racmoData The daily RACMO data to subset. Can be either a variable
  #'   name, in which case raw daily RACMO data is read in; or an existing
  #'   SpatRaster of daily RACMO data.
  #' @param days vector: Which day/s to return?
  #' @inheritParams read_racmoD
  #'
  #' @export

  # Code --------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_day(x = racmoData, days = days)
  return(racmoSubset)
}
