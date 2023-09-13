##
subset_racmoD_by_day <- function(racmoData,
                                 days = NULL,
                                 before = NULL,
                                 after = NULL,
                                 except = NULL,
                                 version = NULL) {
  #' Subset daily RACMO data based on the day of the month
  #'
  #' @description Subset daily RACMO data based on the day of the month. This
  #'   function is essentially a wrapper around `terrapin::subset_by_day()` for
  #'   daily RACMO data.
  #'
  #'   **Note:** This function is distinct to [subset_racmoD_by_date()] as it
  #'   accounts for only the part of the date, not the full date. There is no
  #'   accounting for the month or year.
  #'
  #' @param racmoData The daily RACMO data to subset. Can be either a variable
  #'   name, in which case raw daily RACMO data is read in; or an existing
  #'   SpatRaster of daily RACMO data.
  #'
  #' @inheritParams read_racmoD
  #' @inheritParams terrapin::subset_by_day
  #'
  #' @export

  # Code --------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_day(x = racmoData,
                                         days = days,
                                         before = before,
                                         after = after,
                                         except = except)
  return(racmoSubset)
}
