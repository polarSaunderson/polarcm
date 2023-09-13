##
subset_racmoD_by_date <- function(racmoData,
                                  dates = NULL,
                                  periods = NULL,
                                  before = NULL,
                                  after = NULL,
                                  except = NULL,
                                  version = NULL) {
  #' Subset daily RACMO data based on specific dates
  #'
  #' @description Subset daily RACMO data based on specific dates. This function
  #'   is essentially a wrapper around `terrapin::subset_by_dates()` for daily
  #'   RACMO data.
  #'
  #'   **Note:** This function is distinct to [subset_racmoD_by_day()] as it
  #'   accounts for the full date, not just the day.
  #'
  #' @inheritParams subset_racmoD_by_day
  #' @inheritParams terrapin::subset_by_date
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_date(x = racmoData,
                                          dates = dates,
                                          periods = periods,
                                          before = before,
                                          after = after,
                                          except = except)
  return(racmoSubset)
}
