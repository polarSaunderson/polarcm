##
subset_racmoD_by_year <- function(racmoData,
                                  years = NULL,
                                  before = NULL,
                                  after = NULL,
                                  except = NULL,
                                  version = NULL) {
  #' Subset daily RACMO data based on the year
  #'
  #' @description Subset daily RACMO data based on the year. This function is
  #'   essentially a wrapper around [terrapin::subset_by_year()] for daily RACMO
  #'   data.
  #'
  #' @inheritParams subset_racmoD_by_day
  #' @inheritParams terrapin::subset_by_year
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_year(x = racmoData,
                                          years = years,
                                          before = before,
                                          after = after,
                                          except = except)
  return(racmoSubset)
}
