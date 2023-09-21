##
subset_racmoM_by_year <- function(racmoData,
                                  years   = NULL,
                                  after   = NULL,
                                  before  = NULL,
                                  except  = NULL,
                                  version = NULL) {
  #' Subset monthly RACMO data based on the year
  #'
  #' @description Subset monthly RACMO data using based on year. This function
  #'   is essentially just a wrapper around [terrapin::subset_by_year()] for
  #'   monthly RACMO data.
  #'
  #' @inheritParams subset_racmoM_by_month
  #' @inheritParams terrapin::subset_by_year
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoM(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_year(x = racmoData,
                                          years = years,
                                          after = after,
                                          before = before,
                                          except = except)
  return(racmoSubset)
}

