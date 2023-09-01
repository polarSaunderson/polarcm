##
subset_racmoM_by_year <- function(racmoData,
                                  years,
                                  version = NULL) {
  #' Subset monthly RACMO data based on the year
  #'
  #' @description Subset monthly RACMO data using based on year. This function
  #'   is essentially a RACMO-specific wrapper around
  #'   `terrapin::subset_by_year()`.
  #'
  #' @inheritParams subset_racmoM_by_month
  #' @param years vector: Which year/s to return?
  #' @inheritParams read_racmoM
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoM(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_year(x = racmoData, years = years)
  return(racmoSubset)
}
