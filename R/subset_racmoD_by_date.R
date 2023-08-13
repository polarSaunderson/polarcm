##
subset_racmoD_by_date <- function(racmoData,
                                  dates = NULL,
                                  dateList = NULL) {
  #' Subset daily RACMO data based on specific dates
  #'
  #' @description Subset daily RACMO data based on specific dates. This function
  #'   is essentially a RACMO-specific wrapper around
  #'   `terrapin::subset_by_dates()`. The dates can be input either as a vector
  #'   of specific dates using the "dates" argument; or as a list of vectors
  #'   containing the start and end dates for longer extended periods. All dates
  #'   need to be entered in YYYY-MM-DD format (e.g. "2019-12-31").
  #'
  #' @param racmoData The daily RACMO data to subset. Can be either a variable
  #'   name, in which case raw daily RACMO data is read in; or an existing
  #'   SpatRaster of daily RACMO data.
  #' @param dates A vector of specific dates to return. Must be in the format
  #'   "YYYY-MM-DD".
  #' @param dateList A list of vectors, with the first value in each vector
  #'   indicating when the period begins, and the second when the period ends.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD_data(racmoData)
  racmoSubset <- terrapin::subset_by_date(x = racmoData,
                                          dates = dates, dateList = dateList)
  return(racmoSubset)
}
