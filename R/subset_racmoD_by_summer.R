##
subset_racmoD_by_summer <- function(racmoData,
                                    summers,
                                    australSplit = 3) {
  #' Subset daily RACMO data based on the austral summer
  #'
  #' @description Subset monthly RACMO data based on the austral summer. This
  #'   function is essentially a RACMO-specific wrapper around
  #'   `terrapin::subset_by_summer()`. An austral summer is defined using the
  #'   australSplit argument.
  #'
  #' @param racmoData: The daily RACMO data to subset. Can be either a variable
  #'   name, in which case raw daily RACMO data is read in; or an existing
  #'   SpatRaster of daily RACMO data.
  #' @param summers numeric: Which summer/s to return?
  #' @param australSplit numeric: Which is the last month included in an austral
  #'   summer before the new austral year begins? The default value is 3, which
  #'   means that all months *AFTER* March are considered as part of the
  #'   following summer (i.e. April 1991 -- March 1992 are all in 1992). Swap
  #'   this value according: setting it as 4 means May 1991 -- April 1992 are
  #'   all 1992.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD_data(racmoData)
  racmoSubset <- terrapin::subset_by_summer(x = racmoData, summers = summers,
                                            australSplit = australSplit)
  return(racmoSubset)
}
