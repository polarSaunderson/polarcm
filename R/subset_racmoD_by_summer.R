##
subset_racmoD_by_summer <- function(racmoData,
                                    summers = NULL,
                                    australSplit = 3,
                                    after   = NULL,
                                    before  = NULL,
                                    except  = NULL,
                                    version = NULL) {
  #' Subset daily RACMO data based on the austral summer
  #'
  #' @description Subset dailly RACMO data based on the austral summer. An
  #'   austral summer is defined using the "australSplit" argument; see that for
  #'   an explanation. This function is essentially a wrapper around
  #'   `terrapin::subset_by_summer()` for daily RACMO data.
  #'
  #' @inheritParams subset_racmoD_by_day
  #' @inheritParams terrapin::subset_by_summer
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoD(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_summer(x = racmoData,
                                            summers = summers,
                                            before = before,
                                            after = after,
                                            except = except,
                                            australSplit = australSplit)
  return(racmoSubset)
}
