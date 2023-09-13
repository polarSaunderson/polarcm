##
subset_racmoM_by_summer <- function(racmoData,
                                    summers = NULL,
                                    australSplit = 3,
                                    after   = NULL,
                                    before  = NULL,
                                    except  = NULL,
                                    version = NULL) {
  #' Subset monthly RACMO data based on the austral summer
  #'
  #' @description Subset monthly RACMO data based on the austral summer. An
  #'   austral summer is defined using the "australSplit" argument; see that for
  #'   an explanation. This function is essentially a wrapper around
  #'   `terrapin::subset_by_summer()` for monthly RACMO data.
  #'
  #' @inheritParams subset_racmoM_by_month
  #' @inheritParams terrapin::subset_by_summer
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData   <- read_racmoM(racmoData = racmoData, version = version)
  racmoSubset <- terrapin::subset_by_summer(x = racmoData,
                                            summers = summers,
                                            after = after,
                                            before = before,
                                            except = except,
                                            australSplit = australSplit)
  return(racmoSubset)
}
