calc_racmoD_climatology <- function(racmoData,
                                    FUN,
                                    excludeIncomplete = FALSE,
                                    ...) {
  #' Apply a function to daily RACMO data across multiple years
  #'
  #' @description What is the mean air temperature on the 1st December each
  #'   summer? What is the maximum wind speed on the days from the 10th to the
  #'   15th July each year? What is the standard deviation in surface melt on
  #'   the days in the first weeks of January? This function can help answer
  #'   such questions.
  #'
  #' @param racmoData SpatRaster: The RACMO data. It needs to be an existing
  #'   SpatRaster.
  #' @param FUN Which function/s should be applied across the dataset? For
  #'   example, if set as "mean", this function will return the mean value of
  #'   the racmoData variable on the 1st Dec each year. Can be a vector of
  #'   functions, but be wary of speed as there is no elegant solution to
  #'   supplying '...' for different functions; it is probably easier to run
  #'   this function separately for each required 'FUN' function and then
  #'   combine them as a SpatRasterDataset.
  #' @param excludeIncomplete Should data be excluded from the function if the
  #'   day is not found throughout the dataset? TRUE removes dates not found in
  #'   every calendar year (i.e. Jan--Dec); a numeric value between 1 and 12
  #'   does the same but based on an austral year, and this argument defines the
  #'   'australSplit' (e.g. if 3, all months AFTER March, are considered part of
  #'   the following austral year / summer). If FALSE (the default), all data is
  #'   used when calculating the function.
  #'
  #'   Can be useful so the function is applied to the same years/summers for
  #'   each day, but be careful! Worst case scenario: if a dataset ran from 2nd
  #'   January 2000 to 1st January 2001, no data would be used because none of
  #'   the dates occur in both 2000 and 2001. See
  #'   [terrapin::exclude_unmatched_days()]. It is likely much easier and
  #'   clearer to establish which dates are in the dataset first, and then run
  #'   this function with 'excludeIncomplete' set as FALSE.
  #' @param ... Any arguments that need to be fed to the 'FUN' functions. If
  #'   there are multiple values in the 'FUN' vector (i.e. multiple functions
  #'   will be applied), the "..." arguments will be fed to all of the
  #'   functions.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # RACMO Data
  # racmoData <- read_racmoD_data(racmoData = racmoData)
  # is this useful here? This would do a climatology for all raw data, so
  # probably not - feed in an existing racmoD SpatRaster instead

  # Excluding Unmatched Month-Days
  if (isTRUE(excludeIncomplete)) {
    # TRUE to excluding, but FALSE to using an austral year
    australSplit <- FALSE
  } else if (isFALSE(excludeIncomplete)) {
    # FALSE to excluding, so australSplit is NULL
    australSplit <- NULL
  } else if (excludeIncomplete %in% 1:12) {
    # numeric is the month to austral split on
    australSplit <- excludeIncomplete
  } else {
    stop("Set excludeIncomplete as TRUE, FALSE or a value from 1:12")
  }
  racmoData <- terrapin::exclude_unmatched_days(x = racmoData,
                                      australSplit = australSplit)

  # Define dates
  rDates    <- terrapin::get_terra_dates(racmoData, australSplit = australSplit)
  rMonthDay <- rDates$monthDay
  rUnique   <- unique(rMonthDay)

  # Preallocate
  rKlima    <- list(rep(terra::rast(), length(FUN)))
  varName   <- names(racmoData)[[1]] |>
    strsplit("_") |>
    lapply('[', 1) |>
    unlist()

  # Loop through dates
  for (ii in rUnique) {
    iiIndex <- which(rMonthDay == ii)
    iiData  <- terra::subset(racmoData, iiIndex)
    iiDate  <- terra::time(iiData)[[1]]

    # Apply required Functions
    for (jj in seq_along(FUN)) {
      jjValues        <- terra::app(iiData, fun = FUN[jj], ...)
      names(jjValues) <- paste(varName, names(jjValues), ii, sep = "_")
      terra::time(jjValues) <- iiDate

      rKlima[[jj]] <- c(rKlima[[jj]], jjValues)
    }
  }

  # Transform into SpatRasterDataset, rename and return
  rKlima <- terra::sds(rKlima) |> `names<-`(FUN)
  return(rKlima)
}
