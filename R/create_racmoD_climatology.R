create_racmoD_climatology <- function(racmoData,
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
  #' @param FUN Which function should be applied across the years? For example,
  #'   if set as "mean", this function will return the mean value of the
  #'   racmoData variable on the 1st Dec each year. Can be a vector of
  #'   functions, but be wary of speed and there is no elegant solution to
  #'   supplying ... for different functions; it is probably easier to run this
  #'   separately for each required function and then combine them as a
  #'   SpatRasterDataset.
  #' @param excludeIncomplete Should data be excluded if it is not found in
  #'   throughout the racmoData dataset? TRUE removes dates that not found in
  #'   every calendar year (i.e. Jan--Dec); a numeric value between 1 and 12 is
  #'   used as the australSplit (i.e. if 3, all months AFTER March, are
  #'   considered part of the following summer). If FALSE, all data is used when
  #'   calculating the function.
  #'
  #'   Can be useful so the function is applied to the same years/summers for
  #'   each day, but be careful! Worst case scenario: if there were 5 years of
  #'   complete data (e.g. January 1 2000 - December 31 2005), and a single day
  #'   outside this (e.g. January 1 2006), the function would only be applied to
  #'   the January 1st data as the other dates are not found in 2006. See
  #'   `exclude_unmatched_days`. It is possibly much easier and clearer to
  #'   establish which dates are in the dataset first, and then run this
  #'   function with 'excludeIncomplete' set as FALSE.
  #' @param ... Any arguments that need to be fed to FUN. If there are multiple
  #'   values in a FUN vector, these arguments will be fed to all of the
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
  racmoData <- exclude_unmatched_days(x = racmoData,
                                      australSplit = australSplit)

  # Define dates
  rDates    <- terrapin::get_terra_dates(racmoData, australSplit = australSplit)
  rMonthDay <- rDates$monthDay
  rUnique   <- unique(rMonthDay)

  # Preallocate
  rKlima    <- list(rep(terra::rast(), length(FUN)))
  varName   <- names(x)[[1]] |>
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
