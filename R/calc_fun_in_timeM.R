calc_fun_in_timeM <- function(x,
                              months,
                              annual,
                              FUN, ...) {
  #' Calculate RCM values across multiple months each year / austral summer
  #'
  #' @description Often, it is necessary to look at RCM values across multiple
  #'   months each year / austral summer. For example, what is the total JJA
  #'   precipitation each year? Or what is the average wind speed in DJF each
  #'   summer?
  #'
  #'   The `calc_fun_in_timeM` function takes the input SpatRaster, and creates
  #'   a new SpatRaster, which has new layers that are calculated using the
  #'   'months' and 'FUN' (function) arguments. The [calc_fun_australM()] and
  #'   [calc_fun_annualM()] are simple wrappers around [calc_fun_in_timeM()]
  #'   that differ only in how the months are split up, either into calendar
  #'   years (`annualM`) or austral summers / years (`australM`). Only the
  #'   "month" part of the date for each input layer is considered.
  #'
  #'   The following examples further illustrate these 2 functions.
  #'
  #'   ## Example 1 - Annual Winter (JJA) total precipitation
  #'
  #'   Take a RACMO dataset of monthly precipitation, including all months each
  #'   year from 1979--2018 (i.e. 12 months * 40 years = 480 layers). Set
  #'   'months' as c(6, 7, 8) (i.e. JJA), and 'FUN' as "sum". The returned
  #'   SpatRaster will contain 40 layers, with each one being a year's total JJA
  #'   precipitation. The date of each returned layer is the date of the first
  #'   of the constituent layers that it was computed from. Only layers with the
  #'   same units are processed together, and only years when all requested
  #'   months are available are returned.
  #'
  #'   ## Example 2 - Austral Summer (DJF) mean wind speed
  #'
  #'   Take a MAR dataset of monthly-averaged wind speeds, including all months
  #'   each year from 1979--2018 (i.e. 12 months * 40 years = 480 layers). Set
  #'   'months' as c(12, 1, 2) (i.e. DJF), 'FUN' as "mean" and 'annual' as 3.
  #'   The returned SpatRaster will contain 40 layers, with each one being a
  #'   summer's average DJF wind speed. The date of each returned layer is the
  #'   date of the first of constituent layers that it was computed from. Only
  #'   layers with the same units are processed together, and only austral
  #'   summers / years when all requested months are available are returned.
  #'
  #' @param x SpatRaster: The RCM data to use with 'FUN'. It must be an existing
  #'   SpatRaster.
  #' @param months vector: Which month/s to include? Input can be the month
  #'   number (e.g. 12) or the month name, either in full ("December",
  #'   "december") or abbreviated ("Dec", "dec"). Multiple months can be input
  #'   at once (e.g. c(12, 1, 2)), but do not try to mix strings and numbers in
  #'   the vector.
  #' @param annual BINARY: Should the months be split based on calendar years
  #'   (TRUE), or an austral summer? If the latter, provide a numeric value
  #'   between 1 and 12 to indicate which is the last month included in an
  #'   austral summer before the new austral year begins. For example, a value
  #'   of 3 means that all months *AFTER* March are considered as part of the
  #'   following year / summer (i.e. April 1991 -- March 1992 are all in 1992).
  #' @param FUN Which function should be applied? Examples include "mean", "sd",
  #'   and "median". This function is applied to each pixel across the months
  #'   each year. For example, "mean" gives each pixel's average value across
  #'   the months each year; using "sum" would calculate the total across those
  #'   months each year.
  #' @param ... Any arguments that should be passed to the 'FUN' function.

  # Code -----------------------------------------------------------------------
  # Preallocate for multiple SpatRaster layers
  xCubud     <- list()
  incMonths  <- paste0(substring(month.abb[months], 1, 1),
                       collapse = "") # equivalent to domR::get_initials

  # Basic data
  if (isTRUE(annual)) {
    xData   <- terrapin::subset_by_month(x, months,
                                         excludeIncomplete = "years",
                                         dailyResolution = FALSE)
    xDates  <- terrapin::get_date_info(x)
    periods <- unique(xDates$year)
  } else if (annual %in% 1:12) {
    xData   <- terrapin::subset_by_month(x, months,
                                         excludeIncomplete = annual,
                                         dailyResolution = FALSE)
    xDates  <- terrapin::get_date_info(x)
    periods <- unique(xDates$summer)
  }

  # Apply FUN each year / summer
  for (ii in periods) {
    if (isTRUE(annual)) {
      iiData <- terrapin::subset_by_year(xData, years = ii)
    } else {
      iiData <- terrapin::subset_by_summer(xData, summers = ii,
                                           australSplit = annual)
    }
    units  <- terra::units(iiData)

    if (length(unique(units)) == 1) {    # Units must match to combine layers
      # Apply FUN
      iiFun   <- terra::app(x = iiData, fun = FUN, ...)
      funName <- names(iiFun)

      # Add units, names and a date (lost in terra::app)
      terra::time(iiFun) <- terra::time(iiData)[[1]]
      names(iiFun) <- gsub("height=0",                            # replace this
                           paste(paste0(funName, "-", incMonths), # with this
                                 ii, sep = "-"),
                           names(iiData)[[1]])                    # in this
      # Store outside the loop
      xCubud[[which(periods == ii)]] <- iiFun
    }
  }

  # Return as a complete SpatRaster
  xCubud <- terra::rast(unlist(xCubud))
  terra::varnames(xCubud)  <- terra::varnames(xData)
  terra::longnames(xCubud) <- terra::longnames(xData)
  terra::units(xCubud)     <- paste0(units[1],
                                     " (", incMonths, " ", funName, ")")

  return(xCubud)
}


calc_fun_annualM <- function(x,
                             months,
                             FUN,
                             ...) {
  #' Calculate RCM values over multiple months each year
  #'
  #' @inherit calc_fun_in_timeM description
  #' @inheritParams calc_fun_in_timeM
  #'
  #' @seealso calc_fun_australM
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  x <- calc_fun_in_timeM(x, months = months, annual = TRUE,
                        FUN = FUN, ...)
  return(x)
}

calc_fun_australM <- function(x,
                              months,
                              FUN,
                              australSplit = 3,
                              ...) {
  #' Calculate RCM values over multiple months each austral summer
  #'
  #' @inherit calc_fun_in_timeM description
  #' @inheritParams calc_fun_in_timeM
  #' @param australSplit numeric: Which is the last month included in an austral
  #'   summer before the new austral year begins? The default value is 3, which
  #'   means that all months *AFTER* March are considered as part of the
  #'   following summer (i.e. April 1991 -- March 1992 are all in 1992). Swap
  #'   this value according: setting it as 4 means May 1991 -- April 1992 are
  #'   all 1992.
  #'
  #' @seealso calc_fun_annualM
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  x <- calc_fun_in_timeM(x, months = months, annual = australSplit,
                         FUN = FUN, ...)
  return(x)
}
