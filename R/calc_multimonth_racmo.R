calc_multiMonth_racmo <- function(racmoData,
                                  months, annual,
                                  FUN, ...) {
  #' Calculate RACMO values over multiple months each year / austral summer
  #'
  #' @description Often, it is necessary to look at RACMO values across multiple
  #'   months each year / summer. For example, what is the total JJA
  #'   precipitation each year? Or what is the average wind speed in DJF each
  #'   summer? This function takes the input RACMO data, and creates new
  #'   SpatRasters based on the requested 'months' and the 'FUN' (function)
  #'   arguments, accounting for whether the months should be split into
  #'   calendar years or austral summer years.
  #'
  #'   The following examples further illustrates this function.
  #'
  #'   ## Example 1 - Annual Winter (JJA) total precipitation
  #'   Take a monthly RACMO dataset of precipitation, including all months
  #'   each year from 1979--2018 (i.e. 12 months * 40 years = 480 layers). Set
  #'   'months' as c(6, 7, 8) (i.e. JJA), and FUN as "sum". The returned
  #'   SpatRaster will contain 40 layers, with each one being that year's total
  #'   JJA precipitation. The date of each returned layer is the date of the
  #'   first of the constituent layers that it was computed from. Only layers
  #'   with the same units are processed together, and only years when all
  #'   requested months are available are returned.
  #'
  #'   ## Example 2 - Austral Summer (DJF) mean wind speed
  #'   Take a monthly RACMO dataset of average monthly wind speeds, including
  #'   all months each year from 1979--2018 (i.e. 12 months * 40 years = 480
  #'   layers). Set 'months' as c(12, 1, 2) (i.e. DJF), FUN as "mean" and annual
  #'   as 3. The returned SpatRaster will contain 40 layers, with each one being
  #'   that summers's average DJF wind speed. The date of each returned layer is
  #'   the date of the first of constituent layers that it was computed from.
  #'   Only layers with the same units are processed together, and only austral
  #'   summers / years when all requested months are available are returned.
  #'
  #' @param racmoData SpatRaster: The RACMO data to use with 'FUN'. It must be
  #'   an existing SpatRaster.
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
  racmoCubud <- list()
  incMonths  <- paste0(substring(month.abb[months], 1, 1),
                       collapse = "") # equivalent to domR::get_initials

  # Basic data
  if (isTRUE(annual)) {
    racmoData  <- subset_racmoM_by_month(racmoData, months,
                                         excludeIncomplete = "years")
    racmoDates <- terrapin::get_terra_dates(racmoData)
    periods    <- unique(racmoDates$year)
  } else if (annual %in% 1:12) {
    racmoData  <- subset_racmoM_by_month(racmoData, months,
                                         excludeIncomplete = annual)
    racmoDates <- terrapin::get_terra_dates(racmoData)
    periods    <- unique(racmoDates$summer)
  }

  # Apply FUN each year / summer
  for (ii in periods) {
    if (isTRUE(annual)) {
      iiData <- subset_racmoM_by_year(racmoData, ii)
    } else {
      iiData <- subset_racmoM_by_summer(racmoData, ii, annual)
    }
    units  <- terra::units(iiData)

    if (length(unique(units)) == 1) {    # Units must match to combine layers
      # Apply FUN
      iiFun   <- terra::app(x = iiData, fun = FUN, ...)
      funName <- names(iiFun)

      # Add units, names and a date (lost in terra::app)
      terra::time(iiFun) <- terra::time(iiData)[[1]]
      names(iiFun) <- gsub("height=0",
                           paste(paste0(funName, "-", incMonths),
                                 ii, sep = "-"),
                           names(iiData)[[1]])
      # Store outside the loop
      racmoCubud[[which(periods == ii)]] <- iiFun
    }
  }

  # Return as a complete SpatRaster
  racmoCubud <- terra::rast(unlist(racmoCubud))
  terra::varnames(racmoCubud)  <- terra::varnames(racmoData)
  terra::longnames(racmoCubud) <- terra::longnames(racmoData)
  terra::units(racmoCubud)     <- paste0(units[1], " ", incMonths, "_", funName)

  return(racmoCubud)
}


calc_annual_racmo <- function(racmoData,
                              months,
                              FUN,
                              ...) {
  #' Calculate RACMO values over multiple months each year
  #'
  #' @description Often, it is necessary to look at RACMO values across multiple
  #'   months each year. For example, what is the total JJA precipitation each
  #'   year? Or what is the average wind speed in SON? This function takes the
  #'   input RACMO data, and creates new SpatRasters based on the requested
  #'   'months' and the 'FUN' (function) arguments.
  #'
  #'   The following example further illustrates this function. Take a monthly
  #'   RACMO dataset of precipitation, including all months each year from
  #'   1979--2018 (i.e. 12 months * 40 years = 480 layers). Set 'months' as c(6,
  #'   7, 8) (i.e. JJA), and FUN as "sum". The returned SpatRaster will contain
  #'   40 layers, with each one being that year's total JJA precipitation. The
  #'   date of each returned layer is date of the first of the constituent
  #'   layers that it was computed from. Only layers with the same units are
  #'   processed together, and only years when all requested months are
  #'   available are returned.
  #'
  #' @inheritParams calc_multiMonth_racmo
  #'
  #' @seealso calc_austral_racmo
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  x <- calc_multiMonth_racmo(racmoData, months, TRUE, FUN, ...)
  return(x)
}

calc_austral_racmo <- function(racmoData,
                               months,
                               FUN,
                               australSplit = 3,
                               ...) {
  #' Calculate RACMO values over multiple months each austral summer / year
  #'
  #' @description Often, it is necessary to look at RACMO values across multiple
  #'   months, but across austral summers / years. For example, what is the
  #'   total DJF precipitation each austral year / summer? Or what is the
  #'   average wind speed in DJF? This function takes the input RACMO data, and
  #'   creates new SpatRasters based on the requested 'months' and the 'FUN'
  #'   (function) arguments.
  #'
  #'   The following example further illustrates this function. Take a monthly
  #'   RACMO dataset of average monthly wind speeds, including all months each
  #'   year from 1979--2018 (i.e. 12 months * 40 years = 480 layers). Set
  #'   'months' as c(12, 1, 2) (i.e. DJF), and FUN as "mean". The returned
  #'   SpatRaster will contain 40 layers, with each one being that austral
  #'   summers's average DJF wind speed. The date of each returned layer is the
  #'   date of the first of the constituent layers that it was computed from.
  #'   Only layers with the same units are processed together, and only austral
  #'   years when all requested months are available are returned.
  #'
  #' @inheritParams calc_multiMonth_racmo
  #' @param australSplit numeric: Which is the last month included in an austral
  #'   summer before the new austral year begins? The default value is 3, which
  #'   means that all months *AFTER* March are considered as part of the
  #'   following summer (i.e. April 1991 -- March 1992 are all in 1992). Swap
  #'   this value according: setting it as 4 means May 1991 -- April 1992 are
  #'   all 1992.
  #'
  #' @seealso calc_annual_racmo
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  x <- calc_multiMonth_racmo(racmoData, months, australSplit, FUN, ...)
  return(x)
}
