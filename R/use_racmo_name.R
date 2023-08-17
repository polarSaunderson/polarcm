use_racmo_name <- function(racmoVars,
                           originalUnits = TRUE,
                           monthlyData = FALSE,
                           shortName = TRUE) {
  #' Help with labelling of RACMO variables and units
  #'
  #' @description "senf" isn't clear on a plot, it should be (for example)
  #'   "Sensible Heat Flux (W m-2)". However, that requires a lot of repeated
  #'   typing, particularly when concerned with the superscripts and subscripts
  #'   with `bquote`. This function is (a work-in-progress) way to automate
  #'   this.
  #'
  #' @param racmoVars Which RACMO variable to use? Use the short name code. If a
  #'   vector with multiple racmoVars is provided, a list of expressions is
  #'   returned.
  #' @param originalUnits Should the same units as the dataset be used (TRUE;
  #'   default)? Often this makes the most sense, but some of the variables are
  #'   often changed in the same way for ease of use. For example, it is usually
  #'   easier to think of snow/ice temperatures in ºC rather than Kelvin, to use
  #'   Wm-2 rather than total joules, and to think of melt or precipitation as
  #'   daily totals rather than fluxes per second. Be *VERY VERY VERY* careful
  #'   if setting this as FALSE - it just assumes that the changes are the
  #'   common ones that I have previously done, there is nothing clever about
  #'   it! Set this or 'monthlyData' as NULL to suppress the unit output and
  #'   just return the variable name.
  #' @param monthlyData Is the RACMO data monthly resolution (i.e. racmoM; TRUE)
  #'   or daily (i.e. racmoD; FALSE, the default)? For some variables, this
  #'   makes a difference (e.g. all radiative fluxes are in Joules in monthly
  #'   data, but W m-2 in monthly data); for others (e.g. "t2m", "u10m") it
  #'   makes no difference. Set this or 'originalUnits' as NULL to suppress the
  #'   unit output, and just return the variable name.
  #' @param shortName Should shortened names ("Precip.") or acronyms ("SMB") be
  #'   used over the full name (e.g. "Precipitation", "Surface Mass Balance")?
  #'   If set as NULL, no variable name is used, and only the units are
  #'   returned.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Preallocate & loop to handle if multiple racmoVars
  nameList <- list()
  for (iiVar in racmoVars) {
    # Original Units
    dailyUnits <- switch(iiVar,
                         "precip"   = ,
                         "smb"      = ,
                         "sndiv"    = ,
                         "refreeze" = ,
                         "runoff"   = ,
                         "subl"     = ,
                         "snowmelt" = bquote("( kg "~m^-2~s^-1~")"),
                         "t2m"      = "( K )",
                         "swsn"     = ,
                         "swsd"     = ,
                         "swsu"     = ,
                         "lwsn"     = ,
                         "lwsd"     = ,
                         "lwsu"     = ,
                         "radi"     = ,
                         "turb"     = ,
                         "senf"     = ,
                         "latf"     = ,
                         "seb"      = bquote("( W"~m^-2~")"),
                         "wind"     = ,
                         "w10m"     = ,
                         "v10m"     = ,
                         "u10m"     = bquote("( m"~s^-1~")"),
                         "albedo"   = "")

    monthlyUnits <- switch(iiVar,
                           "precip"   = ,
                           "smb"      = ,
                           "sndiv"    = ,
                           "refreeze" = ,
                           "runoff"   = ,
                           "subl"     = ,
                           "snowmelt" = bquote("( kg "~m^-2~month^-1~")"),
                           "t2m"      = "( K )",
                           "swsn"     = ,
                           "swsd"     = ,
                           "swsu"     = ,
                           "lwsn"     = ,
                           "lwsd"     = ,
                           "lwsu"     = ,
                           "radi"     = ,
                           "turb"     = ,
                           "senf"     = ,
                           "latf"     = ,
                           "seb"      = bquote("( W"~m^-2~")"),
                           "wind"     = ,
                           "w10m"     = ,
                           "v10m"     = ,
                           "u10m"     = bquote("( m"~s^-1~")"),
                           "albedo"   = "")

    # There are some common changes to the original units
    if (isFALSE(originalUnits)) {
      dailyUnits <- switch(iiVar,
                           "precip"   = ,
                           "smb"      = ,
                           "sndiv"    = ,
                           "refreeze" = ,
                           "runoff"   = ,
                           "subl"     = ,
                           "snowmelt" = bquote("( kg "~m^-2~day^-1~")"),
                           "t2m"      = "( ºC )",
                           "albedo"   = "%",
                           dailyUnits)
      monthlyUnits <- switch(iiVar,
                             "swsn"     = ,
                             "swsd"     = ,
                             "swsu"     = ,
                             "lwsn"     = ,
                             "lwsd"     = ,
                             "lwsu"     = ,
                             "radi"     = ,
                             "turb"     = ,
                             "senf"     = ,
                             "latf"     = ,
                             "seb"      = bquote("( W"~m^-2~")"),
                             "t2m"      = "( ºC )",
                             "albedo"   = "%",
                             monthlyUnits)
    }

    # Define unit part
    if (isTRUE(monthlyData)) {
      unitBit <- monthlyUnits
    } else if (isFALSE(monthlyData)) {
      unitBit <- dailyUnits
    } else if (is.null(monthlyData)) {
      unitBit <- ""
    }
    if (is.null(originalUnits)) {
      unitBit <- ""
    }

    # Names
    fullName <- switch(iiVar,
                       "precip"   = "Precipitation",
                       "smb"      = "Surface Mass Balance",
                       "sndiv"    = "Snow Drift",
                       "refreeze" = "Surface Refreezing",
                       "runoff"   = "Surface Runoff",
                       "subl"     = "Sublimation",
                       "snowmelt" = "Melt Flux",
                       "t2m"      = "Surface (2m) Air Temperatures",
                       "albedo"   = "Surface Albedo",
                       "swsn"     = "Net Shortwave Radiation",
                       "swsd"     = "Incoming Shortwave Radiation",
                       "swsu"     = "Outgoing Shortwave Radiation",
                       "lwsn"     = "Net Longwave Radiation",
                       "lwsd"     = "Incoming Longwave Radiation",
                       "lwsu"     = "Outgoing Longwave Radiation",
                       "radi"     = "Net Radiative Fluxes",
                       "turb"     = "Net Turbulent Fluxes",
                       "senf"     = "Sensible Heat",
                       "latf"     = "Latent Heat",
                       "seb"      = "Surface Energy Balance",
                       "wind"     = "Absolute Wind Speed",
                       "w10m"     = "Absolute Wind Speed",
                       "v10m"     = "Southerly Wind Speed",
                       "u10m"     = "Westerly Wind Speed")

    shortened <- switch(iiVar,
                        "precip"   = "Precip.",
                        "smb"      = "SMB",
                        "sndiv"    = "Snow Drift",
                        "refreeze" = "Refreezing",
                        "runoff"   = "Runoff",
                        "subl"     = "Subl.",
                        "snowmelt" = "Melt",
                        "t2m"      = bquote(~T["2m"]),
                        "albedo"   = "Albedo",
                        "swsn"     = bquote(~SW["NET"]),
                        "swsd"     = bquote(~SW["IN"]),
                        "swsu"     = bquote(~SW["OUT"]),
                        "lwsn"     = bquote(~LW["NET"]),
                        "lwsd"     = bquote(~LW["IN"]),
                        "lwsu"     = bquote(~LW["OUT"]),
                        "radi"     = bquote(~Radiative["NET"]),
                        "turb"     = bquote(~Turbulent["NET"]),
                        "senf"     = "Sens. Heat",
                        "latf"     = "Latent Heat",
                        "seb"      = "SEB",
                        "wind"     = "Wind Speed",
                        "w10m"     = "Wind Speed",
                        "v10m"     = "Southerlies",
                        "u10m"     = "Westerlies")

    if (isTRUE(shortName)) {
      nameBit <- shortened
    } else if (isFALSE(shortName)) {
      nameBit <- fullName
    } else if (is.null(shortName)) {
      nameBit <- ""
    }

    # Create final name
    racmoName <- bquote(.(nameBit)~.(unitBit))
    nameList[[iiVar]] <- racmoName
  }

  if (length(nameList) == 1) {
    nameList <- nameList[[iiVar]]
  }

  return(as.expression(nameList))
}
