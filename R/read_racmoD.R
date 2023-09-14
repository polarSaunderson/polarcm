read_racmoD <- function(racmoData, version = NULL) {
  #' Read in daily RACMO data from file
  #'
  #' @description Read in the NetCDF files for daily resolution RACMO data
  #'   (racmoD). This function returns a SpatRaster (see the `terra` package),
  #'   and knows how to handle some different configurations of the racmoD
  #'   `polaR` functions need.
  #'
  #' @param racmoData The daily RACMO data to return. If a valid string, raw
  #'   daily RACMO data for the corresponding variable is read in from the
  #'   respective NetCDF files. Use [list_racmoD_variables()] to see the valid
  #'   variable names. If an existing SpatRaster, it is simply returned as it
  #'   is.
  #' @param version Which version of the daily RACMO data should be used? The
  #'   options are those defined by the user in their ".Rprofile" file; see
  #'   [configure_polaR()] for more information. By default, it uses the racmoD
  #'   data that was defined first in the ".Rprofile". Ignored if 'racmoData' is
  #'   already a SpatRaster.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # If already a SpatRaster, it is returned.
  if ("SpatRaster" %in% methods::is(racmoData)) {
    return(racmoData)
  }

  # If not already a SpatRaster, read in the correct daily RACMO NetCDF data
  # Prepare
  token    <- configure_polarcm()
  version  <- domR::set_if_null(version, token$defaults$racmoD)
  varPaths <- token$varPaths$racmoD[[version]][[racmoData]]

  # Load in the data - racmoD data is split into different files by decade
  racmoD <- c()
  for (ii in varPaths) {
    iiData <- terra::rast(ii, subds = racmoData) |>
      suppressWarnings() # suppress issues with crs & extent, which we add below
    racmoD <- c(racmoD, iiData)
  }
  racmoD <- terra::rast(racmoD)

  # Explicitly add the RACMO crs & extent as they aren't always read correctly
  terra::crs(racmoD) <- use_crs(token$grids$racmoD[[version]]$crs)
  terra::ext(racmoD) <- token$grids$racmoD[[version]]$ext

  # Return
  return(racmoD)
}
