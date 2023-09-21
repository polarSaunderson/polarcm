read_racmoM <- function(racmoData, version = NULL) {
  #' Read in monthly RACMO data from file
  #'
  #' @description Read in the NetCDF files for monthly resolution RACMO data
  #'   (racmoM). This function returns a SpatRaster (see the `terra` package),
  #'   and can handle the necessary configurations for the racmoM data.
  #'
  #' @param racmoData The monthly RACMO data to return. If a valid string, raw
  #'   monthly RACMO data for the corresponding variable is read in from the
  #'   respective NetCDF file. Use [list_racmoM_variables()] to see the valid
  #'   variable names. If an existing SpatRaster, it is simply returned as it
  #'   is.
  #' @param version Which version of the monthly RACMO data should be used? The
  #'   options are those defined by the user in their ".Rprofile" file; see
  #'   [configure_polarcm()] for more information. By default, it uses the
  #'   racmoM data that was defined first in the ".Rprofile". Ignored if
  #'   'racmoData' is already a SpatRaster.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # If already a SpatRaster, it is returned.
  if ("SpatRaster" %in% methods::is(racmoData)) {
    return(racmoData)
  }

  # If not already a SpatRaster, read in the correct monthly RACMO NetCDF data
  # Prepare
  token   <- configure_polarcm()
  version <- set_if_null(version, token$defaults$racmoM)
  varPath <- token$varPaths$racmoM[[version]][[racmoData]]

  # Read in data
  racmoM <- terra::rast(varPath, subds = racmoData) |>
    suppressWarnings() # suppress issues with crs & extent, which we add below

  # Explicitly add the RACMO crs & extent as they aren't always read correctly
  terra::crs(racmoM) <- use_crs(token$grids$racmoM[[version]]$crs)
  terra::ext(racmoM) <- token$grids$racmoM[[version]]$ext

  # Return
  return(racmoM)
}
