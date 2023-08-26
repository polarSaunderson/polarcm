read_racmoM_data <- function(variable) {
  #' Read in monthly RACMO data from file
  #'
  #' @description This function allows other functions to easily access monthly
  #'   RACMO data. It will return a SpatRaster, either by reading the necessary
  #'   NetCDF file, or by simply returning the SpatRaster it is given.
  #'
  #'   The RACMO data must already be stored on the machine, and the path to
  #'   defined using [configure_racmoR()]. The SpatRaster's crs and extent
  #'   are explicitly hardcoded as `terra` fails to read them from the file
  #'   correctly. They are based on RACMO2.3p3; adjust if the RACMO version data
  #'   differs.
  #'
  #' @param variable "string": If a valid string is input (i.e. it must match an
  #'   option returned by [list_racmoM_variables()]), the monthly RACMO data for
  #'   that variable is read from the corresponding NetCDF file. If a SpatRaster
  #'   is entered, it is simply returned, allowing this function to be used
  #'   flexibly.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # If not a SpatRaster, read in RACMO data from the NetCDFs
  if ("SpatRaster" %notIn% methods::is(variable)) {
    token   <- configure_racmoR()        # get access to the raw data paths
    racmoM  <- terra::rast(token$varPaths$racmoM[[variable]],
                           subds = variable) |>
      suppressWarnings() # suppress the warning; will add crs and extent below

    # Remove the "dir" layer if present
    if ("dir" %in% names(racmoM)) racmoM <- racmoM[[-which(names(racmoM) == "dir")]]

    # Explicitly add the RACMO crs & extent as they're not always read correctly
    # It uses a proj string for a rotated pole
    terra::crs(racmoM) <- use_crs("racmo")
    terra::ext(racmoM) <- terra::ext(c(-32.875, 32.625, -30.125, 29.875))
  } else {
    racmoM <- variable # Simply return the input
  }
  return(racmoM)
}
