##
read_racmoD_data <- function(racmoData) {
  #' Read in daily RACMO data from file
  #'
  #' @description This function allows other functions to easily access monthly
  #'   RACMO data. It will return a SpatRaster, either by reading the necessary
  #'   NetCDF file, or by simply returning the SpatRaster it is given. It is the
  #'   daily equivalent of `read_racmoM_data()`, but works a little differently
  #'   underneath because it assumes that the daily data has been separated into
  #'   decadal NetCDFs.
  #'
  #' @param racmoData "string": If a valid string is input (i.e. it must match
  #'   an option returned by `list_racmoD_variables()`), the daily RACMO data
  #'   for that variable is read from the corresponding NetCDF files. If a
  #'   SpatRaster is entered, it is simply returned, allowing this function to
  #'   be used flexibly.
  #'
  #' @export

  # Code --------------------------------------------------------------------
  # If not a SpatRaster, read in RACMO data from the NetCDFs
  if ("SpatRaster" %notIn% methods::is(racmoData)) {
    # Read in raw nc files (suppress warnings as we add the crs & extent below)
    racmo79  <- terra::rast(get_racmoD_file_path(racmoData, 1979),
                            subds = racmoData) |> suppressWarnings()
    racmo81  <- terra::rast(get_racmoD_file_path(racmoData, 1981),
                            subds = racmoData) |> suppressWarnings()
    racmo91  <- terra::rast(get_racmoD_file_path(racmoData, 1991),
                            subds = racmoData) |> suppressWarnings()
    racmo01  <- terra::rast(get_racmoD_file_path(racmoData, 2001),
                            subds = racmoData) |> suppressWarnings()
    racmo11  <- terra::rast(get_racmoD_file_path(racmoData, 2011),
                            subds = racmoData) |> suppressWarnings()

    # Unify as a single file
    racmoD <- c(racmo79, racmo81, racmo91, racmo01, racmo11)

    # Explicitly add the RACMO crs & extent as they're not always read correctly
    # It uses a proj string for a rotated pole
    terra::crs(racmoD) <- use_crs("racmo")
    terra::ext(racmoD) <- terra::ext(c(-32.875, 32.625, -30.125, 29.875))
  } else {
    racmoD <- racmoData # Simply return the input
  }
  return(racmoD)
}

##
get_racmoD_file_path <- function(variable, year) {
  #' Create the file path for the daily racmo data
  #'
  #' @description The daily RACMO data is split across NetCDF files by decade.
  #'   This function simply retrieves the filepaths of the different NetCDFs.
  #'   Only used by `read_racmoD_data()`.
  #'
  #' @param variable "string": Which RACMO variable?
  #' @param year numeric: Which year is the data from?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Which file is the required year in?
  fileYear <- switch((floor((year - 1) / 10) * 10) |> as.character(),
                     "1970" = 1979,
                     "1980" = 1981,
                     "1990" = 1991,
                     "2000" = 2001,
                     "2010" = 2011)

  # Create file path
  path     <- define_racmo_globals()$dirPaths$racmoD
  filePath <- list.files(path,
                         paste0(variable, "_daily-", fileYear),
                         full.names = TRUE)

  return(filePath)
}
