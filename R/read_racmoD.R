read_racmoD <- function(racmoData, version = NULL) {
  #' Read in daily RACMO data from file
  #'
  #' @description Read in the NetCDF files for daily resolution RACMO data
  #'   (racmoM). This function returns a SpatRaster (see the `terra` package),
  #'   but knows how to handle some different configurations of the racmoD
  #'   `polaR` functions need.
  #'
  #' @param racmoData If a valid string is input, the daily RACMO data for
  #'   that variable is returned as a SpatRaster from the corresponding NetCDF
  #'   file/s. Use [list_racmoD_variables()] to see the valid variables. If a
  #'   SpatRaster is entered, it is simply returned as it is.
  #' @param version Which version of the daily RACMO data should be returned?
  #'   The options are those defined by the user in their ".Rprofile" file; see
  #'   [configure_polaR()] for more information. By default, it uses the racmoD
  #'   data that was defined first in the ".Rprofile". Ignored if 'racmoData' is
  #'   already a SpatRaster.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # If already a SpatRaster, it is returned.
  # If not already a SpatRaster, read in the correct daily RACMO NetCDF data
  if ("SpatRaster" %notIn% methods::is(racmoData)) {
    # Prepare
    token    <- configure_polaR()
    version  <- domR::set_if_null(version, token$defaults$racmoD)
    varPaths <- token$varPaths$racmoD[[version]][[racmoData]]

    # Load in the data - racmoD data is split into different files by decade
    racmoD <- terra::rast(varPaths, subds = racmoData)

    # Explicitly add the RACMO crs & extent as they aren't always read correctly
    terra::crs(racmoD) <- use_crs(token$grids$racmoD[[version]]$crs)
    terra::ext(racmoD) <- token$grids$racmoD[[version]]$ext
  } else {
    racmoD <- racmoData
  }
  return(racmoD)
}


#' ##
#' read_racmoD_data <- function(racmoData) {
#'   #' Read in daily RACMO data from file
#'   #'
#'   #' @description This function allows other functions to easily access monthly
#'   #'   RACMO data. It will return a SpatRaster, either by reading the necessary
#'   #'   NetCDF file, or by simply returning the SpatRaster it is given. It is the
#'   #'   daily equivalent of `read_racmoM_data()`, but works a little differently
#'   #'   underneath because it assumes that the daily data has been separated into
#'   #'   decadal NetCDFs.
#'   #'
#'   #' @param racmoData "string": If a valid string is input (i.e. it must match
#'   #'   an option returned by `list_racmoD_variables()`), the daily RACMO data
#'   #'   for that variable is read from the corresponding NetCDF files. If a
#'   #'   SpatRaster is entered, it is simply returned, allowing this function to
#'   #'   be used flexibly.
#'   #'
#'   #' @export
#'
#'   # Code --------------------------------------------------------------------
#'   # If not a SpatRaster, read in RACMO data from the NetCDFs
#'   if ("SpatRaster" %notIn% methods::is(racmoData)) {
#'     # Read in raw nc files (suppress warnings as we add the crs & extent below)
#'     racmo79  <- terra::rast(get_racmoD_file_path(racmoData, 1979),
#'                             subds = racmoData) |> suppressWarnings()
#'     racmo81  <- terra::rast(get_racmoD_file_path(racmoData, 1981),
#'                             subds = racmoData) |> suppressWarnings()
#'     racmo91  <- terra::rast(get_racmoD_file_path(racmoData, 1991),
#'                             subds = racmoData) |> suppressWarnings()
#'     racmo01  <- terra::rast(get_racmoD_file_path(racmoData, 2001),
#'                             subds = racmoData) |> suppressWarnings()
#'     racmo11  <- terra::rast(get_racmoD_file_path(racmoData, 2011),
#'                             subds = racmoData) |> suppressWarnings()
#'
#'     # Unify as a single file
#'     racmoD <- c(racmo79, racmo81, racmo91, racmo01, racmo11)
#'
#'     # Explicitly add the RACMO crs & extent as they're not always read correctly
#'     # It uses a proj string for a rotated pole
#'     terra::crs(racmoD) <- use_crs("racmo")
#'     terra::ext(racmoD) <- terra::ext(c(-32.875, 32.625, -30.125, 29.875))
#'   } else {
#'     racmoD <- racmoData # Simply return the input
#'   }
#'   return(racmoD)
#' }
#'
#'
#'
#'
#' # for use with the new racmoD configuration
#' # # attempt at the full code for read_racmoD_data function; needs testing.
#' # # get_racmoD_file_path is no longer necessary
#' #   if ("SpatRaster" %notIn% methods::is(racmoData)) {
#' #     racmoPaths <- configure_racmoR()$varPaths$racmoD[[version]][[variable]]
#' #     racmoD     <- terra::rast()  # preallocate empty raster
#' #
#' #     for (ii in racmoPaths) {
#' #       iiRast <- terra::rast(ii, subds = variable)
#' #       racmoD <- c(racmoD, iiRast)
#' #    }
#' #
#' #     terra::crs(racmoD) <- use_crs("racmo")
#' #     terra::ext(racmoD) <- terra::ext(c(-32.875, 32.625, -30.125, 29.875))
#' #   } else {
#' #     racmoD <- racmoData
#' #   }
#' #   return(racmoD)
#' # }
#'
#' # # original code from trying the new approach
#' # iiPaths <- token$varPaths$racmoD$rp3[[ii]]
#' #
#' # iiRast  <- terra::rast()
#' #
#' # for (jj in iiPaths) {
#' #   jjRast <- terra::rast(jj, subds = ii)
#' #   iiRast <- c(iiRast, jjRast)
#' # }
#'
#'
#'
#'
#' ##
#' get_racmoD_file_path <- function(variable, year) {
#'   #' Create the file path for the daily racmo data
#'   #'
#'   #' @description The daily RACMO data is split across NetCDF files by decade.
#'   #'   This function simply retrieves the filepaths of the different NetCDFs.
#'   #'   Only used by `read_racmoD_data()`.
#'   #'
#'   #' @param variable "string": Which RACMO variable?
#'   #' @param year numeric: Which year is the data from?
#'   #'
#'   #' @export
#'
#'   # Code -----------------------------------------------------------------------
#'   # Which file is the required year in?
#'   fileYear <- switch((floor((year - 1) / 10) * 10) |> as.character(),
#'                      "1970" = 1979,
#'                      "1980" = 1981,
#'                      "1990" = 1991,
#'                      "2000" = 2001,
#'                      "2010" = 2011)
#'
#'   # Create file path
#'   path     <- configure_racmoR()$dirPaths$racmoD
#'   filePath <- list.files(path,
#'                          paste0(variable, "_daily-", fileYear),
#'                          full.names = TRUE)
#'
#'   return(filePath)
#' }
