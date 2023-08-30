configure_racmoR <- function() {
  #' Define paths to RACMO datasets and variables
  #'
  #' @description
  #'   Please read the Instructions below!
  #'
  #'   `racmoR` functions need to know where certain datasets are on your system
  #'   to work - the `configure_racmoR()` function helps functions access these
  #'   datasets by creating the necessary paths and adding them to a hidden
  #'   ".racmoR" environment. This function therefore needs to be called before
  #'   any `racmoR` functions are used (which is automatically done as part of
  #'   most `racmoR` functions). The datasets must already be saved and
  #'   accessible on your system.
  #'
  #' @details # Instructions
  #'
  #'   This function must be called before using any `racmoR` functions (once
  #'   per R session). The raw data paths need to be defined in an ".Rprofile"
  #'   file. The ".Rprofile" file should be in either: the current working
  #'   directory ([getwd()]), the user's home directory (`Sys.getenv("HOME")`),
  #'   or the R installation ([R.home()]). The ".Rprofile" file is simply a file
  #'   called ".Rprofile" that runs automatically when R starts a new session.
  #'   For more info on ".Rprofile" files, see Section 2.4 of
  #'   [https://csgillespie.github.io/efficientR/set-up.html]().
  #'
  #'   The following code should be pasted into the ".Rprofile" file. This will
  #'   let `racmoR` where to find the RACMO data. Therefore, you must adjust the
  #'   paths to match your directory structure; the paths should be relative to
  #'   your current working directory.
  #'
  #' ```R
  #'   # Prepare data paths for racmoR
  #'   .racmoR = new.env() # hidden racmoR environment
  #'   .racmoR$rawDataPath <- "../../Data/"
  #'   .racmoR$MEaSURES    <- "MEaSURES Boundaries/"
  #'   .racmoR$racmoM      <- "RACMO/RACMO2.3p3_CON_ANT27_monthly/"
  #'   .racmoR$racmoD      <- "RACMO/RACMO2.3p3_CON_ANT27_daily/"
  #'   attach(.racmoR)
  #' ```
  #'
  #'   This function assumes that all raw data is stored in a "Data" directory
  #'   (i.e. 'rawDataPath'), with separate subdirectories for RACMO data at
  #'   monthly ('racmoM') and daily ('racmoD') resolution, and for the
  #'   'MEaSURES' data. The necessary data must already be downloaded before
  #'   trying to use `racmoR` - the package *does not* try to find and download
  #'   data for you. If any datasets are not available, set their path as
  #'   `NULL`. NULLs will restrict the capabilities of the racmoR package in
  #'   expected ways - for example, if no daily RACMO data is available, the
  #'   racmoD functions will fail. However, that may not be an issue. The
  #'   MEaSURES data is used for any location-based calculations or subsetting,
  #'   and for drawing borders.
  #'
  #' # Data Access
  #'   Access monthly RACMO2.3p3 data here (van Dalum et al., 2021):
  #'    [https://doi.org/10.5281/zenodo.5512076.]().
  #'
  #'   Access MEaSURES data here (Mouginot et al., 2017):
  #'    [https://nsidc.org/data/nsidc-0709/versions/2]().
  #'
  #'   Read more about MEaSURES data here:
  #'    [https://nsidc.org/sites/default/files/nsidc-0709-v002-userguide.pdf]().
  #'
  #' # References
  #'
  #' Mouginot, J, B Scheuchl & E Rignot (**2017**) MEaSUREs Antarctic
  #' Boundaries for IPY 2007-2009 from Satellite Radar, Version 2. Boulder,
  #' Colorado USA. NASA National Snow and Ice Data Center Distributed Active
  #' Archive Center. [https://doi.org/10.5067/AXE4121732AD]().
  #' Last access: 08-28-2023.
  #'
  #' van Dalum, CT, WJ van de Berg & MR van den Broeke (**2021**) RACMO2.3p3
  #' monthly SMB, SEB and t2m data for Antarctica (1979-2018). \[CON Data,
  #' Version 2\]. Zenodo. [https://doi.org/10.5281/zenodo.7639053]().
  #' Last access: 08-28-2023.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Check if the .Rprofile necessary for configuration has been prepared
  if (!exists(".racmoR")) {
    stop("\nType ?configure_racmoR and read the Instructions section.")
  }

  # Check if the function has already been called.
  # crs_racmo is created last - if it exists, everything went okay
  if (exists("crs_racmo", envir = .racmoR)) {
    token <- as.list(.racmoR)
    return(invisible(token))
  }

  ### !!! Only run beyond here if the function hasn't been called before !!! ###

  # Access raw data paths
  rawDataPath <- .racmoR$rawDataPath
  racmoM      <- .racmoR$racmoM
  racmoD      <- .racmoR$racmoD
  MEaSURES    <- .racmoR$MEaSURES
  rm(list = c("racmoD", "racmoM", "rawDataPath", "MEaSURES"), envir = .racmoR)

  # Create a token to hold all the info
  token <- list()   # preallocate
  token$dirFolders <- data.frame("rawData"  = basename(rawDataPath))
  token$dirPaths   <- data.frame("rawData"  = rawDataPath)

  # Daily Data
  if (!is.null(racmoD)) {
    # Paths & Folders
    rawDir <- paste0(rawDataPath, racmoD)
    token$dirPaths$racmoD   <- rawDir
    token$dirFolders$racmoD <- racmoD

    # Variables
    token$racmoVars$racmoD <- list.files(rawDir, ".nc") |>  # variable names
      strsplit("_") |>
      lapply('[', 1) |>
      unlist() |>
      unique()
  }

  # Monthly Data
  if (!is.null(racmoM)) {
    # Paths & Folders
    rawDir <- paste0(rawDataPath, racmoM)        # path to racmoM directory
    token$dirPaths$racmoM   <- rawDir
    token$dirFolders$racmoM <- racmoM

    # Variables
    rawPaths  <- list.files(rawDir, ".nc",       # only NetCDFs
                            full.names = TRUE)   # (full paths)
    rawNames  <- list.files(rawDir, ".nc") |>    # variable names
      strsplit("_") |>
      lapply('[', 1) |>
      unlist() |>
      unique()

    # Create data.frame (as it is easy to access using "$")
    token$varPaths$racmoM <- data.frame(matrix(rawPaths, nrow = 1)) |>
      `colnames<-`(rawNames)

    token$racmoVars$racmoM <- rawNames
  }

  # MEaSURES Data
  if (!is.null(MEaSURES)) {
    # MEaSURES paths & directory
    rawDir <- paste0(rawDataPath, MEaSURES)
    token$dirPaths$measures   <- rawDir
    token$dirFolders$measures <- MEaSURES

    # IMBIE Basins (e.g. A-Ap)
    imbie <- paste0(rawDir,
                    "Basins_IMBIE_Antarctica/Basins_IMBIE_Antarctica_v02.shp")
    token$imbie$basins <- terra::vect(imbie)

    # Refined Basins (e.g. Vincennes_Bay)
    basins <- paste0(rawDir,
                     "Basins_Antarctica/Basins_Antarctica_v02.shp")
    token$measures$basins <- terra::vect(basins)

    # Antarctic Coastline
    coast <- paste0(rawDir,
                    "Coastline_Antarctica/Coastline_Antarctica_v02.shp")
    token$measures$coastline <- terra::vect(coast)

    # Antarctic Grounding Line
    GL <- paste0(rawDir,
                 "GroundingLine_Antarctica/GroundingLine_Antarctica_v02.shp")
    token$measures$groundingLine <- terra::vect(GL)

    # Antarctic Ice Shelves
    shelves <- paste0(rawDir,
                      "IceShelf_Antarctica/IceShelf_Antarctica_v02.shp")
    token$measures$shelves <- terra::vect(shelves)
  }

  # Define RACMO crs
  token$crs_racmo <- use_crs("racmo")# maybe this should be the other way around

  # Store in the hidden racmoR environment & return
  detach(.racmoR)                        # remove existing
  list2env(x = token, envir = .racmoR)   # create new
  attach(.racmoR)                        # attach new
  return(invisible(token))               # also return as a list
}



# # NEW THOUGHTS #################################################################
#
# Read from .Rprofile
# ```{r}
# # Check if the .Rprofile necessary for configuration has been prepared
# if (!exists(".racmoR")) {
#   stop("\nType ?configure_racmoR and read the Instructions section.")
# }
#
# # Check if the function has already been called.
# # crs_racmo is created last - if it exists, everything went okay
# if (exists("crs_racmo", envir = .racmoR)) {
#   token <- as.list(.racmoR)
#   return(invisible(token))
# }
#
#
# # Define Defaults
# .racmoR$defaults$marD   <- domR::set_if_null(.racmoR$defaults$marD,
#                                              names(.racmoR$marD)[[1]])
# .racmoR$defaults$marM   <- domR::set_if_null(.racmoR$defaults$marM,
#                                              names(.racmoR$marM)[[1]])
# .racmoR$defaults$racmoM <- domR::set_if_null(.racmoR$defaults$racmoD,
#                                              names(.racmoR$racmoD)[[1]])
# .racmoR$defaults$racmoD <- domR::set_if_null(.racmoR$defaults$racmoD,
#                                              names(.racmoR$racmoD)[[1]])
# ```
#
# # Basics
# ```{r}
# # Parent directory of all data is really important here!
# rawDataPath <- .racmoR$rawDataPath
#
# # Create token to hold all the information
# token <- list()
#
# # Add basic raw data path and file
# token$dirPaths   <- list("rawData" = rawDataPath)
# token$dirFolders <- list("rawData" = basename(rawDataPath))
#
# # Add a list of all data we have     ############ move down when MEaSURES exists
# token$datasets <- list("MEaSURES" = NULL,
#                        "racmoM"   = names(.racmoR$racmoM),
#                        "racmoD"   = names(.racmoR$racmoD),
#                        "marD"     = names(.racmoR$marD),
#                        "marM"     = names(.racmoR$marM))
#
# # Data Paths ===================================================================
#
# ```
#
# # MEaSURES
# ```{r}
# if (!is.null(.racmoR$MEaSURES)) {
#   # Basic path & directory
#   rawDir <- paste0(rawDataPath, .racmoR$MEaSURES)
#   token$dirPaths$MEaSURES   <- rawDir
#   token$dirFolders$MEaSURES <- basename(rawDir)
#
#   # Antarctic Coastline
#   coast <- paste0(rawDir,
#                   "Coastline_Antarctica/Coastline_Antarctica_v02.shp")
#   if (file.exists(coast)) {
#     token$measures$coastline <- terra::vect(coast)
#   } else {warning("Cannot access the coastline in the MEaSURES dataset! ",
#                   "Expected filename:\n  ", coast, "\n\n")}
#
#   # Antarctic Grounding Line
#   GL <- paste0(rawDir,
#                "GroundingLine_Antarctica/GroundingLine_Antarctica_v02.shp")
#   if (file.exists(GL)) {
#     token$measures$groundingLine <- terra::vect(GL)
#   } else {warning("Cannot access the grounding line in the MEaSURES dataset! ",
#                   "Expected filename:\n  ", GL, "\n\n")}
#
#   # Antarctic Ice Shelves
#   shelves <- paste0(rawDir,
#                     "IceShelf_Antarctica/IceShelf_Antarctica_v02.shp")
#   if (file.exists(shelves)) {
#     token$measures$iceShelves <- terra::vect(shelves)
#   } else {warning("Cannot access ice shelves in the MEaSURES dataset! ",
#                   "Expected filename:\n  ", shelves, "\n\n")}
#
#   # IMBIE Basins (e.g. A-Ap)
#   imbie <- paste0(rawDir,
#                   "Basins_IMBIE_Antarctica/Basins_IMBIE_Antarctica_v02.shp")
#   if (file.exists(imbie)) {
#     token$measures$imbieBasins <- terra::vect(imbie)
#   } else {warning("Cannot access IMBIE Basins in the MEaSURES dataset! ",
#                   "Expected filename:\n  ", imbie, "\n\n")}
#
#   # Refined Basins (e.g. Vincennes_Bay)
#   basins <- paste0(rawDir,
#                    "Basins_Antarctica/Basins_Antarctica_v02.shp")
#   if (file.exists(basins)) {
#     token$measures$refinedBasins <- terra::vect(basins)
#   } else {warning("Cannot access Basins in the MEaSURES dataset! ",
#                   "Expected filename:\n  ", basins, "\n\n")}
#
#   # Keep vector of MEaSURES data
#   token$datasets$MEaSURES <- names(token$measures)
# }
# ```
#
# # racmoM
# ```{r}
# if (!is.null(.racmoR$racmoM)) {
#   for (ii in names(.racmoR$racmoM)) {
#     # Basic path & directory
#     iiRawDir <- paste0(rawDataPath, .racmoR$racmoM[[ii]])
#     # token$dirPaths$racmoM[[ii]]   <- iiRawDir             # path to directory
#     # token$dirFolders$racmoM[[ii]] <- basename(iiRawDir)   # directory name
#
#     token$dirPaths[[paste0("racmoM_", ii)]]   <- iiRawDir             # path to directory
#     token$dirFolders[[paste0("racmoM_", ii)]] <- basename(iiRawDir)   # directory name
#
#
#     # Get variables' full paths
#     iiVarPaths <- list.files(iiRawDir, ".nc",           # only NetCDFs
#                              full.names = TRUE)         # get full paths
#
#     # Get variable names
#     iiVarNames <- basename(iiVarPaths) |>
#       strsplit("_") |>
#       lapply('[', 1) |>
#       unlist()
#
#     # Store as a named list for "$" access
#     token$varPaths$racmoM[[ii]] <- setNames(as.list(iiVarPaths), iiVarNames)
#
#     # Keep a vector of variables names
#     token$vars$racmoM[[ii]] <- iiVarNames
#   }
# }
# ```
