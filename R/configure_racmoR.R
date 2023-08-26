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
  #'   any racmoR functions are used (which is automatically done as part of
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
  #'   .racmoR$rawDataPath <- "../../Data/
  #'   .racmoR$racmoM      <- "RACMO/RACMO2.3p3_CON_ANT27_monthly/"
  #'   .racmoR$racmoD      <- "RACMO/RACMO2.3p3_CON_ANT27_daily/"
  #'   .racmoR$MEaSURES    <- "MEaSURES Boundaries/"
  #'   attach(.racmoR)
  #' ```
  #'
  #'   This function assumes that all raw data is stored in a "Data" directory
  #'   (i.e. 'rawDataPath'), with separate subdirectories for RACMO data at
  #'   monthly ('racmoM') and daily ('racmoD') resolution, and for the
  #'   'MEaSURES' data. The necessary data must already be downloaded before
  #'   using `racmoR` - the package *does not* try to find and download data for
  #'   you. If any datasets are not available, set their path as `NULL`. NULLs
  #'   will restrict the capabilities of the racmoR package in expected ways -
  #'   for example, if no daily RACMO data is available, the racmoD functions
  #'   will fail. However, that may not be an issue. The MEaSURES data is used
  #'   for any location-based calculations or subsetting, and for drawing
  #'   borders.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Check if the .Rprofile necessary for configuration has been prepared
  if (!exists(".racmoR")) {
    stop("\nType ?configure_racmoR and read the Instructions section.")
  }

  # Check if the function has already been called.
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
  detach(.racmoR)

  # Create a token to hold all the info
  token <- list()
  token$dirFolders <- data.frame("rawData"  = "Data")      # preallocate
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
    rawDir <- paste0(rawDataPath, MEaSURES)    # path to MEaSURES directory
    token$dirPaths$measures   <- rawDir
    token$dirFolders$measures <- MEaSURES

    token$measures$shelves       <- terra::vect(paste0(rawDir,
                                                       "shelves/IceShelf",
                                                       "_Antarctica_v02.shp"))
    token$measures$basins        <- terra::vect(paste0(rawDir,
                                                       "basinsAA/Basins",
                                                       "_Antarctica_v02.shp"))
    token$imbie$basins           <- terra::vect(paste0(rawDir,
                                                       "basinsIMBIE/Basins_IMBIE",
                                                       "_Antarctica_v02.shp"))
    token$measures$coasts        <- terra::vect(paste0(rawDir,
                                                       "coastline/Coastline",
                                                       "_Antarctica_v02.shp"))
    token$measures$groundingLine <- terra::vect(paste0(rawDir,
                                                  "groundingLine/GroundingLine",
                                                       "_Antarctica_v02.shp"))
  }

  # Define RACMO crs
  token$crs_racmo <- use_crs("racmo")# maybe this should be the other way around

  # Store in the hidden racmoR environment & return
  list2env(x = token, envir = .racmoR)
  attach(.racmoR)
  return(invisible(token))
}
