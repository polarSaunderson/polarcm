define_racmo_globals <- function(rawDataPath = "../../Data/",
                                 racmoM = "RACMO/RACMO2.3p3_CON_ANT27_monthly/",
                                 racmoD = "RACMO/RACMO2.3p3_CON_ANT27_daily/",
                                 MEaSURES = "MEaSURES Boundaries/") {
  #' Define and provide access to global RACMO datasets and variables
  #'
  #' @description racmoR functions need to know where certain datasets are. This
  #'   function helps access these datasets by adding their paths to the global
  #'   environment (as an object called "racmoInfo"). This function should be
  #'   called at the start of a session. The datasets must already be saved and
  #'   accessible. As racmoR expands, this function could also set different
  #'   variables that are necessary between projects.
  #'
  #' @param rawDataPath "string": The relative path to the folder where raw data
  #'   is stored. The approach taken here assumes that all raw data is held in a
  #'   "Data" directory containing subfolders for the "RACMO/racmoM",
  #'   "RACMO/racmoD" and "MEaSURES" datasets. If this is not the case, amend
  #'   this function as necessary.
  #' @param racmoM "string": The name of the subfolder containing the raw
  #'   monthly RACMO NetCDF files. Set as NULL if no daily data is necessary /
  #'   available; if so, "racmoM" functions will not be available.
  #' @param racmoD "string": The name of the subfolder containing the raw daily
  #'   RACMO NetCDF files. Set as NULL if no daily data is necessary /
  #'   available; if so, "racmoD" functions will not be available.
  #' @param MEaSURES "string": The name of the subfolder containing the raw
  #'   MEaSURES data; necessary for cropping for example. Set as NULL if no
  #'   MEaSURES data is necessary / available; if so, any functions with a
  #'   spatial aspect will not be available (e.g. "crop_racmo_to_x",
  #'   "draw_antarctica"). It may be necessary to manually amend the folder and
  #'   file names for these (starting line 79 in the raw code of this function).
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(exists("racmoInfo", envir = .GlobalEnv))) {
    token <- get("racmoInfo", envir = .GlobalEnv)
    return(invisible(token))
  }

  # Only run beyond here if the function hasn't been called yet

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

  # Define RACMO crs
  token$crs_racmo <- use_crs("racmo")# maybe this should be the other way around

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

  # Store in the global environment & return
  racmoInfo <<- token
  return(invisible(token))
}

  # # Monthly Data
  # if (!is.null(racmoM)) {
  #   rawRacmoM      <- paste0(rawDataPath, racmoM)   # path
  #   rawPathsRacmoM <- list.files(rawRacmoM, ".nc",
  #                                full.names = TRUE) # only NetCDFs (full paths)
  #
  #   # Prep for variable names
  #   rawNames <- list.files(rawRacmoM, ".nc")        # file names (for variables)
  #   splitM   <- strsplit(rawNames, "_")
  #
  #   # preallocate
  #   racmoM_df <- matrix(NA, ncol = length(rawNames), nrow = 1) |>
  #     `colnames<-`(seq_along(rawNames))
  #
  #   # Populate
  #   for (ii in seq_along(rawNames)) {
  #     racmoM_df[1, ii] <- rawPathsRacmoM[ii]
  #     colnames(racmoM_df)[ii] <- splitM[[ii]][1]
  #   }
  #   token$rawPath_racmoM <- as.data.frame(racmoM_df) # can subset a df via $name
  # }
