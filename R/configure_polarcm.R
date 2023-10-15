configure_polarcm <- function(refresh = FALSE) {
  #' Prepare necessary datasets and variables
  #'
  #' @description
  #'   Please **read** this *AND* the Instructions section below!
  #'
  #'   `polarcm` functions need to know where certain datasets are on your
  #'   system to work. The [configure_polarcm()] function helps by creating the
  #'   required paths and datasets in a hidden environment (`.polarEnv`) that
  #'   the other functions can access. The [configure_polarcm()] function
  #'   therefore needs to be called before any `polarcm` functions are used
  #'   (this is automatically done as part of most `polarcm` functions).
  #'
  #'   **IMPORTANT**:
  #'   The necessary datasets *must* already be saved and accessible on your
  #'   system (see "Accessing Datasets" below). Any datasets that are not
  #'   available need to be defined as NULL and will be ignored, with
  #'   predictable consequences - functions that try to call them won't work!
  #'
  #' @param refresh BINARY: Should `polarcm` be configured again? Mainly for
  #'   developers and forces `polarcm` to run the configuration again. It sets
  #'   the existing `.polarEnv` as an empty list, and then sources your
  #'   ".Rprofile" again.
  #'
  #'   By default, 'refresh' is FALSE. It is usually unnecessary to reconfigure
  #'   the package at all in a session. Usually we just want to run it once
  #'   at the start of each R session, and after that first call, we only want
  #'   it to return information that has already stored in the `.polarEnv`.
  #'
  #' @returns Creates a hidden environment `.polarEnv` that stores information
  #'   including the file paths, grid / crs information, and data sources.
  #'
  #' @details # Instructions
  #'
  #'   This function must be called before using any `polarcm` functions. It
  #'   needs to be run once at the start of every new R session. This is usually
  #'   done automatically as part of most `polarcm` functions.
  #'
  #'   The raw data paths need to be defined in an ".Rprofile" file. The
  #'   ".Rprofile" file is simply a file called ".Rprofile" that runs
  #'   automatically when R starts a new session.
  #'
  #'   ```
  #'      The ".Rprofile" file should be located in either:
  #'         1) the current working directory ([getwd()]); or
  #'         2) the user's home directory (`Sys.getenv("HOME")`); or
  #'         3) the R installation ([R.home()]).
  #'   ```
  #'   For more information on ".Rprofile" files, see Section 2.4 of
  #'   [https://csgillespie.github.io/efficientR/set-up.html]().
  #'
  #'   ## Code to Copy
  #'   The following code should be copied and pasted into your ".Rprofile"
  #'   file, with the necessary modifications to the filepaths (explained
  #'   below):
  #'
  #'   ```R
  #'     # polarcm ========================================================================
  #'     if(!exists(".polarEnv")) .polarEnv <- new.env()       # hidden polarcm environment
  #'
  #'     ## Raw Data Path -----------------------------------------------------------------
  #'     .polarEnv$rawDataPath     <- "../../Data/"            # relative to working dir
  #'
  #'     ## MEaSURES Data Path ------------------------------------------------------------
  #'     .polarEnv$MEaSURES        <- "MEaSURES Boundaries/"   # relative to rawDataPath
  #'
  #'     ## Monthly RACMO Data Paths ------------------------------------------------------
  #'     .polarEnv$rcm$racmoM$rp3  <- list("dir" = RACMO/RACMO2.3p3_CON_ANT27_monthly/",
  #'                                       "src" = "10.5281/zenodo.5512076")
  #'
  #'     ## Daily RACMO Data Paths --------------------------------------------------------
  #'     .polarEnv$rcm$racmoD      <- NULL      # no racmoD data
  #'
  #'     ## Other RCM Datasets ------------------------------------------------------------
  #'     # Coming soon!
  #'
  #'   ```
  #'
  #'   Once in your ".Rprofile", the above code block lets `polarcm` know where
  #'   to find the datasets on your system, and also to identify which dataset
  #'   it is (via its doi). You must adjust the paths to match your directory
  #'   structure as follows:
  #'
  #'    - `.polarEnv$rawDataPath` must be relative to your current working
  #'    directory;
  #'    - all other paths must be relative to the "rawDataPath" directory;
  #'    - if the data is not available (or you don't want it accessible), the
  #'    path must be set as NULL (see racmoD example).
  #'
  #'   See "Defining Datasets" below for more information on modifying the
  #'   above code block.
  #'
  #'   # Accessing Datasets
  #'   `polarcm` **DOES NOT** download data for you. Follow the doi links on
  #'   this page to find data on Zenodo and download the data you want to use
  #'   first.
  #'
  #'   `polarcm` has been created to handle some commonly-used,
  #'   openly-accessible RACMO and MAR datasets shared on Zenodo (see "Zenodo
  #'   RCM Datasets"). Some functions will also work with other versions of the
  #'   data, or perhaps even output from other RCMs, if you **really** want to
  #'   try it. Be particularly wary of data projections in such cases.
  #'
  #'   However, there is no standard way to share MAR or RACMO output (or any
  #'   other RCM output). The lack of standardisation means that a lot of the
  #'   ease of using `polarcm` comes from background wrangling of the datasets
  #'   to get the data into the structure that `polarcm` expects. For example,
  #'   some authors distribute the data with each file storing a different
  #'   variable, whilst others store all variables in a single file, but create
  #'   separate files for each year or month. `polarcm` works best when data is
  #'   organised by variable.
  #'
  #'   As noted, `polarcm` handles these differences as well as it can do in the
  #'   background, but the package has only been set up for certain datasets
  #'   *so far*. Future versions of the package will continue to build on these
  #'   if there is demand.
  #'
  #'   The following subsections list the datasets that `polarcm` knows what to
  #'   do with. These datasets are all openly available on Zenodo, and need to
  #'   be  identified in the ".Rprofile" file by their doi (see "Defining
  #'   Datasets").
  #'
  #'   ## MEaSURES Dataset
  #'   `polarcm` bases the spatial aspects (e.g. using shelf outlines or
  #'   grounding line) on MEaSURES data, Version 2 (Mouginot et al., 2017).
  #'   This dataset needs to be downloaded first, from:
  #'   [https://doi.org/10.5067/AXE4121732AD]()
  #'
  #'   ## Zenodo RCM Datasets
  #'   ### RACMO Data
  #'   - `[r01_dt]`  RACMO2.3p3 Monthly Data Antarctica   [https://doi.org/10.5281/zenodo.5512076]()
  #'   - `[r02_dt]`  RACMO2.3p2 Monthly Data Antarctica   IN PROGRESS!
  #'
  #'   ### MAR Data
  #'   - Coming soon!
  #'
  #'   *Note:* The Zenodo doi's listed here resolve to the latest version of the
  #'   dataset on Zenodo. It is assumed that any versions of the datasets shared
  #'   on the same Zenodo will be set up in the same way. Let me know if this is
  #'   not the case.
  #'
  #'   *Note:* If multiple datasets are included, the first one (i.e. the
  #'   closest to the top of the ".Rprofile") will be used as the default in
  #'   functions, particularly important for those concerned with data
  #'   projections.
  #'
  #'   For example, in [draw_antarctica()], the default projection will match
  #'   the projection of the first dataset included in the ".Rprofile". The crs
  #'   projection parameters can be overwritten when necessary.
  #'
  #'   *Note:* If multiple datasets are added to the same section (e.g. there is
  #'   both RACMO2.3p2 and RACMO2.3p3 monthly data), the first one defined will
  #'   be considered as the default in functions. This can be overridden in the
  #'   functions that care (mainly the `read_x()` functions) using the 'version'
  #'   argument set to match the corresponding `<name>` (see "Defining Datasets"
  #'   below).
  #'
  #'   *Note:* It is okay to choose your own name for the directory containing
  #'   these datasets (as long as they are defined in ".Rprofile"), but the file
  #'   names for the NetCDFs and the MEaSURES dataset should be the same as
  #'   those that were downloaded.
  #'
  #'   *Note:* Ideally, there will become a standardised way of distributing
  #'   such polar RCM data in future, but if you are aware of any other openly
  #'   accessible datasets that you think should be added to `polarcm`, let me
  #'   know and I'll see whether I can add them in a future update.
  #'
  #'   # Defining Datasets
  #'   To make different datasets available, it is necessary to configure your
  #'   ".Rprofile" file by including the following information for **each**
  #'   dataset type:
  #'
  #'      .polarEnv$rcm$<type>$<name> <- list("dir" = <dataDirectory>,
  #'                                          "src" = <doiCode>)
  #'
  #'   `<type>`
  #'   Some functions need to know whether this is RACMO or MAR data,
  #'   and whether it is monthly, daily, or hourly data. Currently, the options
  #'   for MAR data are under development, so only racmoM and racmoD are usable:
  #'
  #'       racmoM    Monthly RACMO
  #'       racmoD    Daily RACMO
  #'
  #'   All of these options **must** be included in the ".Rprofile" even if the
  #'   dataset is not available or not wanted. In which case, just set the value
  #'   as NULL (e.g. `.polarEnv$rcm$marM <- NULL`).
  #'
  #'   `<name>`
  #'   How do you want to refer to this dataset in your code? For
  #'   example, when reading in data, which is more your style?:
  #'   ```R
  #'      read_racmoM(var = "snowmelt", ver = "rp2")
  #'      read_racmoM(var = "snowmelt", ver = "RACMO2.3p2")
  #'      read_racmoM(var = "snowmelt", ver = "_anyOtherName.You.WANT_2uSE")
  #'   ```
  #'
  #'   `<dataDirectory>`
  #'   Where are the NetCDFs stored on your system? Must be a
  #'   relative path compared to the "Data/" directory as defined in the
  #'   ".Rprofile" file as ".polarEnv$rawDataPath".
  #'
  #'   `<doiCode>`
  #'   Which dataset is this? As explained above, there is no
  #'   standard way to distribute these datasets so we need to explicitly tell
  #'   `polarcm` what dataset we are giving it. We do this using the doi of the
  #'   download page. For example, for the monthly RACMO2.3p3 data above
  #'   (`r01_dt`, with doi "https://doi.org/10.5281/zenodo.5512076"), the
  #'   `<doiCode>` must be "10.5281/zenodo.5512076".
  #'
  #'   If the data has not been shared publicly with a doi, but you know FOR
  #'   CERTAIN that it is in EXACTLY the same format (e.g. organisation of data
  #'   files, crs, extent), as one that has been configured, you should be able
  #'   to just add the `<doiCode>` of the corresponding dataset. Be cautious.
  #'
  #'   ## Directory Structure
  #'   This function assumes that all raw data is stored in a "Data/" directory
  #'   (defined in ".Rprofile" using `.polarEnv$rawDataPath`). Separate
  #'   subdirectories are then expected for the MEaSURES dataset and any RCM
  #'   datasets.
  #'
  #'   These subdirectories can either be separate directories for each dataset
  #'   (e.g. the RACMO Monthly examples below) or further nested (e.g. the MAR
  #'   Monthly examples below). Whichever way, make sure that the path in the
  #'   "dir" part of the ".Rprofile" points to the directory containing the
  #'   datasets.
  #'
  #'   ```
  #'     Data/
  #'     | - MEaSURES Boundaries/
  #'     |   | - Basins_Antarctica/
  #'     |       | - Basins_Antarctica_v02.shp
  #'     |
  #'     | - RACMO2.3p3_Monthly_ANT27_CON/
  #'     |   | - snowmelt_monthlyS_ANT27_CONSettings_197901_201812.nc
  #'     |   | - swsd...file.nc
  #'     |
  #'     | - RACMO2.3p2 Monthly/
  #'     |   | - snowmelt...file.nc
  #'     |   | - swsd...file.nc
  #'     |
  #'     | - MAR Monthly/
  #'     |   | - MARv3.11/
  #'     |   |   | - 2015_file.nc
  #'     |   |   | - 2016_file.nc
  #'     |   | - MARv3.12/
  #'     |       | - 2011_file.nc
  #'     |
  #'     | - MAR Daily/
  #'     |   | - 1980_file.nc
  #'     |   | - 1981_file.nc
  #'  ```
  #'
  #'   *!!!*  *REMEMBER*  *!!!*
  #'
  #'   The data **must** have already been downloaded before trying to use
  #'   `polarcm`!
  #'
  #'   The package *does not* try to find and/or download data for
  #'   you.
  #'
  #'   If any datasets are not available, set their path as `NULL`. NULLs
  #'   will restrict the capabilities of the racmoR package in expected ways -
  #'   for example, if no daily RACMO data is available, the racmoD functions
  #'   will fail. However, that may not be an issue.
  #'
  #'   The MEaSURES data is necessary for any location-based calculations or
  #'   subsetting, and for drawing borders.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Check if the .Rprofile has been prepared configuration
  if (!exists(".polarEnv")) {
    stop("\nType ?configure_polarcm and read the Instructions section.")
  } else if (isTRUE(refresh)) {
    # print("reconfiguring")
    list2env(x = list(), envir = .polarEnv)
    source(".Rprofile")            # print(".Rprofile sourced, .polarEnv should be refreshed!")
    .polarEnv$configured <- FALSE  # print("added nonconfigured to .polarEnv")
  } #else { print("just proceeding...") }

  # Do we need to configure everything?
  if (isTRUE(.polarEnv$configured)) {
    # message("polarcm is already configured!")
    token <- as.list(.polarEnv)
    return(invisible(token))
  } else {
    message(">>> Configuring polarcm (v", utils::packageVersion("polarcm"), ") ... \n")
  }

  # Basic Set-Up ---------------------------------------------------------------
  # Create a token list to hold all the information
  token <- list()

  # Add basic raw data path & folder
  rawDataPath      <- .polarEnv$rawDataPath
  token$dirPaths   <- list("rawData" = rawDataPath)
  token$dirFolders <- list("rawData" = basename(rawDataPath))

  # CRS Definitions ------------------------------------------------------------
  token$grids$ext$racmoExt   <- terra::ext(c(-32.875, 32.625, -30.125, 29.875))
  token$grids$crs$racmoCrs   <- paste("+proj=ob_tran",
                                      "+o_proj=longlat",
                                      "+o_lat_p=-180.0 +lon_0=10.0",
                                      "-m 57.295779506")

  token$grids$crs$lambertCrs <- paste("+proj=laea",
                                      "+lat_0=-90 +lon_0=0",
                                      "+x_0=0 +y_0=0",
                                      "+datum=WGS84 +units=m +no_defs +type=crs")

  token$grids$crs$orthoCrs   <- paste("+proj=ortho",
                                      "+f=0 +lat_0=-90 +lon_0=0",
                                      "+x_0=0 +y_0=0",
                                      "+datum=WGS84 +units=m +no_defs +type=crs")

  message("     The crs definitions have been successfully configured.")

  # MEaSURES Datasets ----------------------------------------------------------
  if (!is.null(.polarEnv$MEaSURES)) {
    # Basic path & directory
    rawDir <- paste0(rawDataPath, .polarEnv$MEaSURES)
    token$dirPaths$MEaSURES   <- rawDir
    token$dirFolders$MEaSURES <- basename(rawDir)
    token$datasets$MEaSURES$doi  <- "doi.org/10.5067/AXE4121732AD"
    token$datasets$MEaSURES$name <- paste("MEaSUREs Antarctic Boundaries for",
                                          "IPY 2007-2009 from Satellite Radar,",
                                          "Version 2")
    token$datasets$MEaSURES$authors <- c("J. Mouginot",
                                         "B. Scheuchl",
                                         "E. Rignot")

    # Antarctic Coastline
    coast <- paste0(rawDir,
                    "Coastline_Antarctica/Coastline_Antarctica_v02.shp")
    if (file.exists(coast)) {
      coast <- terra::vect(coast)
      token$measures$coastline <- coast
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "coastline")
      token$grids$measures$coastline$crs <- "EPSG:3031"
      token$grids$measures$coastline$ext <- terra::ext(coast)
      message("     The MEaSURES coastline dataset has been successfully configured.")
    } else {
      message(" !!  Cannot access the coastline in the MEaSURES dataset!")
      warning(" !!  Cannot access the coastline in the MEaSURES dataset!",
                    "\n     - Looking for filename: ", coast, "\n\n")
    }

    # Antarctic Grounding Line
    GL <- paste0(rawDir,
                 "GroundingLine_Antarctica/GroundingLine_Antarctica_v02.shp")
    if (file.exists(GL)) {
      GL <- terra::vect(GL)
      token$measures$groundingLine <- GL
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "groundingLine")
      token$grids$measures$groundingLine$crs <- "EPSG:3031"
      token$grids$measures$groundingLine$ext <- terra::ext(GL)
      message("     The MEaSURES grounding line dataset has been successfully configured.")
    } else {
      message(" !!  Cannot access the grounding line in the MEaSURES dataset!")
      warning(" !!  Cannot access the grounding line in the MEaSURES dataset!",
            "\n     - Looking for filename: ", GL, "\n\n")
    }

    # Antarctic Ice Shelves
    shelves <- paste0(rawDir,
                      "IceShelf_Antarctica/IceShelf_Antarctica_v02.shp")
    if (file.exists(shelves)) {
      shelves <- terra::vect(shelves)
      token$measures$iceShelves <- shelves
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "iceShelves")
      token$grids$measures$iceShelves$crs <- "EPSG:3031"
      token$grids$measures$iceShelves$crs <- terra::ext(shelves)
      message("     The MEaSURES ice shelves dataset has been successfully configured.")
    } else {
      message(" !!  Cannot access the ice shelves in the MEaSURES dataset!")
      warning(" !!  Cannot access the ice shelves in the MEaSURES dataset!",
              "\n     - Looking for filename: ", shelves, "\n\n")
    }

    # IMBIE Basins (e.g. A-Ap)
    imbie <- paste0(rawDir,
                    "Basins_IMBIE_Antarctica/Basins_IMBIE_Antarctica_v02.shp")
    if (file.exists(imbie)) {
      imbie <- terra::vect(imbie)
      token$measures$imbieBasins <- imbie
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "imbieBasins")
      token$grids$measures$imbieBasins$crs <- "EPSG:3031"
      token$grids$measures$imbieBasins$ext <- terra::ext(imbie)
      message("     The MEaSURES IMBIE basins dataset has been successfully configured.")
    } else {
      message(" !!  Cannot access the IMBIE Basins in the MEaSURES dataset!")
      warning(" !!  Cannot access the IMBIE Basins in the MEaSURES dataset!",
              "\n     - Looking for filename: ", imbie, "\n\n")
    }

    # Refined Basins (e.g. Vincennes_Bay)
    basins <- paste0(rawDir,
                     "Basins_Antarctica/Basins_Antarctica_v02.shp")
    if (file.exists(basins)) {
      basins <- terra::vect(basins)
      token$measures$refinedBasins <- basins
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "refinedBasins")
      token$grids$measures$refinedBasins$crs <- "EPSG:3031"
      token$grids$measures$refinedBasins$ext <- terra::ext(basins)
      message("     The MEaSURES refined basins dataset has been successfully configured.")
    } else {
      message(" !!  Cannot access the refined basins in the MEaSURES dataset!")
      warning(" !!  Cannot access the refined basins in the MEaSURES dataset!",
              "\n     - Looking for filename: ", basins, "\n\n")
    }
  }

  # racmoM Datasets ------------------------------------------------------------
  if (!is.null(.polarEnv$rcm$racmoM)) {
    for (ii in names(.polarEnv$rcm$racmoM)) {
      # Which data is this?
      iiRawDir <- paste0(rawDataPath, .polarEnv$rcm$racmoM[[ii]]$dir)
      iiSrc    <- .polarEnv$rcm$racmoM[[ii]]$src

      # Store path & folder
      token$dirPaths$racmoM[[ii]]   <- iiRawDir
      token$dirFolders$racmoM[[ii]] <- basename(iiRawDir)

      # Handling racmoM depends on the dataset version
      if (iiSrc %in% c("10.5281/zenodo.5512076")) {
        # Get paths for this dataset version
        iiVarPaths <- list.files(iiRawDir, pattern = ".nc",  # only NetCDF files
                                 full.names = TRUE)          # get full paths

        # Get variable names
        iiVarNames <- basename(iiVarPaths) |>
          strsplit("_") |>
          lapply('[', 1) |>
          unlist()

        # Store variables & paths as named lists for "$" access
        token$varPaths$racmoM[[ii]] <- stats::setNames(as.list(iiVarPaths),
                                                       iiVarNames)
        token$varNames$racmoM[[ii]] <- iiVarNames

        # Keep a record of which dataset this is, regardless of the user's name
        token$datasets$racmoM[[ii]]$doi       <- paste0("doi.org/", iiSrc)
        if (iiSrc == "10.5281/zenodo.5512076") {
          token$datasets$racmoM[[ii]]$name    <-
               "RACMO2.3p3 Monthly SMB, SEB and t2m data for Antarctica (1979-2018)"
          token$datasets$racmoM[[ii]]$authors <- c("C. van Dalum",
                                                   "W.J. van de Berg",
                                                   "M. van den Broeke")
        }

        # Assign grid information
        token$grids$racmoM[[ii]]$crs <- token$grids$crs$racmoCrs
        token$grids$racmoM[[ii]]$ext <- token$grids$ext$racmoExt

        if (length(iiVarNames) == 0) {
          message(" !!  No data was found for racmoM dataset ", ii, "!")
          warning(" !!  No data was found for racmoM dataset ", ii, "!")
        } else {
          message("     The racmoM dataset ", ii,
                  " has been successfully configured. (",
                  length(iiVarNames), " variables available)")
        }
      } else {
        message(" !!  src for racmoM", ii, "dataset not recognised !")
        warning(" !!  We don't recognise the src of the racmoM '", ii, "' dataset.",
                "\n    Type ?configure_polarcm and read the instructions.\n")
      }
    }
  }

  # racmoD Datasets ------------------------------------------------------------
  if (!is.null(.polarEnv$rcm$racmoD)) {
    for (ii in names(.polarEnv$rcm$racmoD)) {
      # Which data is this?
      iiRawDir <- paste0(rawDataPath, .polarEnv$rcm$racmoD[[ii]]$dir)
      iiSrc    <- .polarEnv$rcm$racmoD[[ii]]$src

      # Store path & folder
      token$dirPaths$racmoD[[ii]]   <- iiRawDir
      token$dirFolders$racmoD[[ii]] <- basename(iiRawDir)

      # Handling racmoD data depends on the dataset version
      if (iiSrc %in% c("10.5281/zenodo.5512076")) {
        # Get paths for this dataset version
        iiVarPaths <- list.files(iiRawDir, ".nc")

        # Get variable names
        iiVarNames <- strsplit(iiVarPaths, "_") |>
          lapply('[', 1) |>
          unlist() |>
          unique()

        # Store variable names in a named list for "$" access
        token$varNames$racmoD[[ii]] <- iiVarNames

        # Store variable paths in a named list for "$" access
        for (jj in iiVarNames) {
          # Which files contain this variable?
          jjFiles <- list.files(iiRawDir, pattern = paste0(jj, ".+nc"),
                                full.names = TRUE)

          # Store
          token$varPaths$racmoD[[ii]][[jj]] <- jjFiles
        }

        # Keep a record of which dataset this is, regardless of the user's name
        token$datasets$racmoD[[ii]]$doi  <- paste0("doi.org/", iiSrc)
        token$datasets$racmoD[[ii]]$name <- paste("Daily version of RACMO2.3p3",
                                                  "SMB, SEB and t2m data for",
                                                  "Antarctica (1979-2018)")
        token$datasets$racmoM[[ii]]$authors <- c("C. van Dalum",
                                                 "W.J. van de Berg",
                                                 "M. van den Broeke")

        # Assign grid information
        token$grids$racmoD[[ii]]$crs <- token$grids$crs$racmoCrs
        token$grids$racmoD[[ii]]$ext <- token$grids$ext$racmoExt

        if (length(iiVarNames) == 0) {
          message(" !!  No data was found for racmoD dataset ", ii, "!")
          warning(" !!  No data was found for racmoD dataset ", ii, "!")
        } else {
          message("     The racmoD dataset ", ii,
                  " has been successfully configured. (",
                  length(iiVarNames), " variables available)")
        }
      } else {
        message(" !!  src for racmoD", ii, "dataset not recognised !")
        warning(" !!  We don't recognise the src of the racmoD '", ii, "' dataset.",
                "\n   Type ?configure_polarcm and read the instructions.\n")
      }
    }
  }

  # Define defaults ------------------------------------------------------------
  # which dataset was defined first?
  defType <- names(.polarEnv$rcm)[[1]]                     # type e.g. racmoM
  defVer  <- names(.polarEnv$rcm[[1]])[1]                  # version e.g. rp2
  defSrc  <- .polarEnv$rcm[[1]][[1]]$src                   # src info
  token$defaults$rcm  <- c(defType, defVer, defSrc)        # store above
  token$defaults$grid <- token$grids[[defType]][[defVer]]  # default projection

  # Defaults of each data type
  token$defaults$racmoM  <- names(.polarEnv$rcm$racmoM)[[1]]
  token$defaults$racmoD  <- names(.polarEnv$rcm$racmoD)[[1]]

  message("     The polarcm defaults have been successfully configured.")
  message("       --> ", defType, " dataset ", defVer, " will be considered the ",
          "default RCM data.")

  # Confirm & Finish Configuration ---------------------------------------------
  token$configured <- TRUE
  token$message <- .polarEnv$testing

  # Remove the data added in the .Rprofile file (it's already been used!)
  origNames <- names(.polarEnv)
  rm(list = origNames, envir = .polarEnv)

  # Store as a hidden .polarEnv environment
  list2env(x = token, envir = .polarEnv)   # create new environment for the list

  # Let us know we succeeded and return a list if we want
  message("\n>>> polarcm configuration complete!\n")
  return(invisible(token))
}
