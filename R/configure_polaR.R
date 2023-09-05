configure_polaR <- function(refresh = FALSE) {
  #' Prepare necessary datasets and variables
  #'
  #' @description
  #'   Please read the Instructions below!
  #'
  #'   `polaR` functions need to know where certain datasets are on your system
  #'   to work - the `configure_polaR()` function helps by creating the
  #'   required paths and datasets in a hidden environment (`.polarEnv`) that
  #'   the other functions can access. The `configure_polaR` function therefore
  #'   needs to be called before any `polaR` functions are used (which is
  #'   automatically done as part of most `polaR` functions). The necessary
  #'   datasets *must* already be saved and accessible on your system (see
  #'   "Accessing Datasets"); any datasets that are not available need to be
  #'   defined as NULL and will be ignored, with predictable consequences -
  #'   functions that try to call them won't work!).
  #'
  #' @param refresh BINARY: Should `polaR` be configured again? Mainly for
  #'   developers and forces `polaR` to run the configuration again. It sets the
  #'   existing `.polarEnv` as an empty list, and then sources the ".Rprofile"
  #'   again. By default, 'refresh' is FALSE, because it is usually not
  #'   necessary to reconfigure the package very often at all -  after the first
  #'   call in a session, we usually just want it to return what has already
  #'   stored in the `.polarEnv` and reconfiguring would unnecessarily slow it
  #'   down.
  #'
  #' @details # Instructions
  #'
  #'   This function must be called before using any `polaR` functions (once
  #'   per R session). The raw data paths need to be defined in an ".Rprofile"
  #'   file. The ".Rprofile" file is simply a file called ".Rprofile" that runs
  #'   automatically when R starts a new session.
  #'
  #'   The ".Rprofile" file should be in either:
  #'   1) the current working directory ([getwd()]); or
  #'   2) the user's home directory (`Sys.getenv("HOME")`); or
  #'   3) the R installation ([R.home()]).
  #'
  #'   For more information on ".Rprofile" files, see Section 2.4 of
  #'   [https://csgillespie.github.io/efficientR/set-up.html]().
  #'
  #'   The following code should be copied and pasted into the ".Rprofile" file.
  #'   This code lets `polaR` know where to find the datasets on your system,
  #'   and also which dataset it is. You must adjust the paths to match your
  #'   directory structure:
  #'    - `.polarEnv$rawDataPath` must be relative to your current working
  #'    directory;
  #'    - all other paths must be relative to the "rawDataPath" directory.
  #'    - If the data is not available (or you don't want it accessible), the
  #'    path must be set as NULL (see racmoD example).
  #'
  #'   See "Defining Datasets" below for more information on modifying the
  #'   following code block.
  #'
  #'   ```R
  #'     # polaR ========================================================================
  #'     if(!exists(".polarEnv")) .polarEnv <- new.env()       # hidden polaR environment
  #'
  #'     ## Raw Data Path ---------------------------------------------------------------
  #'     .polarEnv$rawDataPath     <- "../../Data/"            # relative to working dir
  #'
  #'     ## MEaSURES Data Path ----------------------------------------------------------
  #'     .polarEnv$MEaSURES        <- "MEaSURES Boundaries/"   # relative to rawDataPath
  #'
  #'     ## Monthly RACMO Data Paths ----------------------------------------------------
  #'     .polarEnv$rcm$racmoM$rp3  <- list("dir" = RACMO/RACMO2.3p3_CON_ANT27_monthly/",
  #'                                       "src" = "10.5281/zenodo.5512076")
  #'
  #'     ## Daily RACMO Data Paths ------------------------------------------------------
  #'     .polarEnv$rcm$racmoD      <- NULL      # no racmoD data
  #'
  #'     ## RCM Datasets ----------------------------------------------------------------
  #'     .polarEnv$rcm$marM        <- NULL      # no marM data
  #'     .polarEnv$rcm$marD        <- NULL      # no marD data
  #'     .polarEnv$rcm$marH        <- NULL      # no marH (hourly) data
  #'
  #'   ```
  #'
  #'   # Accessing Datasets
  #'   `polaR` **DOES NOT** download data for you. Follow the doi links on this
  #'   page to find data on Zenodo and download the data you want to use first.
  #'
  #'   `polaR` has been created to handle some commonly-used, openly-accessible
  #'   RACMO and MAR datasets shared on Zenodo (see "Zenodo RCM Datasets"). Some
  #'   functions will also work with other versions of the data, or perhaps even
  #'   output from other RCMs, if you **really** want to try it.
  #'
  #'   However, there is no standard way to share MAR or RACMO output (or any
  #'   other RCM output). The lack of standardisation means that a lot of the
  #'   ease of using `polaR` comes from background wrangling of the datasets to
  #'   get the data into the structure that `polaR` expects. For example, some
  #'   authors distribute the data with each file storing a different variable,
  #'   whilst others store all variables in a single file, but create separate
  #'   files for each year or month. `polaR` works best when data is organised
  #'   by variable.
  #'
  #'   As noted, `polaR` handles these differences as well as it can do in the
  #'   background, but the package has only been set up for certain datasets
  #'   *so far*. The following subsections list the datasets that `polaR` knows
  #'   what to do with. These datasts are all openly available on Zenodo, and
  #'   need to be identified in the ".Rprofile" file by their doi (see
  #'   "Defining Datasets").
  #'
  #'   ## MEaSURES Data
  #'   `polaR` bases the spatial aspects (e.g. using shelf outlines or grounding
  #'   line) on MEaSURES data. The MEaSURES data (Version 2; Mouginot et al.,
  #'   2017) needs to be downloaded first, from here:
  #'
  #'   ## Zenodo RCM Datasets
  #'   ### RACMO Data
  #'   - `[r01_dt]`  RACMO2.3p3 Monthly Data Antarctica   [https://doi.org/10.5281/zenodo.5512076]()
  #'
  #'   - `[r02_dt]`  RACMO2.3p2 Monthly Data Antarctica   [https://doi.org/10.5281/zenodo.7760490]()
  #'
  #'   - `[r03_dt]`  RACMO2.3p2 Monthly Data AA Peninsula [https://doi.org/10.5281/zenodo.7961732]()
  #'
  #'   ### MAR Data
  #'   - `[m01_dt]`  MARv3.11 3-Hourly Melt AA Peninsula  [https://doi.org/10.5281/zenodo.6347190]()
  #'
  #'   *Note:* The Zenodo doi's listed resolve to the latest version of the
  #'   dataset on Zenodo; it is assumed that any versions of the datasets shared
  #'   using the same Zenodo dataset will be set up in the same way. Let me know
  #'   if this is not the case.
  #'
  #'   *Note:* If multiple datasets are added to a section (e.g. there is both
  #'   RACMO2.3p2 and RACMO2.3p3 monthly data), the first one defined will be
  #'   considered as the default in functions. This can be overridden in the
  #'   functions that care (mainly the `read_x()` functions) using the 'version'
  #'   argument set to match the corresponding `<name>` (see "Defining Datasets"
  #'   below).
  #'
  #'   *Note:* It is okay to choose your own name for the directory containing
  #'   these datasets (as long as they are defined in ".Rprofile"), but the file
  #'   names should be the same as those on Zenodo.
  #'
  #'   *Note:* Ideally, there will become a standardised way of distributing
  #'   such data in future, but if you are aware of any other openly accessible
  #'   datasets that you think should be added to this list, let me know and
  #'   I'll see whether I can add them in a future update.
  #'
  #'   # Defining Datasets
  #'   To make different datasets available, it is necessary to configure your
  #'   ".Rprofile" file by including the following information for **each**
  #'   dataset type:
  #'
  #'      .polarEnv$rcm$<type>$<name> <- list("dir" = <dataDirectory>,
  #'                                          "src" = <doiCode>)
  #'
  #'   `<type>` Some functions need to know whether this is RACMO or MAR data,
  #'   and whether it is monthly, daily, or hourly data. Currently, the options
  #'   are:
  #'     racmoM    Monthly RACMO
  #'     racmoD    Daily RACMO
  #'     marM      Monthly MAR
  #'     marD      Daily MAR
  #'     marH      Hourly MAR
  #'
  #'   All of these options **must** be included in the ".Rprofile" even if the
  #'   dataset is not available or not wanted. In which case, just set the value
  #'   as NULL (e.g. `.polarEnv$rcm$marM <- NULL`).
  #'
  #'   `<name>` How do you want to refer to this dataset in your code? For
  #'   example, when reading in data, which is more your style?:
  #'   ```R
  #'      read_racmoM(var = "snowmelt", ver = "rp2")
  #'      read_racmoM(var = "snowmelt", ver = "RACMO2.3p2")
  #'      read_racmoM(var = "snowmelt", ver = "_anyOtherName.You.WANT_2uSE")
  #'   ```
  #'
  #'   `<dataDirectory>` Where are the NetCDFs stored on your system? Must be a
  #'   relative path compared to the "Data/" directory as defined in the
  #'   ".Rprofile" file as ".polarEnv$rawDataPath".
  #'
  #'   `<doiCode>` Which dataset is this? As explained above, there is no
  #'   standard way to distribute these datasets so we need to explicitly tell
  #'   `polaR` what dataset we are giving it. We do this using the doi of the
  #'   download page. For example, for the monthly RACMO2.3p3 data above
  #'   (r01_dt, with doi "https://doi.org/10.5281/zenodo.5512076"), the
  #'   <doiCode> must be "10.5281/zenodo.5512076". If the data has not been
  #'   shared, but you know it is in EXACTLY the same format (e.g. organisation
  #'   of data files, crs, extent), you can just add the respective <doiCode>.
  #'
  #'   ## Directory Structure
  #'   This function assumes that all raw data is stored in a "Data/" directory
  #'   (i.e. what we define in ".Rprofile" using '.polarEnv$rawDataPath').
  #'   Separate subdirectories are then expected for the MEaSURES data and any
  #'   RACMO or MAR datasets. These can be either separate directories for each
  #'   dataset (e.g. the RACMO Monthly examples below) or further nested (e.g.
  #'   the MAR Monthly examples below). Whichever way, make sure that the path
  #'   in the "dir" points to the dataset.
  #'
  #'   ```
  #'   Data/
  #'   | - - MEaSURES Boundaries/
  #'   |     | - Basins_Antarctica/
  #'   |         | - Basins_Antarctica_v02.shp
  #'   |
  #'   | - - RACMO2.3p3 Monthly/
  #'   |     | - albedo.nc
  #'   |     | - swsd.nc
  #'   |
  #'   | - - RACMO2.3p2 Monthly/
  #'   |     | - albedo.nc
  #'   |     | - swsd.nc
  #'   |
  #'   | - - MAR Monthly/
  #'   |     | - MARv3.11/
  #'   |     |   | - 2015.nc
  #'   |     |   | - 2016.nc
  #'   |     | - MARv3.12/
  #'   |         | - 2011.nc
  #'   |
  #'   | - - MAR Daily/
  #'   |     | - 1980.nc
  #'   |     | - 1981.nc
  #'   ```
  #'
  #'   *!!!REMEMBER!!!*
  #'
  #'   The data must have already been downloaded before
  #'   trying to use `polaR` - the package *does not* try to find and download
  #'   data for you. If any datasets are not available, set their path as
  #'   `NULL`. NULLs will restrict the capabilities of the racmoR package in
  #'   expected ways - for example, if no daily RACMO data is available, the
  #'   racmoD functions will fail. However, that may not be an issue. The
  #'   MEaSURES data is used for any location-based calculations or subsetting,
  #'   and for drawing borders.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Check if the .Rprofile has been prepared configuration
  if (!exists(".polarEnv")) {
    stop("\nType ?configure_polaR and read the Instructions section.")
  } else if (isTRUE(refresh)) {
    # print("reconfiguring")
    list2env(x = list(), envir = .polarEnv)
    source(".Rprofile")            # print(".Rprofile sourced, .polarEnv should be refreshed!")
    .polarEnv$configured <- FALSE  # print("added nonconfigured to .polarEnv")
  } #else { print("just proceeding...") }

  # Do we need to configure everything?
  if (isTRUE(.polarEnv$configured)) {
    # message("polaR already configured!")
    token <- as.list(.polarEnv)
    return(invisible(token))
  } else {
    message("Configuring polaR...\n")
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

  token$grids$ext$racmoApExt <- terra::ext(c(-32.025, -12.525, -8.725, 5.975))
  token$grids$crs$racmoApCrs <- paste("+proj=ob_tran",
                                      "+o_proj=latlon",
                                      "+o_lat_p=-180.0 +lon_0=30.0",
                                      "-m 57.295779506")

  token$grids$ext$marExt     <- NA
  token$grids$crs$marCrs     <- paste("+proj=stere",
                                      "+lat_0=-90 +lat_ts=-71",
                                      "+lon_0=0 +x_0=0 +y_0=0",
                                      "+datum=WGS84 +units=km +no_defs +type=crs") # km!

  token$grids$crs$lambertCrs <- paste("+proj=laea",
                                      "+lat_0=-90 +lon_0=0",
                                      "+x_0=0 +y_0=0",
                                      "+datum=WGS84 +units=m +no_defs +type=crs")

  token$grids$crs$orthoCrs   <- paste("+proj=ortho",
                                      "+f=0 +lat_0=-90 +lon_0=0",
                                      "+x_0=0 +y_0=0",
                                      "+datum=WGS84 +units=m +no_defs +type=crs")

  message("  The crs definitions have been succesfully configured.")

  # MEaSURES Datasets ----------------------------------------------------------
  if (!is.null(.polarEnv$MEaSURES)) {
    # Basic path & directory
    rawDir <- paste0(rawDataPath, .polarEnv$MEaSURES)
    token$dirPaths$MEaSURES   <- rawDir
    token$dirFolders$MEaSURES <- basename(rawDir)
    token$datasets$MEaSURES$doi  <- "doi.org/10.5067/AXE4121732AD"
    token$datasets$MEaSURES$name <-
         "MEaSUREs Antarctic Boundaries for IPY 2007-2009 from Satellite Radar, Version 2"
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
      message("  The MEaSURES coastline dataset has been succesfully configured.")
    } else {warning("Cannot access the coastline in the MEaSURES dataset! ",
                    "   Expected filename:\n  ", coast, "\n\n")}

    # Antarctic Grounding Line
    GL <- paste0(rawDir,
                 "GroundingLine_Antarctica/GroundingLine_Antarctica_v02.shp")
    if (file.exists(GL)) {
      GL <- terra::vect(GL)
      token$measures$groundingLine <- GL
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "groundingLine")
      token$grids$measures$groundingLine$crs <- "EPSG:3031"
      token$grids$measures$groundingLine$ext <- terra::ext(GL)
      message("  The MEaSURES grounding line dataset has been succesfully configured.")
    } else {warning("Cannot access the grounding line in the MEaSURES dataset! ",
                    "   Expected filename:\n  ", GL, "\n\n")}

    # Antarctic Ice Shelves
    shelves <- paste0(rawDir,
                      "IceShelf_Antarctica/IceShelf_Antarctica_v02.shp")
    if (file.exists(shelves)) {
      shelves <- terra::vect(shelves)
      token$measures$iceShelves <- shelves
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "iceShelves")
      token$grids$measures$iceShelves$crs <- "EPSG:3031"
      token$grids$measures$iceShelves$crs <- terra::ext(shelves)
      message("  The MEaSURES ice shelves dataset has been succesfully configured.")
    } else {warning("Cannot access ice shelves in the MEaSURES dataset! ",
                    "   Expected filename:\n  ", shelves, "\n\n")}

    # IMBIE Basins (e.g. A-Ap)
    imbie <- paste0(rawDir,
                    "Basins_IMBIE_Antarctica/Basins_IMBIE_Antarctica_v02.shp")
    if (file.exists(imbie)) {
      imbie <- terra::vect(imbie)
      token$measures$imbieBasins <- imbie
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "imbieBasins")
      token$grids$measures$imbieBasins$crs <- "EPSG:3031"
      token$grids$measures$imbieBasins$ext <- terra::ext(imbie)
      message("  The MEaSURES IMBIE basins dataset has been succesfully configured.")
    } else {warning("Cannot access IMBIE Basins in the MEaSURES dataset! ",
                    "   Expected filename:\n  ", imbie, "\n\n")}

    # Refined Basins (e.g. Vincennes_Bay)
    basins <- paste0(rawDir,
                     "Basins_Antarctica/Basins_Antarctica_v02.shp")
    if (file.exists(basins)) {
      basins <- terra::vect(basins)
      token$measures$refinedBasins <- basins
      token$datasets$MEaSURES$maps <- c(token$datasets$MEaSURES$maps, "refinedBasins")
      token$grids$measures$refinedBasins$crs <- "EPSG:3031"
      token$grids$measures$refinedBasins$ext <- terra::ext(basins)
      message("  The MEaSURES refined basins dataset has been succesfully configured.")
    } else {warning("Cannot access refined basins in the MEaSURES dataset! ",
                    "   Expected filename:\n  ", basins, "\n\n")}
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
      if (iiSrc %in% c("10.5281/zenodo.5512076", "10.5281/zenodo.7760490")) {
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
        } else if (iiSrc == "10.5281/zenodo.7760490") {
          token$datasets$racmoM[[ii]]$name    <-
               "Monthly averaged RACMO2.3p2 variables (1979-2022); Antarctica"
          token$datasets$racmoM[[ii]]$authors <- c("J.M. van Wessem",
                                                   "W.J. van de Berg",
                                                   "M.R. van den Broeke")
        }

        # Assign grid information
        token$grids$racmoM[[ii]]$crs <- token$grids$crs$racmoCrs
        token$grids$racmoM[[ii]]$ext <- token$grids$ext$racmoExt

        message("  The ", ii, " racmoM dataset has been succesfully configured.")
      } else if (iiSrc %in% c("10.5281/zenodo.7961732")) {
        # print("Antarctic Peninsula")
        # Get paths for this dataset version
        iiVarPaths <- list.files(iiRawDir, pattern = ".nc",  # only NetCDF files
                                 full.names = TRUE)          # get full paths

        # Get variable names
        iiVarNames <- basename(iiVarPaths) |>
          strsplit("_") |>
          lapply('[', 1) |>
          unlist()

        # Store variables & paths
        warning("A decision needs to be made on the ", ii, " dataset!\n")

        # Assign grid information
        token$grids$racmoM[[ii]]$crs <- token$grids$crs$racmoApCrs
        token$grids$racmoM[[ii]]$ext <- token$grids$ext$racmoApExt
        # token$grids$racmoM[[ii]]$res <- c(0.05, 0.05)

      } else {
        warning("  We don't recognise the src of the racmoM '", ii, "' dataset.",
                "\n  Type ?configure_antarctica and read the instructions.\n")
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
        token$datasets$racmoD[[ii]]$doi    <- paste0("doi.org/", iiSrc)
        token$datasets$racmoD[[ii]]$name   <-
             "Daily version of RACMO2.3p3 SMB, SEB and t2m data for Antarctica (1979-2018)"
        token$datasets$racmoM[[ii]]$authors <- c("C. van Dalum",
                                                 "W.J. van de Berg",
                                                 "M. van den Broeke")

        # Assign grid information
        token$grids$racmoD[[ii]]$crs <- token$grids$crs$racmoCrs
        token$grids$racmoD[[ii]]$ext <- token$grids$ext$racmoExt

        message("  The ", ii, " racmoD dataset has been succesfully configured.")
      } else {
        warning("  We don't recognise the src of the racmoD '", ii, "' dataset.",
                "\n  Type ?configure_antarctica and read the instructions.\n")
      }
    }
  }

  # marH Datasets --------------------------------------------------------------
  if (!is.null(.polarEnv$rcm$marH)) {
    for (ii in names(.polarEnv$rcm$marH)) {
      # Which data is this?
      iiRawDir <- paste0(rawDataPath, .polarEnv$rcm$marH[[ii]]$dir)
      iiSrc <- .polarEnv$rcm$marH[[ii]]$src

      # Store path & folder
      token$dirPaths$marD[[ii]]   <- iiRawDir
      token$dirFolders$marD[[ii]] <- basename(iiRawDir)

      # Handling marH data depends on the dataset version
      if (iiSrc %in% c("10.5281/zenodo.6347190")) {
        # Get paths for this dataset version
        iiVarPaths <- list.files(iiRawDir, ".nc",
                                 full.names = TRUE)

        # Get variable names
        iiVarNames <- basename(iiVarPaths) |>
          strsplit("_") |>
          lapply('[', 2) |>
          unlist() |>
          unique()

        # Store variable names in a named list for "$" access
        token$varNames$marH[[ii]] <- iiVarNames

        # Store variable paths in a named list for "$" access
        for (jj in iiVarNames) {
          # Which files contain this variable?
          jjFiles <- list.files(iiRawDir, pattern = paste0(jj, ".+nc"))

          # Store
          token$varPaths$marH[[ii]][[jj]] <- jjFiles
        }

        # Keep a record of which dataset this is, regardless of the user's name
        token$datasets$marH[[ii]]$doi     <- paste0("doi.org/", iiSrc)
        token$datasets$marH[[ii]]$name    <-
             "Antarctic Peninsula MAR 3-hourly data"
        token$datasets$marH[[ii]]$authors <- c("C. Kittel", "X. Fettweis",
                                               "J. Wille")

        message("  The ", ii, " marH dataset has been succesfully configured.")
      } else {
        warning("  We don't recognise the src of the marH '", ii, "' dataset.",
                "\n  Type ?configure_antarctica and read the instructions.\n")
      }
    }
  }

  # Define defaults ------------------------------------------------------------
  token$defaults$racmoM  <- names(.polarEnv$rcm$racmoM)[[1]]
  token$defaults$racmoD  <- names(.polarEnv$rcm$racmoD)[[1]]
  token$defaults$marM    <- names(.polarEnv$rcm$marM)[[1]]
  token$defaults$marD    <- names(.polarEnv$rcm$marD)[[1]]
  token$defaults$marH    <- names(.polarEnv$rcm$marH)[[1]]
  token$defaults$rcm     <- c(names(.polarEnv$rcm)[[1]],    # type (e.g. racmoM)
                              names(.polarEnv$rcm[[1]])[1], # version (e.g. rp2)
                              .polarEnv$rcm[[1]][[1]]$src)  # src

  message("  The polaR defaults have been succesfully configured.")

  # Confirm & Finish Configuration ---------------------------------------------
  token$configured <- TRUE
  token$message <- .polarEnv$testing

  # Remove the data added in the .Rprofile file (it's already been used!)
  origNames <- names(.polarEnv)
  rm(list = origNames, envir = .polarEnv)

  # Store as a hidden .polarEnv environment
  list2env(x = token, envir = .polarEnv)   # create new environment for the list

  # Let us know we succeeded and return a list if we want
  message("\npolaR succesfully configured!\n")
  return(invisible(token))
}

# alt name options: polaR  antarcticR antarcticaR   Rcm   maRacmo   racmoR racMar

# OLD ====

#' configure_racmoR <- function() {
#'   #' Define paths to RACMO datasets and variables
#'   #'
#'   #' @description
#'   #'   Please read the Instructions below!
#'   #'
#'   #'   `racmoR` functions need to know where certain datasets are on your system
#'   #'   to work - the `configure_racmoR()` function helps functions access these
#'   #'   datasets by creating the necessary paths and adding them to a hidden
#'   #'   ".racmoR" environment. This function therefore needs to be called before
#'   #'   any `racmoR` functions are used (which is automatically done as part of
#'   #'   most `racmoR` functions). The datasets must already be saved and
#'   #'   accessible on your system.
#'   #'
#'   #' @details # Instructions
#'   #'
#'   #'   This function must be called before using any `racmoR` functions (once
#'   #'   per R session). The raw data paths need to be defined in an ".Rprofile"
#'   #'   file. The ".Rprofile" file should be in either: the current working
#'   #'   directory ([getwd()]), the user's home directory (`Sys.getenv("HOME")`),
#'   #'   or the R installation ([R.home()]). The ".Rprofile" file is simply a file
#'   #'   called ".Rprofile" that runs automatically when R starts a new session.
#'   #'   For more info on ".Rprofile" files, see Section 2.4 of
#'   #'   [https://csgillespie.github.io/efficientR/set-up.html]().
#'   #'
#'   #'   The following code should be pasted into the ".Rprofile" file. This will
#'   #'   let `racmoR` where to find the RACMO data. Therefore, you must adjust the
#'   #'   paths to match your directory structure; the paths should be relative to
#'   #'   your current working directory.
#'   #'
#'   #' ```R
#'   #'   # Prepare data paths for racmoR
#'   #'   .racmoR = new.env() # hidden racmoR environment
#'   #'   .racmoR$rawDataPath <- "../../Data/"
#'   #'   .racmoR$MEaSURES    <- "MEaSURES Boundaries/"
#'   #'   .racmoR$racmoM      <- "RACMO/RACMO2.3p3_CON_ANT27_monthly/"
#'   #'   .racmoR$racmoD      <- "RACMO/RACMO2.3p3_CON_ANT27_daily/"
#'   #'   attach(.racmoR)
#'   #' ```
#'   #'
#'   #'   This function assumes that all raw data is stored in a "Data" directory
#'   #'   (i.e. 'rawDataPath'), with separate subdirectories for RACMO data at
#'   #'   monthly ('racmoM') and daily ('racmoD') resolution, and for the
#'   #'   'MEaSURES' data. The necessary data must already be downloaded before
#'   #'   trying to use `racmoR` - the package *does not* try to find and download
#'   #'   data for you. If any datasets are not available, set their path as
#'   #'   `NULL`. NULLs will restrict the capabilities of the racmoR package in
#'   #'   expected ways - for example, if no daily RACMO data is available, the
#'   #'   racmoD functions will fail. However, that may not be an issue. The
#'   #'   MEaSURES data is used for any location-based calculations or subsetting,
#'   #'   and for drawing borders.
#'   #'
#'   #' # Data Access
#'   #'   Access monthly RACMO2.3p3 data here (van Dalum et al., 2021):
#'   #'    [https://doi.org/10.5281/zenodo.5512076.]().
#'   #'
#'   #'   Access MEaSURES data here (Mouginot et al., 2017):
#'   #'    [https://nsidc.org/data/nsidc-0709/versions/2]().
#'   #'
#'   #'   Read more about MEaSURES data here:
#'   #'    [https://nsidc.org/sites/default/files/nsidc-0709-v002-userguide.pdf]().
#'   #'
#'   #' # References
#'   #'
#'   #' Mouginot, J, B Scheuchl & E Rignot (**2017**) MEaSUREs Antarctic
#'   #' Boundaries for IPY 2007-2009 from Satellite Radar, Version 2. Boulder,
#'   #' Colorado USA. NASA National Snow and Ice Data Center Distributed Active
#'   #' Archive Center. [https://doi.org/10.5067/AXE4121732AD]().
#'   #' Last access: 08-28-2023.
#'   #'
#'   #' van Dalum, CT, WJ van de Berg & MR van den Broeke (**2021**) RACMO2.3p3
#'   #' monthly SMB, SEB and t2m data for Antarctica (1979-2018). \[CON Data,
#'   #' Version 2\]. Zenodo. [https://doi.org/10.5281/zenodo.7639053]().
#'   #' Last access: 08-28-2023.
#'   #'
#'   #' @export
#'
#'   # Code -----------------------------------------------------------------------
#'   # Check if the .Rprofile necessary for configuration has been prepared
#'   if (!exists(".racmoR")) {
#'     stop("\nType ?configure_racmoR and read the Instructions section.")
#'   }
#'
#'   # Check if the function has already been called.
#'   # crs_racmo is created last - if it exists, everything went okay
#'   if (exists("crs_racmo", envir = .racmoR)) {
#'     token <- as.list(.racmoR)
#'     return(invisible(token))
#'   }
#'
#'   ### !!! Only run beyond here if the function hasn't been called before !!! ###
#'
#'   # Access raw data paths
#'   rawDataPath <- .racmoR$rawDataPath
#'   racmoM      <- .racmoR$racmoM
#'   racmoD      <- .racmoR$racmoD
#'   MEaSURES    <- .racmoR$MEaSURES
#'   rm(list = c("racmoD", "racmoM", "rawDataPath", "MEaSURES"), envir = .racmoR)
#'
#'   # Create a token to hold all the info
#'   token <- list()   # preallocate
#'   token$dirFolders <- data.frame("rawData"  = basename(rawDataPath))
#'   token$dirPaths   <- data.frame("rawData"  = rawDataPath)
#'
#'   # Daily Data
#'   if (!is.null(racmoD)) {
#'     # Paths & Folders
#'     rawDir <- paste0(rawDataPath, racmoD)
#'     token$dirPaths$racmoD   <- rawDir
#'     token$dirFolders$racmoD <- racmoD
#'
#'     # Variables
#'     token$racmoVars$racmoD <- list.files(rawDir, ".nc") |>  # variable names
#'       strsplit("_") |>
#'       lapply('[', 1) |>
#'       unlist() |>
#'       unique()
#'   }
#'
#'   # Monthly Data
#'   if (!is.null(racmoM)) {
#'     # Paths & Folders
#'     rawDir <- paste0(rawDataPath, racmoM)        # path to racmoM directory
#'     token$dirPaths$racmoM   <- rawDir
#'     token$dirFolders$racmoM <- racmoM
#'
#'     # Variables
#'     rawPaths  <- list.files(rawDir, ".nc",       # only NetCDFs
#'                             full.names = TRUE)   # (full paths)
#'     rawNames  <- list.files(rawDir, ".nc") |>    # variable names
#'       strsplit("_") |>
#'       lapply('[', 1) |>
#'       unlist() |>
#'       unique()
#'
#'     # Create data.frame (as it is easy to access using "$")
#'     token$varPaths$racmoM <- data.frame(matrix(rawPaths, nrow = 1)) |>
#'       `colnames<-`(rawNames)
#'
#'     token$racmoVars$racmoM <- rawNames
#'   }
#'
#'   # MEaSURES Data
#'   if (!is.null(MEaSURES)) {
#'     # MEaSURES paths & directory
#'     rawDir <- paste0(rawDataPath, MEaSURES)
#'     token$dirPaths$measures   <- rawDir
#'     token$dirFolders$measures <- MEaSURES
#'
#'     # IMBIE Basins (e.g. A-Ap)
#'     imbie <- paste0(rawDir,
#'                     "Basins_IMBIE_Antarctica/Basins_IMBIE_Antarctica_v02.shp")
#'     token$imbie$basins <- terra::vect(imbie)
#'
#'     # Refined Basins (e.g. Vincennes_Bay)
#'     basins <- paste0(rawDir,
#'                      "Basins_Antarctica/Basins_Antarctica_v02.shp")
#'     token$measures$basins <- terra::vect(basins)
#'
#'     # Antarctic Coastline
#'     coast <- paste0(rawDir,
#'                     "Coastline_Antarctica/Coastline_Antarctica_v02.shp")
#'     token$measures$coastline <- terra::vect(coast)
#'
#'     # Antarctic Grounding Line
#'     GL <- paste0(rawDir,
#'                  "GroundingLine_Antarctica/GroundingLine_Antarctica_v02.shp")
#'     token$measures$groundingLine <- terra::vect(GL)
#'
#'     # Antarctic Ice Shelves
#'     shelves <- paste0(rawDir,
#'                       "IceShelf_Antarctica/IceShelf_Antarctica_v02.shp")
#'     token$measures$shelves <- terra::vect(shelves)
#'   }
#'
#'   # Define RACMO crs
#'   token$crs_racmo <- use_crs("racmo")# maybe this should be the other way around
#'
#'   # Store in the hidden racmoR environment & return
#'   detach(.racmoR)                        # remove existing
#'   list2env(x = token, envir = .racmoR)   # create new
#'   attach(.racmoR)                        # attach new
#'   return(invisible(token))               # also return as a list
#' }



# # NEW THOUGHTS #################################################################
#
# Because there is no standard way to share MAR or RACMO output, there needs to
# be quite a lot of wrangling in the background to get the data into the
# structure that `.racmoR` expects. For example, some authors distribute the
# data with each file storing a different variable, whilst others store all
# variables in the same file, but create separate files for each year or month.
# `.racmoR` works best when data is organised by variable.
#
# .racmoR handles this as well as it can in the background, but has only been
# set up for certain datasets *so far*. The following datasets, which are all
# openly available on Zenodo, can be handled, providing that the correct doi is
# defined in the ".Rprofile" file:
#
#   RACMO Data
#   [r01]  RACMO2.3p3 Monthly Data Antarctica   [https://doi.org/10.5281/zenodo.5512076]
#   [r02]  RACMO2.3p2 Monthly Data Antarctica   [https://doi.org/10.5281/zenodo.7760490]
#   [r03]  RACMO2.3p2 Monthly Data AA Peninsula [https://doi.org/10.5281/zenodo.7961732]
#
#   MAR Data
#   [m01]  MARv3.11 3-Hourly Melt AA Peninsula  [https://doi.org/10.5281/zenodo.6347190]
#
# Ideally, there would become a standardised way of distributing such data in
# future, but if you are aware of any other datasets that you think should be
# added to this list, let me know and I'll see whether I can add them in a
# future update.
#
# In ".Rprofile", it is necessary to configure a file in the following way:
#
#   .racmoR$<type>$<name> <- list("dir" = <dataDirectory>,
#                                 "src" = <doiCode>)
#
#   <type> Some functions need to know whether this is RACMO or MAR data, and
#   whether it is monthly, daily, or hourly data. Currently, the options are:
#     racmoD    Daily RACMO
#     racmoM    Monthly RACMO
#     marM      Monthly MAR
#     marD      Daily MAR
#     marH      Hourly MAR
#
#   <name> How do you want to refer to this dataset in your code? For example,
#   when reading data in:
#   `read_racmoM(var = "snowmelt", ver = "rp2")` or
#   `read_racmoM(var = "snowmelt", var = "RACMO2.3p2")`
#
#   <dataDirectory> Where are the NetCDFs stored on your system? Must be a
#   relative path compared to the "Data/" directory defined in the .Rprofile
#   file as ".racmoR$rawDataPath".
#
#   <doiCode> Which dataset is this? As explained above, there is no standard
#   way to distribute these datasets so we need to explicitly tell `.racmoR`
#   what we are giving it. This should be the doi of the download page. For
#   example, for the monthly RACMO2.3p3 data above (r01), with doi
#   https://doi.org/10.5281/zenodo.5512076, the doiCode must be
#   "10.5281/zenodo.5512076". If the data has not been shared, but you know it
#   is in EXACTLY the same format, you can just add the respective doiCode.
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
#
# # racmoD
# ```{r}
# for (ii in names(.racmoR$racmoD)) {
#   iiRawDir <- paste0(rawDataPath, .racmoR$racmoD[[ii]])
#   token$dirPaths[[paste0("racmoD_", ii)]] <- iiRawDir
#   token$dirFolders[[paste0("racmoD_", ii)]] <- basename(iiRawDir)
#
#   # All file paths for ii version of racmoD
#   iiVarPaths <- list.files(iiRawDir, ".nc")
#
#   iiVarNames <- strsplit(iiVarPaths, "_") |>
#     lapply('[', 1) |>
#     unlist() |>
#     unique()
#
#   for (jj in iiVarNames) {
#     jjFiles <- list.files(iiRawDir, pattern = paste0(jj, ".+nc"))
#     token$varPaths$racmoD[[ii]][[jj]] <- jjFiles
#   }
# }
# ```
