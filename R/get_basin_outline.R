get_basin_outline <- function(extent = "",
                              rectangularExtent = FALSE,
                              returnImbie = NULL,
                              crs = NULL,
                              preferType = NULL,
                              useOnly = NULL,
                              imbieBasins = NULL,
                              crsIn = NULL) {
  #' Return basin outlines for a given extent
  #'
  #' @description This function is useful for plotting or cropping to Antarctic
  #'   basins. It is mainly used in the [draw_antarctica()] and
  #'   [crop_to_basin()] functions, but can be used separately as well.
  #'
  #'   It is possible to return only IMBIE basins (e.g. "A-Ap"), only the
  #'   refined MEaSURES basins (e.g. "Vincennes_Bay"), or both; see the
  #'   'returnImbie' argument.
  #'
  #'   The function works in two ways.
  #'
  #'   1) A vector of basin names can be entered as the 'extent', and these
  #'   exact basins are simply returned (for either the IMBIE or the refined
  #'   basins). The basin names must match those in the MEaSURES dataset
  #'   exactly.
  #'
  #'   2) An extent can be defined according to [get_extent()]. Any basins which
  #'   intersect with this extent (and match the 'returnImbie' argument) are
  #'   then returned. This occurs if 'rectangularExtent' is TRUE, and most of
  #'   the remaining arguments are fed directly into `get_extent()`.
  #'
  #'   The basins will be returned in the projection specified by the 'crs'
  #'   argument.
  #'
  #'   **Note:** If an existing basin SpatVector is entered, it will be
  #'   returned.
  #'
  #' @param extent Define the extent used to select basins.
  #'
  #'   To return only specific basins, enter their names as a vector, and set
  #'   'rectangularExtents' as FALSE (the default).
  #'
  #'   To return any basins that fall within a geographical extent, set
  #'   'rectangularExtents' as TRUE, and follow the logic of `get_extents()` to
  #'   define the geographical extent with the remaining parameters. The most
  #'   obvious reason to do this would be to include the outline of basins
  #'   within the bounding box of the named basins, but that aren't named
  #'   themselves. See examples.
  #'
  #' @param returnImbie BINARY: By default (NULL), both the "IMBIE" and refined
  #'   "MEaSURES" basins are returned.
  #'
  #'   To return only the IMBIE basins, set this as TRUE. To return the MEaSURES
  #'   refined basins, set this as FALSE.
  #'
  #'   **Note:** This argument is distinct to the 'useOnly' and 'imbieBasins'
  #'   arguments, which are fed in `get_extent()` to define the initial extent
  #'   to search for basins in.
  #'
  #' @param crs "string": Which projection should the basins be returned in?
  #'   See `use_crs()` or `terra::crs()`. By default (i.e. NULL), it will match
  #'   the first RCM data defined in the ".Rprofile".
  #'
  #' @inheritParams get_extent
  #'
  #' @examples -----------------------------------------------------------------
  #' \dontrun{
  #'   # Compare 'returnImbie' argument
  #'   t1 <- "A-Ap"
  #'   get_basin_outline(t1) |>
  #'     terra::plot(col = "black")
  #'   get_basin_outline(t1, FALSE, returnImbie = TRUE) |>
  #'     terra::lines(col = "red", lwd = 2)
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Handle default CRS
  token  <- configure_polarcm()
  crs    <- set_if_null(crs, token$defaults$grid$crs)
  crs    <- use_crs(crs)

  # Prepare possible basin combinations; basins are stored as EPSG:3031
  if (isTRUE(returnImbie)) {
    basins <- token$measures$imbieBasins
  } else if (isFALSE(returnImbie)) {
    basins <- token$measures$refinedBasins
  } else if (is.null(returnImbie)) {
    basins <- rbind(token$measures$imbieBasins,
                    token$measures$refinedBasins)
  }

  # The following extents must be rectangles, as there's not enough information
  if ("SpatRaster" %in% methods::is(extent)) {
    # print("input is a SpatRaster")
    rectangularExtent <- TRUE
  } else if ("SpatExtent" %in% methods::is(extent)) {
    if (extent[1] < extent[2] & extent[3] < extent[4]) {
      # print("input is a SpatExtent")
      rectangularExtent <- TRUE
    } else {
      stop("An extent needs to be c(xmin, xmax, ymin, ymax).")
    }
  } else if ("SpatVector" %in% methods::is(extent)) {
    # print("input is a SpatVector")
    next       # don't do anything yet
  } else if (extent[[1]] == "") {
    # print("input is empty")
    rectangularExtent <- TRUE
  } # else extent should be the name/s as a string

  # Are we creating a rectangular extent, or using exact basin outlines?
  if (isTRUE(rectangularExtent)) {
    # print("A rectangular extent is requested")
    extent <- get_extent(extent = extent,
                         rectangularExtent = TRUE,
                         preferType = preferType,
                         useOnly = useOnly,
                         imbieBasins = imbieBasins,
                         crs = use_crs("stereo"),# return 3031 to match basins
                         crsIn = crsIn)

    # Establish which basins fall within the given extent
    basins   <- terra::crop(basins, extent)
  } else {
    # print("The exact outlines are requested")
    if ("SpatVector" %in% methods::is(extent)) {
      # Print("input is a SpatVector; basically do nothing")
      basins <- extent
    } else {
      # print("input is name/s; using exact basins")
      basins <- basins[basins$NAME %in% extent]
    }
  }

  # Reproject
  basins   <- terra::project(basins, use_crs(crs))
  return(basins)
}


# TESTING - uncomment a "tst" and the print + plot lines

# # Blank = All of Antarctica
# tst <- get_basin_outline("")
#
# # Named
# tst <- get_basin_outline("A-Ap", rectangularExtent = TRUE)
# tst <- get_basin_outline("A-Ap", rectangularExtent = FALSE)
# tst <- get_basin_outline(c("A-Ap", "Vincennes_Bay"), rectangularExtent = TRUE)
# tst <- get_basin_outline(c("A-Ap", "Vincennes_Bay"), rectangularExtent = FALSE)
#
# # SpatVector (run another one first)
# tst <- get_basin_outline(tst, rectangularExtent = TRUE)
# tst <- get_basin_outline(tst, rectangularExtent = FALSE)
#
# # SpatExtent
# tst <- get_basin_outline(terra::ext(-10, 10, 8, 18),
#                          crsIn = use_crs("racmo"),
#                          rectangularExtent = FALSE)
#
# # SpatRaster
#
#
#
# print(tst)
# terra::plot(tst)

# =============================================================================!
# SCRAP CODE


#
#   # Search for exact basin names
#   if (isFALSE(rectangularExtent)) {
#     if (extent[[1]] != "") {
#       basins <- basins[basins$NAME %in% extent]
#     }
#   } else {
#   # Define the required extent to search for basins within
#     extent <- get_extent(extent = extent,
#                          rectangularExtent = rectangularExtent,
#                          preferType = preferType,
#                          useOnly = useOnly,
#                          imbieBasins = imbieBasins,
#                          crs = use_crs("stereo"), # return 3031 to match basins
#                          crsIn = crsIn)
#
#     # Establish which basins intersect with the extent
#     basins <- terra::intersect(basins, extent)
#   }
#
#   # Reproject
#   basins   <- terra::project(basins, use_crs(crs))
#
#   return(basins)
# }


# EXAMPLES : THESE NEED TO BE BUILT INTO THE DOCUMENTATION
# t1 <- "A-Ap"
#
# # Compare returnImbie argument
# get_basin_outline(t1) |> terra::plot(col = "black")
# get_basin_outline(t1, FALSE, returnImbie = NULL) |>
#   terra::lines(col = "red", lwd = 2)
#
# get_basin_outline(t1) |> terra::plot(col = "black")
# get_basin_outline(t1, FALSE, returnImbie = TRUE) |>
#   terra::lines(col = "red", lwd = 2)
#
# get_basin_outline(t1) |> terra::plot(col = "black")
# get_basin_outline(t1, FALSE, returnImbie = FALSE) |>
#   terra::lines(col = "red", lwd = 2)
