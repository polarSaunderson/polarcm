get_basin_outline <- function(extent = "",
                              rectangularExtent = FALSE,
                              returnImbie = NULL,
                              crs = "racmo",
                              preferType = NULL,
                              useOnly = NULL,
                              imbieBasins = NULL,
                              crsIn = NULL) {
  #' Return basin outlines for a given extent
  #'
  #' @description This function is useful for plotting basins in Antarctica. It
  #'   is mainly used in the `draw_antarctica()` function, but works separately
  #'   as well. It is possible to return only IMBIE basins (e.g. "A-AP"), only
  #'   MEaSURES basins (e.g. "Vincennes_Bay"), or both; see 'returnImbie'.
  #'
  #'   The function works in two ways.
  #'
  #'   1) a vector of basin names can be entered as the 'extent', and these
  #'   basins are simply returned from the relevant MEaSURES/IMBIE dataset. The
  #'   names must match the dataset exactly.
  #'
  #'   2) an extent can be defined according to `get_extent()`. Any basins which
  #'   intersect with this extent (and match the 'returnImbie' argument) are
  #'   then returned. This occurs if 'rectangularExtent' is TRUE, and most of
  #'   the remaining arguments are fed directly into `get_extent()`.
  #'
  #'   Finally, the basins can be reprojected (set via 'crs').
  #'
  #' @param extent Define the extent used to select basins.
  #'
  #'   To return only specific basins, enter their names as a vector, and set
  #'   'rectangularExtents' as FALSE (the default).
  #'
  #'   To return any basins that fall within a geographical extent, set
  #'   'rectangularExtents' as TRUE, and follow the logic of `get_extents()` to
  #'   define the geographical extent. The most obvious reason to do this would
  #'   be to include the outline of basins within the bounding box of the named
  #'   basins, but that aren't named themselves. See examples.
  #'
  #' @param returnImbie BINARY: By default (NULL), both "IMBIE" and "MEaSURES"
  #'   basins within the extent are returned. Set this to return only the
  #'   "IMBIE" (TRUE) or "MEaSURES" (FALSE) basins. This argument is distinct to
  #'   the 'useOnly' and 'imbieBasins', which are used by `get_extent()` to
  #'   define the initial extent to search for basins in.
  #'
  #' @param crs "string": Which projection should the basins be returned in?
  #' @inheritParams get_extent
  #'
  #' @examples -----------------------------------------------------------------
  #'   # Compare returnImbie argument
  #'   t1 <- "A-Ap"
  #'   get_basin_outline(t1) |> terra::plot(col = "black")
  #'   get_basin_outline(t1, FALSE, returnImbie = NULL) |>
  #'   terra::lines(col = "red", lwd = 2)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Prepare all possible basin combinations
  meas   <- define_racmo_globals()$measures$basins
  immB   <- define_racmo_globals()$imbie$basins
  basins <- rbind(meas, immB)                      # MEaSURES is EPSG:3031

  # Search for exact basin names to return
  if (isFALSE(rectangularExtent)) {
    if (extent[[1]] != "") {
      if (isTRUE(returnImbie)) {
        basins <- immB[immB$NAME %in% extent]
      } else if (isFALSE(returnImbie)) {
        basins <- meas[meas$NAME %in% extent]
      } else if (is.null(returnImbie)) {
        basins <- basins[basins$NAME %in% extent]
      }
    }
  } else {
  # Define the required extent to search for basins within
    extent <- get_extent(extent = extent,
                         rectangularExtent = rectangularExtent,
                         preferType = preferType,
                         useOnly = useOnly,
                         imbieBasins = imbieBasins,
                         crs = use_crs("stereo"), # return 3031 to match shelves
                         crsIn = crsIn)

    # Establish which basins intersect with the extent
    if (isTRUE(returnImbie)) {
      basins   <- terra::intersect(immB, extent)
    } else if (isFALSE(returnImbie)) {
      basins   <- terra::intersect(meas, extent)
    } else if (is.null(returnImbie)) {
      basins   <- terra::intersect(basins, extent)
    }
  }

  # Reproject
  basins   <- terra::project(basins, use_crs(crs))

  return(basins)
}


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
