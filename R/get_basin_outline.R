get_basin_outline <- function(extent = "",
                              exactExtents = TRUE,
                              returnImbie = NULL,
                              rectangularExtent = TRUE,
                              preferType = NULL,
                              returnOnly = NULL,
                              imbieBasins = NULL,
                              crs = "racmo",
                              crsIn = NULL) {
  #' Return the MEaSURES basin outlines for a given extent
  #'
  #' @description This function is useful for plotting basins in Antarctica. It
  #'   is mainly used in the `draw_antarctica()` function, but works separately
  #'   as well. The basins can be reprojected using the "crs" argument,
  #'   defaulting to "racmo".
  #'
  #' @param extent Define the extent used to select basins If exactExtents is
  #'   TRUE, this extent must be a vector of basin names that will be returned
  #'   directly from the MEaSURES / IMBIE datasets. Make sure the names match.
  #'
  #'   If exactExtents is FALSE, the input is fed into `get_extents()` and the
  #'   other arguments become relevant. The most obvious reason to do this would
  #'   be to include the outline of basins within the bounding box of the named
  #'   basins, but that aren't named themselves. See examples.
  #' @param exactExtents BINARY: Should only the named basin/s in 'extent' be
  #'   included (TRUE), or  (FALSE) should 'extent' be fed into `get_extents()`?
  #'   The latter defines the extent and all basins that intersect with that
  #'   extent will be returned. See examples.
  #' @param returnImbie BINARY: By default (NULL), both IMBIE and MEaSURES
  #'   basins within the extent are returned. Set this to return only the IMBIE
  #'   (TRUE) or MEaSURES (FALSE) basins. This argument is distinct to
  #'   returnOnly, which is used by `get_extent()` to define the initial extent
  #'   to search for basins in.
  #' @param crs "string": Which projection should the basins be returned in?
  #' @inheritParams
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Prepare all possible basin combinations
  meas   <- define_racmo_globals()$measures$basins
  immB   <- define_racmo_globals()$imbie$basins
  basins <- rbind(meas, immB)                      # MEaSURES is EPSG:3031

  # Search for exact basin names to return
  if (isTRUE(exactExtents)) {
    if (isTRUE(returnImbie)) {
      basins <- immB[immB$NAME %in% extent]
    } else if (isFALSE(returnImbie)) {
      basins <- meas[meas$NAME %in% extent]
    } else if (is.null(returnImbie)) {
      basins <- basins[basins$NAME %in% extent]
    }
  } else {
  # Define the required extent to search for basins within
    extent <- get_extent(extent = extent,
                         rectangularExtent = rectangularExtent,
                         preferType = preferType,
                         returnOnly = returnOnly,
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
