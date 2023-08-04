get_shelf_outline <- function(extent = "",
                              exactExtents = TRUE,
                              rectangularExtent = TRUE,
                              preferType = NULL,
                              returnOnly = NULL,
                              imbieBasins = NULL,
                              crs = "racmo",
                              crsIn = NULL) {
  #' Return the MEaSURES shelf outlines for a given extent
  #'
  #' @description This function is useful for plotting ice shelves in
  #'   Antarctica. It is mainly used in the `draw_antarctica()` function, but
  #'   works separately as well. The ice shelves can be reprojected using the
  #'   "crs" argument, defaulting to "racmo".
  #'
  #' @param extent Define the extent used to select ice shelves. If exactExtents
  #'   is TRUE, this extent must be a vector of ice shelf names that will be
  #'   returned directly from the MEaSURES dataset. Make sure the names match.
  #'
  #'   If exactExtents is FALSE, the input is fed into `get_extents()` and the
  #'   other arguments become relevant. The most obvious reason to do this would
  #'   be to include the outline of shelves within the bounding box of the named
  #'   shelves, but that aren't named themselves. See examples.
  #' @param exactExtents BINARY: Should only the named shelf/shelves in 'extent'
  #'   be included (TRUE), or  (FALSE) should 'extent' be fed into
  #'   `get_extents()`? The latter defines the extent and all ice shelves that
  #'   intersect with that extent will be returned. See examples.
  #' @param crs "string": Which projection should the ice shelves be returned
  #'   in?
  #' @inheritParams
  #'
  #' @examples
  #'   # All ice shelves in Antarctica
  #'   t1 <- get_shelf_outline()
  #'   terra::plot(t1)
  #'
  #'   # Just on shelf  t2 <- get_shelf_outline("Amery")
  #'   terra::plot(t2)
  #'
  #'   # Multiple shelves; only the named shelves
  #'   t3 <- get_shelf_outline(c("Amery", "West"), exactExtents = TRUE)
  #'   terra::plot(t3)
  #'
  #'   # Multiple shelves; includes intermediary shelves
  #'   t4 <- get_shelf_outline(c("Amery", "West"), exactExtents = FALSE)
  #'   terra::plot(t4)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Prepare all possible ice shelves
  shelves <- define_racmo_globals()$measures$shelves   # MEaSURES is EPSG:3031

  # Search for the exact shelf names to return
  if (isTRUE(exactExtents)) {
    shelves <- shelves[shelves$NAME %in% extent]
  } else {
  # Define the required extent to search for shelves within
    extent <- get_extent(extent = extent,
                         rectangularExtent = rectangularExtent,
                         preferType = preferType,
                         returnOnly = returnOnly,
                         imbieBasins = imbieBasins,
                         crs = use_crs("stereo"), # return 3031 to match shelves
                         crsIn = crsIn)

    # Establish which shelves intersect with the extent
    shelves   <- terra::intersect(shelves, extent)
  }

  # Reproject
  shelves   <- terra::project(shelves, use_crs(crs))

  return(shelves)
}



# EXAMPLES: NEED TO BE PROPERLY INCLUDED IN THE DOCUMENTATION
#
# # Single shelves
# t1 <- "Getz"
# get_shelf_outline(t1) |> terra::plot(col = "black")
# get_shelf_outline(t1, FALSE, returnOnly = "shelves") |>
#   terra::lines(col = "red", lwd = 2)
#
# t2 <- "Shackleton"
# get_shelf_outline(t2) |> terra::plot(col = "black")
# get_shelf_outline(t2, FALSE, returnOnly = "shelves") |>
#   terra::lines(col = "red", lwd = 2)
#
# # Multiple shelves
# t3 <- c("Amery", "West")
# get_shelf_outline(t3) |> terra::plot(col = "black")
# get_shelf_outline(t3, FALSE) |> terra::lines(col = "red", lwd = 2)
#
# # Be careful with names still
# t3 <- "Drygalski"
# get_shelf_outline(t3) |> terra::plot(col = "black")
# get_shelf_outline(t3, FALSE) |> terra::lines(col = "red", lwd = 2)
# get_shelf_outline(t3, FALSE) |> terra::plot()
#
# # Basins can be used to define the extent, and shelves returned
# t4 <- "A-Ap"
# get_shelf_outline(t4, FALSE) |> terra::plot()
