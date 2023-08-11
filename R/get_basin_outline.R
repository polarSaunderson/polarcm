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
  # Prepare all possible basin combinations; basins are stored as EPSG:3031
  if (isTRUE(returnImbie)) {
    basins <- define_racmo_globals()$imbie$basins
  } else if (isFALSE(returnImbie)) {
    basins <- define_racmo_globals()$measures$basins
  } else if (is.null(returnImbie)) {
    basins <- rbind(define_racmo_globals()$imbie$basins,
                    define_racmo_globals()$measures$basins)
  }

  # Must be rectangles, as there isn't enough extra information
  if ("SpatRaster" %in% is(extent)) {
     # print("SpatRaster")
    rectangularExtent <- TRUE
  } else if ("SpatExtent" %in% is(extent)) {
     # print("SpatExtent")
    rectangularExtent <- TRUE
  } else if ("SpatVector" %in% is(extent)) {
     # print("SpatVector")  # necessary so the next one is skipped
  } else if (extent[[1]] == "") {
     # print("blank")
    rectangularExtent <- TRUE
  }

  if (isTRUE(rectangularExtent)) {
     # print("rectangular requested")
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
     # print("exact requested")
    if ("SpatVector" %in% is(extent)) {
       # print("SpatVector")
      basins <- extent
    } else {
       # print("named")
      basins <- basins[basins$NAME %in% extent]
    }
    # Reproject
    basins   <- terra::project(basins, use_crs(crs))
  }
  # print(basins)
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
