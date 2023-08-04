get_shelf_outline <- function(extent = "",
                              rectangularExtent = FALSE,
                              crs = "racmo",
                              preferType = NULL,
                              useOnly = NULL,
                              imbieBasins = NULL,
                              crsIn = NULL) {
  #' Return ice shelf outlines for a given extent
  #'
  #' @description This function is useful for plotting ice shelves in
  #'   Antarctica. It is mainly used in the `crop_racmo...()` and
  #'   `draw_antarctica()` functions.
  #'
  #'   The function works in two ways.
  #'
  #'   1) a vector of shelf names can be entered as the 'extent', and these
  #'   shelves are simply returned from the MEaSURES ice shelf dataset. The
  #'   names must match the dataset exactly.
  #'
  #'   2) an extent can be defined according to `get_extent()`. Any shelves
  #'   which intersect with this extent are then returned. This occurs if
  #'   'rectangularExtent' is TRUE, and most of the remaining arguments are fed
  #'   directly into `get_extent()`.
  #'
  #'   Finally, the shelves can be reprojected (set via 'crs').
  #'
  #' @param extent Define the extent used to select ice shelves.
  #'
  #'   To return only specific ice shelves, enter their names as a vector, and
  #'   set 'rectangularExtents' as FALSE (the default).
  #'
  #'   To return any ice shelves that fall within a geographical extent, set
  #'   'rectangularExtents' as TRUE, and follow the logic of `get_extents()` to
  #'   define the geographical extent. The most obvious reason to do this would
  #'   be to include the outline of ice shelves within the bounding box of the
  #'   named ice shelves, but that aren't named themselves. See examples.
  #'
  #' @param crs "string": Which projection should the shelves be returned in?
  #' @inheritParams get_extent
  #'
  #' @examples -----------------------------------------------------------------
  #'   # All ice shelves in Antarctica
  #'   t1 <- get_shelf_outline()
  #'   terra::plot(t1)
  #'
  #'   # Just on shelf  t2 <- get_shelf_outline("Amery")
  #'   terra::plot(t2)
  #'
  #'   # Multiple shelves; only the named shelves
  #'   t3 <- get_shelf_outline(c("Amery", "West"), rectangularExtent = FALSE)
  #'   terra::plot(t3)
  #'
  #'   # Multiple shelves; includes intermediary shelves
  #'   t4 <- get_shelf_outline(c("Amery", "West"), rectangularExtent = TRUE)
  #'   terra::plot(t4)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Prepare all possible ice shelves
  shelves <- define_racmo_globals()$measures$shelves   # MEaSURES is EPSG:3031

  # Search for the exact shelf names to return
  if (isFALSE(rectangularExtent)) {
    if (extent[[1]] != "") {
      shelves <- shelves[shelves$NAME %in% extent]
    }
  } else {
  # Define the required extent to search for shelves within
    extent <- get_extent(extent = extent,
                         rectangularExtent = rectangularExtent,
                         preferType = preferType,
                         useOnly = useOnly,
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
# get_shelf_outline(t1, FALSE, useOnly = "shelves") |>
#   terra::lines(col = "red", lwd = 2)
#
# t2 <- "Shackleton"
# get_shelf_outline(t2) |> terra::plot(col = "black")
# get_shelf_outline(t2, FALSE, useOnly = "shelves") |>
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


# # TESTING ====================================================================
# sTst <- list("s1" = list("extent" = ""),
#              "s2" = list("extent" = "Amery"),
#              "s3" = list("extent" = c("Amery", "West"), "rectangularExtent" = TRUE),
#              "s4" = list("extent" = c("Amery", "West"), "rectangularExtent" = FALSE),
#              "s5" = list("extent" = c("Amery", "A-Ap", "West"),
#                          "rectangularExtent" = TRUE),
#              "s6" = list("extent" = c("Amery", "A-Ap", "West"),
#                          "rectangularExtent" = FALSE),
#              "s7" = list("extent" = c("Amery", "A-Ap", "West"),
#                          "rectangularExtent" = TRUE, "useOnly" = "basins"),
#              "s8" = list("extent" = c("Amery", "A-Ap", "West"),
#                          "rectangularExtent" = TRUE, "useOnly" = "shelves"),
#              "s9" = list("extent" = c("Amery", "A-Ap", "West", "Leppard"),
#                           "rectangularExtent" = TRUE, "imbieBasins" = NULL),
#              "s10" = list("extent" = c("Amery", "A-Ap", "West", "Leppard"),
#                           "rectangularExtent" = TRUE, "imbieBasins" = TRUE),
#              "s11" = list("extent" = c("Amery", "A-Ap", "West", "Leppard"),
#                           "rectangularExtent" = TRUE, "imbieBasins" = FALSE))
# tt <- sTst
#
# for (ii in seq_along(tt)) {
#   iiLines <- do.call(get_coastline, tt[[ii]])
#   terra::plot(iiLines, main = names(tt)[ii])
#
#   # iiLines <- do.call(get_grounding_line, tt[[ii]])
#   # terra::lines(iiLines, col = "red")
#
#   iiLines <- do.call(get_shelf_outline, tt[[ii]])
#   terra::lines(iiLines, col = "red")
# }
#
# bTst <- list("b1" = list("extent" = ""),
#              "b2" = list("extent" = "Dry Valleys"),
#              "b3" = list("extent" = c("A-Ap", "Dry Valleys"),
#                          "rectangularExtent" = FALSE),
#              "b4" = list("extent" = c("A-Ap", "Dry Valleys"),
#                          "rectangularExtent" = TRUE),
#              "b9" = list("extent" = c("Amery", "A-Ap", "West", "Leppard"),
#                          "rectangularExtent" = TRUE, "returnImbie" = TRUE),
#              "b12" = list("extent" = c("Amery", "A-Ap", "West", "Leppard"),
#                          "rectangularExtent" = TRUE, "returnImbie" = FALSE),
#              "b13" = list("extent" = c("Amery", "A-Ap", "West", "Leppard"),
#                          "rectangularExtent" = TRUE, "returnImbie" = NULL))
#
# tt <- bTst
#
# for (ii in seq_along(tt)) {
#   print(ii)
#   iiLines <- do.call(get_basin_outline, tt[[ii]])
#   terra::plot(iiLines, col = "red", main = names(tt)[ii])
#   cat(ii, "done \n")
# }
