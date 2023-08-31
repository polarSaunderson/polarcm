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
  #'   3) an existing shelf SpatVector will be returned.
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
  #'   # Just a single shelf
  #'   t2 <- get_shelf_outline("Amery")
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
  token <- configure_polaR()

  # Prepare all possible ice shelves
  shelves <- token$measures$iceShelves   # MEaSURES is EPSG:3031

  # These must be rectangles, as there isn't enough extra information
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

  # Are we creating a rectangular extent, or using exact shelf outlines?
  if (isTRUE(rectangularExtent)) {
    # print("A rectangular extent is requested")
    extent <- get_extent(extent = extent,
                         rectangularExtent = TRUE,
                         preferType = preferType,
                         useOnly = useOnly,
                         imbieBasins = imbieBasins,
                         crs = use_crs("stereo"),# return 3031 to match shelves
                         crsIn = crsIn)

    # Establish which shelves fall within the given extent
    shelves   <- terra::crop(shelves, extent)
  } else {
    # print("The exact outlines are requested")
    if ("SpatVector" %in% methods::is(extent)) {
      # Print("input is a SpatVector; basically do nothing")
      shelves <- extent
    } else {
      # print("input is name/s; using exact shelves")
      shelves <- shelves[shelves$NAME %in% extent]
    }
  }

  # Reproject
  shelves   <- terra::project(shelves, use_crs(crs))
  return(shelves)
}


# TESTING  - uncomment a "tst" and the print + plot lines

# Blank = All of Antarctica
# tst <- get_shelf_outline("")
#
# SpatExtent
# tst <- get_shelf_outline(terra::ext(c(14, 24, 6, 11)),
#                          crsIn = use_crs("racmo"),
#                          rectangularExtent = FALSE)  # is overwritten to TRUE
#
# SpatVector
# tst <- get_shelf_outline(shelf, rectangularExtent = TRUE)
# tst <- get_shelf_outline(shelf, rectangularExtent = FALSE)
#
# SpatRaster
# sRaster <- terra::crop(ss, terra::ext(c(15, 20, 8, 12)))
# tst <- get_shelf_outline(sRaster, rectangularExtent = FALSE) # overwritten?
#
# # named
# tst <- get_shelf_outline("Shackleton", rectangularExtent = FALSE)
# tst <- get_shelf_outline("Shackleton", rectangularExtent = TRUE)
# tst <- get_shelf_outline(c("Amery", "West"), rectangularExtent = TRUE)
# tst <- get_shelf_outline(c("Amery", "West"), rectangularExtent = FALSE)
#
# print(tst)
# terra::plot(tst)


# =============================================================================!
# SCRAP CODE





  # # Must be rectangles, as there isn't enough extra information
  # if ("SpatRaster" %in% methods::is(extent) | extent[[1]] == "") {
  #   print("SpatRaster")
  #   rectangularExtent <- TRUE
  # }
  #
  # # The extent is already a shelf, so just return it
  # if ("SpatVector" %in% methods::is(extent)) {
  #   print("SpatVector")
  #   shelves <- extent
  # } else if (isFALSE(rectangularExtent)) {
  # # use specific shelf names for exact matches
  #   print("rectangular is FALSE")
  #   shelves <- shelves[shelves$NAME %in% extent]
  # }
  #
  # if (isTRUE(rectangularExtent)) {
  #   print("rectangular is TRUE")
  #   extent <- get_extent(extent = extent,
  #                        rectangularExtent = rectangularExtent, # will be TRUE
  #                        preferType = preferType,
  #                        useOnly = useOnly,
  #                        imbieBasins = imbieBasins,
  #                        crs = use_crs("stereo"), # return 3031 to match shelves
  #                        crsIn = crsIn)
  #
  #   # Establish which shelves intersect with the extent
  #   shelves   <- terra::intersect(shelves, extent)
  # } else {
  #   # use specific shelf names for exact matches
  #   print("rectangular is FALSE")
  #   shelves <- shelves[shelves$NAME %in% extent]
  # }



  # # Search for the exact shelf names to return
  # if (isFALSE(rectangularExtent)) {
  #   print("not rectangular")
  #   # print("here"); return(shelves)
  #   if (extent[[1]] != "") {
  #       print("not empty")
  #     if ("SpatVector" %in% methods::is(extent)) {
  #       print("SpatVector")
  #       shelves <- extent
  #     } else {
  #       print("named")
  #       shelves <- shelves[shelves$NAME %in% extent]
  #     }
  #   }
  # } else {
  # # Define the required extent to search for shelves within
  #   extent <- get_extent(extent = extent,
  #                        rectangularExtent = rectangularExtent,
  #                        preferType = preferType,
  #                        useOnly = useOnly,
  #                        imbieBasins = imbieBasins,
  #                        crs = use_crs("stereo"), # return 3031 to match shelves
  #                        crsIn = crsIn)
  #
  #   # Establish which shelves intersect with the extent
  #   shelves   <- terra::intersect(shelves, extent)
  # }




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
