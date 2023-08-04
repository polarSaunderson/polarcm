get_extent <- function(extent = "",
                       rectangularExtent = FALSE,
                       preferType = NULL,
                       useOnly = NULL,
                       imbieBasins = NULL,
                       crs = "racmo",
                       crsIn = NULL) {
  #' Return the extent of different Antarctic boundaries
  #'
  #' @description This function helps us to very easily crop and/or plot data
  #'   for parts of Antarctica. It is very flexible in what it accepts, but is
  #'   particularly useful when entering MEaSURES ice shelf or basin names.
  #'   However, it can also handle other SpatRasters, SpatVectors, or
  #'   SpatExtents, allowing it to be used in different contexts. The extent can
  #'   be returned in different projections via the 'crs' argument. The returned
  #'   extent can be either exact shape outlines or a rectangular SpatExtent.
  #'
  #' @param extent Multiple types are allowed as input.
  #'
  #'   If 'extent' is an empty string (i.e. "", the default), the whole of
  #'   Antarctica is returned.
  #'
  #'   If 'extent' is a defined string (e.g. "Shackleton"), basins and ice shelves
  #'   in the MEaSURES dataset with those names are returned; if the same name
  #'   refers to both an ice shelf and a basin, 'preferType' determines which is
  #'   returned.
  #'
  #'   A vector of multiple ice shelf/basin names can also be used at once; if
  #'   that vector contains both ice shelves and basins but only one type is
  #'   required, use the 'useOnly' argument.
  #'
  #'   If a defined string, by default the exact outline of the ice shelf or
  #'   basin is returned, but 'rectangularExtent' can be set for a bounding box
  #'   instead.
  #'
  #'   If 'extent' is a SpatRaster or SpatVector, then the extent of the
  #'   SpatRaster or SpatVector will be returned.
  #'
  #'   If 'extent' is a SpatExtent, it is necessary to also set 'crsIn', and a
  #'   SpatExtent will be returned.
  #'
  #' @param rectangulerExtent If 'extent' is a defined string (e.g. "Amery"), the
  #'   'rectangulerExtent' argument determines whether the actual outline is
  #'   returned (FALSE; the default), or if a rectangular extent box
  #'   encompassing the area is returned (TRUE). If multiple ice shelves and/or
  #'   basins are included, the extent box will extend over all of them at once.
  #'
  #' @param imbieBasins BINARY: Should the IMBIE basins (TRUE) or the MEaSURES
  #'   basins (FALSE) be used to define the extent? The IMBIE basins have codes
  #'   (e.g. "A-Ap") and are larger regions; the MEaSURES basins have names
  #'   (that can overlap with the ice shelf names), and are smaller "sub" regions.
  #'   If NULL (the default), both are returned. This is mainly useful if there
  #'   is a vector containing both, but only one should be used. See also
  #'   'useOnly'. Distinct from the 'returnImbie' argument used in similar
  #'   functions such as `get_shelf_outline()` and `get_basin_outline()`.
  #'
  #' @param preferType Sometimes an ice shelf and the MEaSURES basins have the
  #'   same name; usually they both refer to the same general area, and by
  #'   default (i.e. 'preferType' = NULL), both are combined to form a new larger
  #'   extent encompassing both. However, for some names (e.g. "Shackleton" and
  #'   "Drygalski"), the name refers to an ice shelf far away from the basin
  #'   with the same name, returning an unexpected extent.
  #'
  #'   This argument lets the user choose between the two types if the name is
  #'   found in both the ice shelves and basins datasets: "shelf", "shelves",
  #'   "floating" or "FL" to return the ice shelves; "basin", "basins",
  #'   "grounded" or "GR" to return the basins. All other names that aren't in
  #'   both the ice shelves and basins datasets will be returned regardless of
  #'   this option. See also 'useOnly'.
  #'
  #' @param useOnly If there is a vector containing both ice shelf and basin
  #'   names, but only one or the other should be included, use this argument.
  #'   Ice shelves or basins are specified as outlined in the 'preferType'
  #'   argument.
  #'
  #' @param crs "string": Which crs should the extent be returned in? See
  #'   `use_crs()` or `terra::crs()`.
  #'
  #' @param crsIn "string": Which projection is the extent given in? Needs
  #'   defining if the extent is a SpatExtent, as they do not have a crs value
  #'   attached.
  #'
  #' @examples -----------------------------------------------------------------
  #'   # Full Antarctic extent
  #'   t1 <- get_extent()
  #'   terra::plot(t1)
  #'
  #'   # Single shelf
  #'   t2 <- get_extent("Amery")
  #'   terra::plot(t2)
  #'
  #'   # Single basin
  #'   t3 <- get_extent("Dry Valleys")
  #'   terra::plot(t3)
  #'
  #'   # Name appears in both the ice shelf and basins datasets
  #'   t4 <- get_extent("Shackleton")
  #'   terra::plot(t4)
  #'
  #'   # Extent covers both the ice shelf and basin
  #'   t5 <- get_extent("Shackleton", rectangularExtent = TRUE)
  #'   terra::plot(t5)
  #'
  #'   # Prefer the shelf
  #'   t6 <- get_extent("Shackleton", preferType = "shelf")
  #'   terra::plot(t6)
  #'
  #'   # Prefer the basin
  #'   t7 <- get_extent("Shackleton", preferType = "basin")
  #'   terra::plot(t7)
  #'
  #'   # Multiple shelves and basins
  #'   t8 <- get_extent(c("Amery", "Shackleton", "West", "Dry Valleys"),
  #'                    preferType = "NULL")
  #'   terra::plot(t8)
  #'
  #'   # Multiple shelves & basins, prefer the shelves: no Shackleton basin
  #'   t9 <- get_extent(c("Amery", "Shackleton", "West", "Dry Valleys"),
  #'                    preferType = "shelf")
  #'   terra::plot(t9)
  #'
  #'   # Multiple shelves & basins; useOnly shelves
  #'   # no Shackleton & Dry Valleys basins
  #'   t10 <- get_extent(c("Amery", "Shackleton", "West", "Dry Valleys"),
  #'                     useOnly = "shelf")
  #'   terra::plot(t10)
  #'
  #'   # Multiple shelves & basins; useOnly basins
  #'   # no Shackleton, Amery & West shelves
  #'   t11 <- get_extent(c("Amery", "Shackleton", "West", "Dry Valleys"),
  #'                     useOnly = "basins")
  #'   terra::plot(t11)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoInfo  <- define_racmo_globals()
  extentType <- is(extent)

  # Output depends on extent type
  if (length(intersect(extentType, c("SpatRaster", "SpatVector"))) > 0) {
  # Extent of the SpatRaster or SpatVector
    xBounds <- terra::project(extent, use_crs(crs))
    xBounds <- terra::ext(xBounds)
  } else if ("SpatExtent" %in% extentType) {
  # Extent of the SpatExtent
    xBounds <- terra::rast(extent, crs = use_crs(crsIn)) |> # rast to reproject
      terra::project(use_crs(crs)) |>                       # do reproject
      terra::ext()                                          # reprojected extent
  } else if (extent[[1]] == "") {
  # Extent for all of Antarctica
    xBounds <- terra::project(racmoInfo$measures$coasts,
                              use_crs(crs))
    xBounds <- terra::ext(xBounds)
  } else {
  # Extent for defined shelves or basins
    shelfSynonyms <- c("shelf", "Shelf", "shelves", "Shelves", "float",
                       "FL", "floating", "floating ice", "ice shelves")
    basinSynonyms <- c("basin", "Basin", "basins", "Basins",
                       "GR", "grounded", "grounded ice", "land ice", "land")

    # Find name/s of the extent's ice shelves name/s in the MEaSURES datasets
    xShelves <- racmoInfo$measures$shelves[racmoInfo$measures$shelves$NAME %in% extent]

    # Find name/s of the extent's basins name/s in the MEaSURES datasets
    if (isTRUE(imbieBasins)) {
      xBasins  <- racmoInfo$imbie$basins[racmoInfo$imbie$basins$NAME %in% extent]
    } else if (isFALSE(imbieBasins)) {
      xBasins  <- racmoInfo$measures$basins[racmoInfo$measures$basins$NAME %in% extent]
    } else if (is.null(imbieBasins)) {
      immB <- racmoInfo$imbie$basins[racmoInfo$imbie$basins$NAME %in% extent]
      meas <- racmoInfo$measures$basins[racmoInfo$measures$basins$NAME %in% extent]
      xBasins <- rbind(immB, meas)
    }

    # useOnly shelves or useOnly basins from the input list?
    if (!is.null(useOnly)) {
      if (useOnly %in% shelfSynonyms) {
        xBounds <- xShelves
      } else if (useOnly %in% basinSynonyms) {
        xBounds <- xBasins
      }
    } else {
    # Or is there a preference if named in both, but all others are needed too?
      if (!is.null(preferType)) {
        # Prefer the shelf or basin if both are there?
        inBoth <- intersect(xBasins$NAME, xShelves$NAME)
        if (length(inBoth) > 0) {
          if (preferType %in% shelfSynonyms) {
            # remove any basins that match a shelf name
            xBasins <- xBasins[xBasins$NAME %notIn% inBoth]
          } else if (preferType %in% basinSynonyms) {
            # remove any shelves that match a basin name
            xShelves <- xShelves[xShelves$NAME %notIn% inBoth]
          }
        }
      }
      # Combine shelves & basins to return all
      if (nrow(xShelves) > 0 & nrow(xBasins) > 0) {
        xBounds <- terra::vect(list(xShelves, xBasins))
      } else if (nrow(xShelves) > 0) {
        xBounds <- xShelves
      } else if (nrow(xBasins) > 0) {
        xBounds <- xBasins
      }
    }

    # Does anything actually match the criteria?
    if (nrow(xBounds) == 0) {
      stop("Nothing matched your search criteria!")
    }

    # reproject then bounding box, as the other way around doesn't work

    # Reproject
    xBounds <- terra::project(xBounds, use_crs(crs))

    # Bounding box?
    if (isTRUE(rectangularExtent)) {
      xBounds <- terra::ext(xBounds)
    }
  }

  return(xBounds)
}



# EXAMPLES TO BUILD IN & DOCUMENT PROPERLY
# # All of Antarctica
# terra::plot(get_extent(""), main = "t1")
# terra::lines(get_extent("", rectangularExtent = TRUE), col = "red", lwd = 2)
#
# # Single shelf
# terra::plot(get_extent("Amery"), main = "t2")
# terra::lines(get_extent("Amery", rectangularExtent = TRUE), col = "red", lwd = 2)
#
# # Single basin - IMBIE Basin Names
# terra::plot(get_extent("A-Ap"), main = "t3")
# terra::lines(get_extent("A-Ap", rectangularExtent = TRUE), col = "red", lwd = 2)
#
# # Single basin - MEaSURES Basin Names
# terra::plot(get_extent("Vincennes_Bay"), main = "t4")
# terra::lines(get_extent("Vincennes_Bay", rectangularExtent = TRUE), col = "red", lwd = 2)
#
# # Some names appear as both a basin and a shelf
# terra::plot(get_extent("Shackleton"), main = "t5")
# terra::lines(get_extent("Shackleton", rectangularExtent = TRUE), col = "red", lwd = 2)
#
# # Use preferType for a shelf or basin preference if name is duplicated
# terra::plot(get_extent("Shackleton"), main = "t6")
# terra::lines(get_extent("Shackleton", preferType = "shelves"), col = "red", lwd = 2)
# terra::lines(get_extent("Shackleton", preferType = "basins"), col = "blue", lwd = 2)
#
# # Define extent using multiple shelves
# terra::plot(get_extent(c("Amery", "West")), main = "t7")
# terra::lines(get_extent(c("Amery", "West"), rectangularExtent = TRUE), col = "red", lwd = 2)
#
# # Define extent using multiple shelves and basins together
# terra::plot(get_extent(c("Amery", "West", "Publications")), main = "t8")
# terra::lines(get_extent(c("Amery", "West", "Publications"), rectangularExtent = TRUE), col = "red", lwd = 2)
#
# # Use only shelves or basins if both are in the vector
# terra::plot(get_extent(c("Amery", "West", "Publications")), main = "t9")
# terra::lines(get_extent(c("Amery", "West", "Publications"), useOnly = "shelves"), col = "red")
# terra::lines(get_extent(c("Amery", "West", "Publications"), useOnly = "basins"), col = "blue")
#
# # Basins names are defined according to IMBIE or MEaSURES by default
# terra::plot(get_extent(c("A-Ap", "Vincennes_Bay", "Dry Valleys")), main = "t10")
#
# # Use only IMBIE or MEaSURES basins if both are in the vector
# terra::plot(get_extent(c("A-Ap", "Vincennes_Bay", "Dry Valleys")), main = "t11")
# terra::lines(get_extent(c("A-Ap", "Vincennes_Bay", "Dry Valleys"), imbieBasins = TRUE), col = "red")
# terra::lines(get_extent(c("A-Ap", "Vincennes_Bay", "Dry Valleys"), imbieBasins = FALSE), col = "blue")
