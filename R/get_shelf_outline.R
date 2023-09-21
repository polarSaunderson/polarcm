get_shelf_outline <- function(extent = "",
                              rectangularExtent = FALSE,
                              crs = NULL,
                              preferType = NULL,
                              useOnly = NULL,
                              imbieBasins = NULL,
                              crsIn = NULL) {
  #' Return ice shelf outlines for a given extent
  #'
  #' @description This function is useful for plotting or cropping to Antarctic
  #'   ice shelves. It is mainly used in the [draw_antarctica()] and
  #'   [crop_to_shelf()] functions, but can be used separately as well.
  #'
  #'   The function works in 2 ways.
  #'
  #'   1) A vector of shelf names can be entered as the 'extent', and these
  #'   exact ice shelves are simply returned from the MEaSURES ice shelf
  #'   dataset. The ice shelf names must match those in the dataset exactly.
  #'
  #'   2) An extent can be defined according to [get_extent()]. Any shelves
  #'   which intersect with this extent are then returned. This occurs if
  #'   'rectangularExtent' is TRUE, and most of the remaining arguments are fed
  #'   directly into [get_extent()].
  #'
  #'   The ice shelves will be returned in the projection specified by the 'crs'
  #'   argument.
  #'
  #'   **Note:** If an existing ice shelf SpatVector is entered, it will be
  #'   returned.
  #'
  #' @param extent Define the extent used to select ice shelves.
  #'
  #'   To return only specific ice shelves, enter their names as a vector, and
  #'   set 'rectangularExtents' as FALSE (the default).
  #'
  #'   To return any ice shelves that fall within a geographical extent, set
  #'   'rectangularExtents' as TRUE, and follow the logic of [get_extent()] to
  #'   define the geographical extent with the remaining parameters. The most
  #'   obvious reason to do this would be to include the outline of ice shelves
  #'   within the bounding box of the named ice shelves, but that aren't named
  #'   themselves. See examples.
  #'
  #' @param crs "string": Which projection should the shelves be returned in?
  #'   See [polarcm::use_crs()] or [terra::crs()]. By default (i.e. NULL), it
  #'   will match the first RCM data defined in the ".Rprofile".
  #'
  #' @inheritParams get_extent
  #'
  #' @examples -----------------------------------------------------------------
  #' \dontrun{
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
  #' }
  #' @export

  # Code -----------------------------------------------------------------------
  # Handle default CRS
  token  <- configure_polarcm()
  crs    <- set_if_null(crs, token$defaults$grid$crs)
  crs    <- use_crs(crs)

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
