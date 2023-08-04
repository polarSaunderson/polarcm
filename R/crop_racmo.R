crop_racmo <- function(racmoData, extent, rectangularExtent = TRUE, ...) {
  #' Crop RACMO data to a spatial extent
  #'
  #' @description This function is a simple wrapper around `terra::crop()` and
  #'   `racmoR::get_extent()` to spatially reduce a RACMO SpatRaster.
  #'
  #' @param racmoData SpatRaster: The RACMO data to crop. It must be an existing
  #'   SpatRaster; if not, use `crop_racmoD()` or `crop_racmoM()`.
  #' @param extent The area to crop the RACMO data to; fed directly into
  #'   `racmoR::get_extent()` so see there for details.
  #' @param rectangularExtent BINARY: Should the RACMO data be cropped to a
  #'   rectangular bounding box around the extent (TRUE), or to the actual
  #'   outline (e.g. shelf/basin) of the extent (FALSE)? The latter is achieved
  #'   by masking out pixels (setting their values as NA) if they do not contain
  #'   part of the extent outline.
  #' @param ... Any other arguments that can be fed into `get_extent()`.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  dataCrs <- terra::crs(racmoData) # projection of data that will be cropped
  extent  <- get_extent(extent,
                        rectangularExtent = rectangularExtent,
                        crs = dataCrs, ...) # return extent in data's projection

  croppedRacmo <- terra::crop(racmoData, extent, snap = "out")

  # If the data should be masked to the
  if (isFALSE(rectangularExtent)) {
    croppedRacmo <- terra::mask(croppedRacmo, extent,
                                touches = !rectangularExtent)
  }

  return(croppedRacmo)
}

##
crop_racmoD <- function(racmoData, extent, rectangularExtent = TRUE, ...) {
  #' Crop daily RACMO data to a spatial extent
  #'
  #' @description This function is a simple wrapper around
  #'   `racmoR::crop_racmo()` and `racmoR::read_racmoD_data()` to spatially crop
  #'   daily RACMO SpatRasters.
  #'
  #' @param racmoData SpatRaster: The RACMO data to crop. Can be either a
  #'   variable name, in which case raw daily RACMO data is read in; or an
  #'   existing SpatRaster of daily RACMO data.
  #' @param extent The area to crop the RACMO data to; fed directly into
  #'   `racmoR::get_extent()` so see there for details.
  #' @param rectangularExtent BINARY: Should the RACMO data be cropped to a
  #'   rectangular bounding box around the extent (TRUE), or to the actual
  #'   outline (e.g. shelf/basin) of the extent (FALSE)? The latter is achieved
  #'   by masking out pixels (setting their values as NA) if they do not contain
  #'   part of the extent outline.
  #' @param ... Any other arguments that can be fed into `get_extent()`.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData    <- read_racmoD_data(racmoData)
  croppedRacmo <- crop_racmo(racmoData, extent, rectangularExtent, ...)

  return(croppedRacmo)
}

##
crop_racmoM <- function(racmoData, extent, rectangularExtent = TRUE, ...) {
  #' Crop monthly RACMO data to a spatial extent
  #'
  #' @description This function is a simple wrapper around
  #'   `racmoR::crop_racmo()` and `racmoR::read_racmoM_data()` to spatially crop
  #'   monthly RACMO SpatRasters.
  #'
  #' @param racmoData SpatRaster: The RACMO data to crop. Can be either a
  #'   variable name, in which case raw monthly RACMO data is read in; or an
  #'   existing SpatRaster of monthly RACMO data.
  #' @param extent The area to crop the RACMO data to; fed directly into
  #'   `racmoR::get_extent()` so see there for details.
  #' @param rectangularExtent BINARY: Should the RACMO data be cropped to a
  #'   rectangular bounding box around the extent (TRUE), or to the actual
  #'   outline (e.g. shelf/basin) of the extent (FALSE)? The latter is achieved
  #'   by masking out pixels (setting their values as NA) if they do not contain
  #'   part of the extent outline.
  #' @param ... Any other arguments that can be fed into `get_extent()`.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  racmoData    <- read_racmoM_data(racmoData)
  croppedRacmo <- crop_racmo(racmoData, extent, rectangularExtent, ...)

  return(croppedRacmo)
}
