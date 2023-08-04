crop_racmo_to_shelf <- function(racmoData, shelf) {
  #' Crop RACMO data to an Antarctic ice shelf
  #'
  #' @description Spatially crop RACMO SpatRasters. 'shelf' must be an exact
  #'   match for a name in the MEaSURES dataset. Multiple shelves can be used in
  #'   a vector. The data returned is for a bounding box around the shelves; use
  #'   `mask_racmo_to_shelf()` for "cropping" tightly to the actual shelf
  #'   outline.
  #'
  #' @param racmoData SpatRaster: The RACMO data to crop. It must be an existing
  #'   SpatRaster.
  #' @param shelf "string": The EXACT name of the ice shelves to crop the RACMO
  #'   data to. If a vector with multiple shelves is input, non-shelf pixels in
  #'   the bounding box between those shelves are also included.
  #'
  #' @seealso mask_racmo_to_shelf()
  #' @seealso crop_racmo_to_basin()
  #'
  #' @examples -----------------------------------------------------------------
  #'   dontrun{
  #'     x <- subset_racmoM_by_summer("precip", 1991) |>
  #'       subset_racmoM_by_month(12)
  #'
  #'     # Single shelf
  #'     xCrop <- crop_racmo_to_shelf(x, "Amery")
  #'     terra::plot(xCrop)
  #'
  #'     # Multiple shelves
  #'     xCrop2 <- crop_racmo_to_shelf(x, c("Amery", "Shackleton"))
  #'     terra::plot(xCrop2)
  #'   }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify shelf
  dataCrs <- terra::crs(racmoData)
  shelf   <- get_shelf_outline(extent = shelf, rectangularExtent = FALSE,
                               crs = dataCrs)  # shelves are in 3031; reprojects

  # Crop
  croppedRacmo <- terra::crop(racmoData, shelf, snap = "out")

  # Add data back that is lost in cropping
  terra::varnames(croppedRacmo)  <- terra::varnames(racmoData)[[1]]
  terra::longnames(croppedRacmo) <- terra::longnames(racmoData)[[1]]
  terra::units(croppedRacmo)     <- terra::units(racmoData)[[1]]
  names(croppedRacmo)            <- names(racmoData)

  return(croppedRacmo)
}

mask_racmo_to_shelf <- function(racmoData, shelf) {
  #' Mask RACMO data to an Antarctic ice shelf
  #'
  #' @description Spatially mask RACMO SpatRasters. 'shelf' must be an exact
  #'   match for a name in the MEaSURES dataset. Multiple shelves can be used in
  #'   a vector. The data returned is for the actual outline around the shelves;
  #'   use `crop_racmo_to_shelf()` for a rectangular bounding box.
  #'
  #' @param racmoData SpatRaster: The RACMO data to mask. It must be an existing
  #'   SpatRaster.
  #' @param shelf "string": The EXACT name of the ice shelves to mask the
  #'   RACMO data to. If multiple are used, non-shelf pixels in the bounding box
  #'   are masked out.
  #'
  #' @seealso crop_racmo_to_shelf()
  #' @seealso mask_racmo_to_basin()
  #'
  #' @examples -----------------------------------------------------------------
  #'   dontrun{
  #'     x <- subset_racmoM_by_summer("precip", 1991) |>
  #'       subset_racmoM_by_month(12)
  #'
  #'     # Single shelf
  #'     xMask <- mask_racmo_to_shelf(x, "Amery")
  #'     terra::plot(xMask)
  #'
  #'     # Multiple shelves
  #'     xMask2 <- mask_racmo_to_shelf(x, c("Amery", "Shackleton"))
  #'     terra::plot(xMask2)
  #'   }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify shelf
  dataCrs <- terra::crs(racmoData)
  shelf   <- get_shelf_outline(extent = shelf, rectangularExtent = FALSE,
                               crs = dataCrs)  # shelves are in 3031; reprojects

  # Crop
  croppedRacmo <- terra::crop(x = racmoData, y = shelf, snap = "out")
  maskedRacmo  <- terra::mask(x = croppedRacmo, mask = shelf, touches = TRUE, )

  # Add data back that is lost in cropping
  terra::varnames(maskedRacmo)  <- terra::varnames(racmoData)[[1]]
  terra::longnames(maskedRacmo) <- terra::longnames(racmoData)[[1]]
  terra::units(maskedRacmo)     <- terra::units(racmoData)[[1]]
  names(maskedRacmo)            <- names(racmoData)

  return(maskedRacmo)
}

crop_racmo_to_basin <- function(racmoData, basin) {
  #' Crop RACMO data to an Antarctic ice shelf
  #'
  #' @description Spatially mask RACMO SpatRasters. 'basin' must be an exact
  #'   match for a name in the MEaSURES or IMBIE dataset. Multiple basins can be
  #'   used in a vector. The data returned is for a bounding box around the
  #'   shelves; use `mask_racmo_to_basin()` for "cropping" tightly to the actual
  #'   basin outline.
  #'
  #' @param racmoData SpatRaster: The RACMO data to crop. It must be an existing
  #'   SpatRaster.
  #' @param basin "string": The EXACT name of the basins to crop the RACMO
  #'   data to. If a vector with multiple basins is input, non-shelf pixels in
  #'   the bounding box between those basin are also included.
  #'
  #' @seealso mask_racmo_to_basin()
  #' @seealso crop_racmo_to_shelf()
  #'
  #' @examples -----------------------------------------------------------------
  #'   dontrun{
  #'     x <- subset_racmoM_by_summer("precip", 1991) |>
  #'       subset_racmoM_by_month(12)
  #'
  #'     # Single basin
  #'     xCrop <- crop_racmo_to_basin(x, "A-Ap")
  #'     terra::plot(xCrop)
  #'
  #'     # Multiple basins
  #'     xCrop2 <- crop_racmo_to_basin(x, c("A-Ap", "Dry Valleys"))
  #'     terra::plot(xCrop2)
  #'   }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify basin
  dataCrs <- terra::crs(racmoData)
  basin   <- get_basin_outline(extent = basin, rectangularExtent = FALSE,
                               crs = dataCrs)  # shelves are in 3031; reprojects

  # Crop
  croppedRacmo <- terra::crop(racmoData, basin, snap = "out")

  # Add data back that is lost in cropping
  terra::varnames(croppedRacmo)  <- terra::varnames(racmoData)[[1]]
  terra::longnames(croppedRacmo) <- terra::longnames(racmoData)[[1]]
  terra::units(croppedRacmo)     <- terra::units(racmoData)[[1]]
  names(croppedRacmo)            <- names(racmoData)

  return(croppedRacmo)
}

mask_racmo_to_basin <- function(racmoData, basin) {
  #' Mask RACMO data to an Antarctic basin
  #'
  #' @description Spatially mask RACMO SpatRasters. 'basin' must be an exact
  #'   match for a name in the MEaSURES or IMBIE dataset. Multiple basins can be
  #'   used in a vector. The data returned is for the actual outline around the
  #'   basins; use `crop_racmo_to_basin()` for a rectangular bounding box.
  #'
  #' @param racmoData SpatRaster: The RACMO data to mask. It must be an existing
  #'   SpatRaster.
  #' @param basin "string": The EXACT name of the basins to mask the RACMO data
  #'   to. If multiple are used, non-basins pixels in the bounding box are also
  #'   masked out.
  #'
  #' @seealso crop_racmo_to_basin()
  #' @seealso mask_racmo_to_shelf()
  #'
  #' @examples -----------------------------------------------------------------
  #'   dontrun{
  #'     x <- subset_racmoM_by_summer("precip", 1991) |>
  #'       subset_racmoM_by_month(12)
  #'
  #'     # Single basin
  #'     xMask <- mask_racmo_to_basin(x, "A-Ap")
  #'     terra::plot(xMask)
  #'
  #'     # Multiple basins
  #'     xMask2 <- mask_racmo_to_basin(x, c("A-Ap", "Dry Valleys"))
  #'     terra::plot(xMask2)
  #'   }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify basin
  dataCrs <- terra::crs(racmoData)
  basin   <- get_basin_outline(extent = basin, rectangularExtent = FALSE,
                               crs = dataCrs)  # basins are in 3031; reprojects

  # Crop
  croppedRacmo <- terra::crop(x = racmoData, y = basin, snap = "out")
  maskedRacmo  <- terra::mask(x = croppedRacmo, mask = basin, touches = TRUE)

  # Add data back that is lost in cropping
  terra::varnames(maskedRacmo)  <- terra::varnames(racmoData)[[1]]
  terra::longnames(maskedRacmo) <- terra::longnames(racmoData)[[1]]
  terra::units(maskedRacmo)     <- terra::units(racmoData)[[1]]
  names(maskedRacmo)            <- names(racmoData)

  return(maskedRacmo)
}
