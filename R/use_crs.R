use_crs <- function(crs = "racmo") {
  #' Return a crs string
  #'
  #' @description The RACMO projection does not have a standard EPSG code. This
  #'   function returns the crs definition for RACMO in Antarctica. A couple of
  #'   other common projection options for Antarctica are also available. The
  #'   projection for RACMO is taken directly from the RACMO2.3p3 NetCDFs; use
  #'   ncdf4::ncatt_get(nc, "rotated_pole") to view it.
  #'
  #' @param crs "string": Which crs to return? Defaults to "racmo", but other
  #'   options are: "lambert", "ortho", "stereographic" (EPSG:3031), and "era5"
  #'   / "WGS84" (EPSG:4326).
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Non-EPSG Projections
  racmoCrs   <- paste("+proj=ob_tran +o_proj=longlat",
                      "+o_lat_p=-180.0 +lon_0=10.0",
                      "-m 57.295779506")

  lambertCrs <- paste("+proj=laea +lat_0=-90 +lon_0=0",
                      "+x_0=0 +y_0=0",
                      "+datum=WGS84 +units=m +no_defs +type=crs")

  orthoCrs   <- paste("+proj=ortho +f=0 +lat_0=-90 +lon_0=0",
                      "+x_0=0 +y_0=0",
                      "+datum=WGS84 +units=m +no_defs +type=crs")

  # Switch and return crs
  crs <- switch(tolower(crs),                   # make case insensitivite
                   "racmo"                      = racmoCrs,
                   "lambert"                    = lambertCrs,
                   "ortho"  = , "orthographic"  = orthoCrs,
                   "stereo" = , "stereographic" = ,
                   "3031"   = , "epsg:3031"     = "EPSG:3031",
                   "wgs84"  = , "wgs 84"        = ,
                   "era5"   = , "ecmwf"         = ,
                   "4326"   = , "epsg:4326"     = "EPSG:4326",
                   crs)

  return(crs)
}
