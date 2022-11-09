  #' Create aspect classes
  #'
  #' Creates a 3-class aspect from DAH. classes from a TPI generated from a DEM following the rules in
  #' https://github.com/gianmarcoalberti/GmAMisc/blob/master/R/landfClass.R
  #'
  #' Classes derived from TPI calculated from a 25m DEM
  #'
  #' @param x: input 25m dah created in SAGA (.tif).
  #' @param threshold: sets the DAH threshold where <x are cold aspects and >x are warm aspects. The rest are neutral.
  #' @keywords diurnal anisotropic heating, aspect classes
  #' @export
  #' @examples
  #' data(dem)
  #'
#x = raster::raster("./temp_data/dem.tif")
#scale = 3; sn=3; ln=7; n.classes="six"; add.tpi=FALSE; stand.tpi = FALSE
#x <- rast("./cv-rasters/10/dah_10.tif")
#threshold = 0.2
create_aspect_classes <- function (x, threshold = 0.2) {

#threshold <- 0.2
#threshold <- 0.3 testing for SE

# Build a reclass matrix for three group using (+/- threshold)
# all values > 0 and <= 0.25 become 1, etc.
# Wetzinkwa on the 0.3 threshold
# PeterHope on the 0.2 threshold

m <- c( -10, (threshold*-1), 1,
        (threshold*-1 ), threshold, 2,
        threshold, 10,  3)

rclmat <- matrix(m, ncol=3, byrow =TRUE)
rc <- terra::classify(x , rclmat)
#crs(rc) <- PROJ
#rc <- crop(rc, dem_template)
return(rc)
#terra::writeRaster(rc, file.path(output, "dah_LS.tif"),  overwrite = TRUE)
}
#terra::writeRaster(rc, file.path(output, "dah_LS.tif"),  overwrite = TRUE)


