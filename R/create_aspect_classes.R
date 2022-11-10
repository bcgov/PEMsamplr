  #' Create aspect classes
  #'
  #' Creates a 3-class aspect from DAH. classes from a TPI generated from a DEM following the rules in
  #' https://github.com/gianmarcoalberti/GmAMisc/blob.master/R/landfClass.r
  #'
  #'
  #' Thresholds of 0.2 - 0.3 are reasonable setting
  #'
  #' @param x: input 25m dah created in SAGA (.tif).
  #' @param threshold: sets the DAH threshold where <x are cold aspects and >x are warm aspects. The rest are neutral.
  #' @keywords diurnal anisotropic heating, aspect classes
  #'
  #' @return
  #' @export
  #' @examples
  #' data(dah)
  #'

create_aspect_classes <- function (x, threshold = 0.2) {


m <- c( -10, (threshold*-1), 1,
        (threshold*-1 ), threshold, 2,
        threshold, 10,  3)

rclmat <- matrix(m, ncol=3, byrow =TRUE)
rc <- terra::classify(x , rclmat)

return(rc)

}



