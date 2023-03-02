#' create cost layer
#'
#' @param x **SpatRast or RasterLayer (raster package), output from prepare_cost_layer ()
#' @param start **SpatVec of sf object with Start Locations for the cost layer.
#' @import magrittr
#' @import gdistance
#' @return RasterLayer with Cost
#' @export
#'
#' @examples
#' create_cost(costprep, start)

#x <- costprep
#start <- helidrop

create_cost_layer <- function(x, start){

  # if is spatRast need to convert first

  if(class(x)[1] =="SpatRaster") {

    print("converting SpatRaster to Raster object")
    x <- raster::raster(x)

  }

  if(class(start)[1] =="sf") {

    print("converting SpatRaster to Raster object")
    x <- raster::raster(x)

  }


  # create  transition layer
  tr <- gdistance::transition(x, transitionFunction = function(x) 1/mean(x,na.rm = T), directions = 8, symm = F)
  tr1 <- gdistance::geoCorrection(tr)

  rm(tr)
  acost <- gdistance::accCost(tr1,start)
  gc()
  terra::plot(acost)
  return(acost)
}
