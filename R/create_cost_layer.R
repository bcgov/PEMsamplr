#' create cost layer
#'
#' @param x **SpatRast or RasterLayer (raster package), output from prepare_cost_layer ()
#' @param start **SpatVec of sf object with Start Locations for the cost layer.
#' @param trans_output TRUE/FALSE - do you want to save output of transition layer as rds (to be used in vrp)
#' @param transition_outdir **filepath for where to output transition rds
#' @importFrom magrittr "%>%"
#' @import gdistance
#' @return RasterLayer with Cost and output the transition file for Vrp use
#' @export
#'
#' @examples
#' create_cost(costprep, start, outdir)

create_cost_layer <- function(x, start, trans_output = FALSE, transition_outdir = NULL){

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

  if(trans_output == TRUE){
  saveRDS(tr1, file.path(transition_outdir,"transition_layer.rds"))
  }

  acost <- gdistance::accCost(tr1,start)
  gc()
  terra::plot(acost)
  return(acost)
}
