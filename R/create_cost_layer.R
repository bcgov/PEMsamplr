#' create cost layer
#'
#' @param x RasterLayer or RasterBrick (raster package) takes output from prepare_cost_layer ()
#' @param start Start Locations for the cost layer.
#' @import magrittr
#' @return RasterLayer with Cost
#' @export
#'
#' @examples
#' create_cost(alt)

# create_cost_layer <- function(x, start){
#
#   # if is spatRast need to convert first
#
#   if(class(x)[1] =="SpatRaster") {
#
#     print("converting SpatRaster to Raster object")
#     x <- raster::raster(x)
#
#   }
#
#   # create  transition layer
#   tr <- gdistance::transition(x, transitionFunction = function(x) 1/mean(x), directions = 8, symm = F)
#   tr1 <- geoCorrection(tr)
#
#   rm(tr)
#   #plot(raster(tr1))
#   #saveRDS(tr1, file.path(out_path,"input_raster", "transition_layer.rds"))
#
#   acost <- accCost(tr1,start)
#
#   # calculate the cost layer for travel
#   #plot(acost)
#
#   #writeRaster(acost, file.path(out_path,"input_raster", "acost.tif"), format = "GTiff", overwrite = TRUE)
#
#   gc()
#
#   # check that it is the same extent as the raster and crop to the tempate (25m )
#
#   return(acost)
# }
