#' create cost layer
#'
#' @param dem **SpatRast or RasterLayer (raster package), output from prepare_cost_layer ()
#' @param start **SpatVec of sf object with Start Locations for the cost layer.
#' @import magrittr
#' @import terra
#' @import gdistance
#' @return RasterLayer with Cost
#' @export
#'
#' @examples
#' create_cost(costprep, start)

#x <- altAll#costprep
#start <- start

# create_helicost_layer <- function(dem, start){
#
#
#   tr <- gdistance::transition(altRast, transitionFunction = function(x) 1/mean(x,na.rm = T),
#                    directions = 8, symm = F)
#   tr <- geoCorrection(tr)
#
#
#   # create  transition layer
#   tr <- gdistance::transition(x, transitionFunction = function(x) 1/mean(x), directions = 8, symm = F)
#   tr1 <- gdistance::geoCorrection(tr)
#
#   rm(tr)
#   acost <- gdistance::accCost(tr1,start)
#   gc()
#   terra::plot(acost)
#   return(acost)
# }
