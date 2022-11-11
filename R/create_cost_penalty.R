#' Create cost penalty layer
#'
#' @param vec_dir folder location of base vector layers
#' @param cost SpatRast or Raster of cost layer
#' @param costval Numeric value of assigned high cost
#' @import magrittr
#' @return SpatRast
#' @export
#'
#' @examples
#' create_cost_penalty(vec_dir, cost, costval = 3000)
#'

#
# # Prepare the
# #
# vec_dir  = "D:\\PEM_DATA\\PEMsamplr\\temp\\date\\base_layers" #location of baselayers
# cov_dir = "D:\\PEM_DATA\\PEMsamplr\\temp\\date\\25m" #location of dem
# cost <- file.path(cov_dir, "dem.tif")
# template <- file.path(cov_dir, "template.tif")
#
#
# rcost <- terra::rast(cost)
# terra::crs(rcost) <- "epsg:3005"
# names(rcost) = "cost"
#
#
# rtemp <- terra::rast(template)
# terra::crs(rtemp) <- "epsg:3005"
# names(cost) = "cost"
#
# cost <- rcost

#shape <- file.path(vec_dir, "cutblocks.gpkg")
#assign_highcost(shape, crs = 3005, costval = 3000)

# function to assign high cost and format output

.assign_highcost = function(shape, crs = 3005, costval, cost) {

      hcsf <- sf::st_read(shape) %>%
        sf::st_set_crs(crs) %>%
        dplyr::mutate(cost = costval) %>%
        dplyr::select(cost) %>%
        sf::st_buffer(dist = 150) %>%
        sf::st_cast("MULTIPOLYGON")

      rhc <- terra::rasterize( hcsf, cost, field = "cost")
      return(rhc)

}


create_cost_penalty <- function(vec_dir, cost, costval = 3000) {

  # 1. Assign high cost for cutblocks
  rcutblock<- .assign_highcost(file.path(vec_dir, "cutblocks.gpkg"), costval = costval, cost = cost)
  hc <- terra::merge(rcutblock, cost)

  # 2. Assign high cost to age class 1 and 2
  rvri12_class <- .assign_highcost(file.path(vec_dir, "vri_class1_2.gpkg"), costval = costval, cost = cost)
  hc <- terra::merge(rvri12_class, hc)

  # 3. Assign a slightly lower cost to age class 3.
  rvri3_class <- .assign_highcost(file.path(vec_dir, "vri_class3.gpkg"), costval = costval, cost = cost)
  hc <- terra::merge(rvri3_class, hc)

  # for some AOIS;
  # 3a Assign a high cost to deciduous leading species area

  # # fix the trycatch section ## STILL TO FIX
  # rvri_decid <- tryCatch({.assign_highcost(file.path(vec_dir, "vri_decid.gpkg"), costval = costval,cost = cost))
  # hc <- terra::merge(rvri_decid , hc)
  #
  # })

  # 4. Assign high cost to private lands
  rpriv <- .assign_highcost(file.path(vec_dir, "private.gpkg"), costval = costval, cost = cost)
  hc <- terra::merge(rpriv, hc)

  # 5. Add high cost for high and medium intensity fire areas or all fires
  rfireint <- .assign_highcost(file.path(vec_dir,"fire_int.gpkg"), costval = costval, cost = cost)
  hc <- terra::merge(rfireint, hc)

  # 6. Add high cost for all fires
  rfires <- .assign_highcost(file.path(vec_dir,"fires.gpkg"), costval = costval, cost = cost)
  hc <- terra::merge(rfires, hc)

  # 7. Assign high cost to transmission lines
  rtrans <- .assign_highcost(file.path(vec_dir,"translines.gpkg"), costval = costval, cost = cost)
  hc <- terra::merge(rtrans, hc)

  return(hc)
}
