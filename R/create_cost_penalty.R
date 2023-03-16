#' Create cost penalty layer
#'
#' @param vec_dir text string with folder location of base vector layers
#' @param dem SpatRast or Raster of cost layer
#' @param cost SpatRast or Raster of cost layer
#' @param costval Numeric value of assigned high cost
#' @importFrom magrittr "%>%"
#' @return SpatRast
#' @export
#'
#' @examples
#' create_cost_penalty(vec_dir, dem, cost, costval = 3000)
#'
# function to assign high cost and format output


create_cost_penalty <- function(vec_dir, dem, cost, costval = 3000) {

  # 1. Assign high cost for cutblocks
  if(file.exists(file.path(vec_dir, "cutblocks.gpkg"))){

    rcutblock<- .assign_highcost(file.path(vec_dir, "cutblocks.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(rcutblock, cost)
  }

  # 2. Assign high cost to age class 1 and 2
  if(file.exists(file.path(vec_dir, "vri_class1_2.gpkg"))){
  rvri12_class <- .assign_highcost(file.path(vec_dir, "vri_class1_2.gpkg"), costval = costval, cost = cost)
  hc <- terra::cover( rvri12_class, hc)

  }

  # 3. Assign a slightly lower cost to age class 3.
  if(file.exists(file.path(vec_dir, "vri_class3.gpkg"))){
    rvri3_class <- .assign_highcost(file.path(vec_dir, "vri_class3.gpkg"), costval = costval, cost = cost)
    hc <- terra::cover(rvri3_class, hc)
  }

  # for some AOIS;
  # 3a Assign a high cost to deciduous leading species area

  if(file.exists(file.path(vec_dir, "vri_decid.gpkg"))){

   rvri_decid <- .assign_highcost(file.path(vec_dir, "vri_decid.gpkg"), costval = costval,cost = cost)
   hc <- terra::cover( rvri_decid, hc )
  }

  # 4. Assign high cost to private lands
  if(file.exists(file.path(vec_dir, "private.gpkg"))){

  rpriv <- .assign_highcost(file.path(vec_dir, "private.gpkg"), costval = costval, cost = cost)
  hc <- terra::cover( rpriv, hc)
  }

  # 5. Add high cost for high and medium intensity fire areas or all fires
  if(file.exists(file.path(vec_dir, "fire_int.gpkg"))){
  rfireint <- .assign_highcost(file.path(vec_dir,"fire_int.gpkg"), costval = costval, cost = cost)
  hc <- terra::cover(hc, rfireint)
  }

  # 6. Add high cost for all fires
  if(file.exists(file.path(vec_dir, "fires.gpkg"))){
  rfires <- .assign_highcost(file.path(vec_dir,"fires.gpkg"), costval = costval, cost = cost)
  hc <- terra::cover(hc,rfires)

  }

  # 7. Assign high cost to transmission lines
  if(file.exists(file.path(vec_dir, "translines.gpkg"))){
  rtrans <- .assign_highcost(file.path(vec_dir,"translines.gpkg"), costval = costval, cost = cost)
  hc <- terra::cover(hc, rtrans)
  }


  # 8. Very steep areas
  slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "degrees") # convert these radians to rise/run in next line
  # add a threshold value here
  # degrees (45 degrees = 100%, use around 30 degrees ~ 60% )

   m <- c( 0, 30, NA,
          30, 45, 3000)

    rclmat <- matrix(m, ncol=3, byrow =TRUE)
    rc <- terra::classify(slope, rclmat)

    hc_out  <- terra::cover( rc, hc)
    names(hc_out)<- "cost"

    return(hc_out)

}

.assign_highcost = function(shape, crs = 3005, costval, cost) {

  hcsf <- sf::st_read(shape) %>%
    sf::st_set_crs(crs) %>%
    dplyr::mutate(cost = costval) %>%
    dplyr::select(cost) %>%
    sf::st_buffer(dist = 150) %>%
    sf::st_cast("MULTIPOLYGON")

  rhc <- terra::rasterize( hcsf, cost, field = "cost", fun = "max")
  return(rhc)
}

