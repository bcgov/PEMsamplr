#' Check major road layer
#' Runs a number of basic checks on the road layer prior to using in costlayer
#'
#' @param roads sf object "road_major.gpkg"
#' @return TRUE if all checks run correctly
#' @importFrom sf st_read st_drop_geometry
#' @importFrom dplyr select rename tibble filter
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' check_road_layer(roads)

check_road_layer <- function(roads){

  ## read in the major roads
  #roads <- sf::st_read(file.path(vec_dir, "road_major.gpkg"), quiet = TRUE) %>%
  #  sf::st_zm()

  roads_check <- roads %>%
    st_drop_geometry()%>%
    dplyr::select(ROAD_CLASS,ROAD_SURFACE, ROAD_NAME_FULL)

  if(length(roads_check == 3)){

    print("check 1: road layer contain required fields")
  } else {

    stop(print("road layer does not contain required fields"))
  }

  # check road surface
  roads_check <- roads_check %>% dplyr::rename('road_surface' = ROAD_CLASS,
                                               'surface' = ROAD_SURFACE,
                                               'name' = ROAD_NAME_FULL)

  rsurface = unique(roads_check$surface)
  if("overgrown" %in% rsurface ) {
    print("check 2: road class contains overgrown road segment, are you sure these are actual roads?")
  }else{

    print("check 2: road class does not contain overgrown road segment")
  }

  rSpd <- dplyr::tibble(
    "road_surface" = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "boat", "ferry", "driveway","unclassifed"),
    #"speed" = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1))
    "speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 0.1, 3000, 4.5, 3000))

  rclass = as.data.table(unique(roads_check$road_surface))
  names(rclass) = "road_surface"
  rdsAll <- merge(rclass, rSpd, by = "road_surface", all = T)
  rdsAll <- rdsAll %>%
    dplyr::filter(is.na(speed))
  rdsAll = as.data.frame(rdsAll)

  if(nrow(rdsAll)>0){
    print("check 3: undefined speeds for the following road surface types")

  } else {
    print("check 3: all road surface types assigned a speed")
  }

  # options to add more chceks here (geometry etc)

  return(TRUE)

}
