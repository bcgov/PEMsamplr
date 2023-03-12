#' Create cost no sample area
#'
#' Creates and applies a mask to permanently changed areas where sample is not be conducted.
#' This includes (lakes, and permanently changed landscape i.e roads)
#'
#' @param vec_dir text string with folder location of base vector layers
#' @param cost SpatRast of cost layer with high cost penalty applied
#' @import magrittr
#' @return **SpatRast** A masked cost raster layer
#' @export
#'
#' @examples
#' create_cost_nosample <- function(vec_dir, cost)
#'
create_cost_nosample <- function(vec_dir, cost) {

  # exclude water:
  water <- sf::st_read(file.path(vec_dir, "water.gpkg")) %>%
    dplyr::filter(WATERBODY_TYPE != "W")
  water_buff <- st_buffer(water, dist = 150) # lakes

  # exclude roads

  # build in a check for if file exists and a stop

  roads <- sf::st_read(file.path(vec_dir, "road_all.gpkg")) %>%
    sf::st_transform(3005)
  roads_buff <- sf::st_buffer(roads, dist = 175) # roads

  # create accumulated sample cost mask
  sample_cost_masked <- terra::mask(cost, roads_buff, inverse = TRUE) %>%
    terra::mask(water_buff, inverse = TRUE)

  return(sample_cost_masked)

}
