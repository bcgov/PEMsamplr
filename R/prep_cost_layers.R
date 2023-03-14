#' prepare_cost_layers
#'
#' Use roads speed and slope to calculate initial cost of travel to be used in cost layer
#'
#' @param vec_dir directory where clean vector layers are stored
#' @param dem digital terrain model .SpatRast
#' @param heli TRUE or FALSE to indicate if road or heli design
#' @import magrittr
#' @return **SpatRast** cost surface to be used in cost layer creation
#' @export
#' @examples
#' prep_cost_layers(vec_dir, dem)
#'
#'

# Prepare the input layers for the cost layers

prep_cost_layers <- function(vec_dir, dem, heli = FALSE) {

  if(heli == FALSE) {

    ## read in the major roads
    roads <- sf::st_read(file.path(vec_dir, "road_major.gpkg"), quiet = TRUE) %>%
      sf::st_zm()

    roads$ROAD_CLASS[roads$trail == 1] <- "trail"
    roads <- roads[,c("ROAD_SURFACE","ROAD_CLASS", "ROAD_NAME_FULL")]
    roads <- roads %>% dplyr::rename('road_surface' = ROAD_CLASS,
                                     'surface' = ROAD_SURFACE,
                                     'name' = ROAD_NAME_FULL)
    rdsAll <-  data.table::as.data.table(roads) %>% sf::st_as_sf()
    rSpd <- dplyr::tibble(
      "road_surface" = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "ferry", "driveway","unclassifed"),
      #"speed" = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1))
      "speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 3000, 4.5, 3000))

    #   # convert speed to pace
    rSpd <- data.table::as.data.table(rSpd) %>%
      dplyr::mutate(pace = 1.5*(1/speed)) %>%
      dplyr::select(-speed) # km/h to minutes per 25m pixal

    rdsAll <- merge(rdsAll, rSpd, by = "road_surface", all = F)
    rdsAll <- rdsAll[,"pace"]
    #allRast <- terra::rasterize(rdsAll, dem, field = "pace")
    #allRast[is.nan(allRast[])] <- NA

    #   # create a roads raster (buffered)
    rdsAll <- sf::st_buffer(rdsAll, dist = 25, endCapStyle = "SQUARE", joinStyle = "MITRE")
    rdsAll <- sf::st_cast(rdsAll, "MULTIPOLYGON")
    rdsRast <- terra::rasterize(rdsAll, dem, field = "pace")
    rdsRast[is.nan(rdsRast[])] <- NA
    #rm(allRast)

    print("road layers prepared")

    #   # prepare the water data

    water <- sf::st_read(file.path(vec_dir, "water.gpkg"),quiet = TRUE) %>%
      dplyr::filter(WATERBODY_TYPE != "W") %>%
      dplyr::mutate(cost = 10000) %>%
      sf::st_cast("MULTIPOLYGON") %>%
      dplyr::select(cost)
    water_r <- terra::rasterize(water, dem, field = "cost")

    dem[water_r] <- 0
    rm(water_r)

    print("water layer prepared")

    #   # prepare walking terrain function
    slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "radians") # convert these radians to rise/run in next line

    dem <- (3/5) * 6*exp(-3.5*abs(tan(slope) + 0.05)) * (40/60)## this converts km/hr to minutes/25m pixel
    # 40 x 25 = 1lm / 60 minutes from hours
    dem_toblers <- 1/dem %>% round(3)

    altAll  <- terra::cover(rdsRast, dem_toblers)
    #terra::plot(altAll)

    print("walking terrain surface (minutes by 25m pixal) prepared")

    gc()

  } else { # heli = TRUE

    #   # prepare walking terrain function
    slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "radians") # convert these radians to rise/run in next line

    dem <- (3/5) * 6*exp(-3.5*abs(tan(slope) + 0.05)) * (40/60)## this converts km/hr to minutes/25m pixel
    # 40 x 25 = 1lm / 60 minutes from hours
    dem_toblers <- 1/dem %>% round(3)

    altAll  <- dem_toblers
    #terra::plot(altAll)

    print("walking terrain surface (minutes by 25m pixal) prepared")

    gc()


  }

  return(altAll)

}



# # Vineyettw
# #
# #
# # # create  transition layer
# # tr <- transition(altAll, transitionFunction = function(x) 1/mean(x), directions = 8, symm = F)
# #
# # saveRDS(tr, file.path(out_path,"input_raster", "transition_layer.rds"))
# # #tr <- readRDS(file.path(out_path,"input_raster", "transition_layer.rds"))
# #
# # rm(altAll)
# # rm(rdsAll)
# # rm(rdsRast)
# # rm(alt)
# # gc()
# #
# # tr1 <- geoCorrection(tr)
# #
# # rm(tr)
# # gc()
# #
# # #plot(raster(tr1))
# #
# # # output transition layer to use in creating TSP paths
# # saveRDS(tr1, file.path(out_path,"input_raster", "transition_layer.rds"))
# #
# # #tr1 <- readRDS(file.path(out_path,"input_raster", "transition_layer.rds"))
# # #tr1 = tr
# #
# #
# # acost <- accCost(tr1,start)
# #
# # # calculate the cost layer for travel
# # plot(acost)
# #
# # writeRaster(acost, file.path(out_path,"input_raster", "acost.tif"), format = "GTiff", overwrite = TRUE)
# #
# # rm(tr1)
# # gc()
