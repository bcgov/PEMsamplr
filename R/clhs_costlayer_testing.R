#' # #' #' TESTING COST LAYER
#' #' #'
#' #' #' Use roads speed and slope to calculate initial cost of travel to be used in cost layer
#' #' #'
#' #' #' @param vec_dir directory where clean vector layers are stored
#' #' #' @param dem digital terrain model .SpatRast
#' #' #' @param heli TRUE or FALSE to indicate if road or heli design
#' #' #'@importFrom magrittr "%>%"
#' #' #' @return **SpatRast** cost surface to be used in cost layer creation
#' #' #' @export
#' #' #' @examples
#' #' #' prep_cost_layers(vec_dir, dem)
#' #' #'
#' #' #'
#' #'
#' #' # Prepare the input layers for the cost layers
#' #'
#' #' prep_cost_layers11 <- function(vec_dir, dem, heli = FALSE) {
#' #'
#' #'   cities <- st_read(file.path(vec_dir, "major_towns_bc.gpkg"))
#' #'   nearest_town = "datecreek_test"
#' #'   start <- cities[cities$NAME == nearest_town,"NAME"]%>%
#' #'     as("Spatial")
#' #'
#' #'   outpath <- fid$sampling_input_landscape[2]
#' #'
#' #'
#' #' #  if(heli == FALSE) {
#' #'
#' #'     ## read in the major roads
#' #'     roads <- sf::st_read(file.path(vec_dir, "road_major.gpkg"), quiet = TRUE) %>%
#' #'       sf::st_zm()
#' #'
#' #'     roads$ROAD_CLASS[roads$trail == 1] <- "trail"
#' #'     roads <- roads[,c("ROAD_SURFACE","ROAD_CLASS", "ROAD_NAME_FULL")]
#' #'     roads <- roads %>% dplyr::rename('road_surface' = ROAD_CLASS,
#' #'                                      'surface' = ROAD_SURFACE,
#' #'                                      'name' = ROAD_NAME_FULL)
#' #'     rdsAll <-  data.table::as.data.table(roads) %>% sf::st_as_sf()
#' #'     rSpd <- dplyr::tibble(
#' #'       "road_surface" = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "ferry", "driveway","unclassifed"),
#' #'       "speed" = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1, 4.5, 30))
#' #'       #"speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 3000, 4.5, 3000))
#' #'
#' #'     rSpd <- rSpd %>%
#' #'       rowwise() %>%
#' #'       mutate(pace_sec_km = 3600 / speed,
#' #'              pace_sec_m = 3600/ (speed * 1000),
#' #'              pace_min_pixal = 60/(speed * 1000)*25) # gives seconds/km
#' #'
#' #'
#' #'     # speed = km/h
#' #'     #   # convert speed to pace (pace = time (sec)/distance (km))
#' #'     #rSpd2 <- data.table::as.data.table(rSpd) %>%
#' #'     #  dplyr::mutate(pace = 1.5*(1/speed)) %>%
#' #'     #  dplyr::select(-speed) # km/h to minutes per 25m pixal
#' #'
#' #'     rdsAll <- merge(rdsAll, rSpd, by = "road_surface", all = F)
#' #'     rdsAll <- rdsAll[,"pace_min_pixal"]
#' #'     #allRast <- terra::rasterize(rdsAll, dem, field = "pace")
#' #'     #allRast[is.nan(allRast[])] <- NA
#' #'
#' #'     #   # create a roads raster (buffered)
#' #'     rdsAll <- sf::st_buffer(rdsAll, dist = 25, endCapStyle = "SQUARE", joinStyle = "MITRE")
#' #'     rdsAll <- sf::st_cast(rdsAll, "MULTIPOLYGON")
#' #'     rdsRast <- terra::rasterize(rdsAll, dem, field = "pace_min_pixal", fun = "max")
#' #'     rdsRast[is.nan(rdsRast[])] <- NA
#' #'     #rm(allRast)
#' #'
#' #'     print("road layers prepared")
#' #'
#' #'     roads_r <- raster(rdsRast)
#' #'
#' #'
#' #'
#' #'
#' #'     #   # prepare the water data
#' #'
#' #'     water <- sf::st_read(file.path(vec_dir, "water.gpkg"),quiet = TRUE) %>%
#' #'       dplyr::filter(WATERBODY_TYPE != "W") %>%
#' #'       dplyr::mutate(cost = 10000) %>%
#' #'       sf::st_cast("MULTIPOLYGON") %>%
#' #'       dplyr::select(cost)
#' #'     water_r <- terra::rasterize(water, dem, field = "cost", fun = "max")
#' #'
#' #'     dem[water_r] <- NA
#' #'     rm(water_r)
#' #'
#' #'     print("water layer prepared")
#' #'
#' #'     #   # prepare walking terrain function
#' #'     #slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "radians") # convert these radians to rise/run in next line
#' #'
#' #'     tic()
#' #'     demr <- raster(dem)
#' #'     altDiff <- function(x){x[1]-x[2]}
#' #'     hd <- transition(demr, transitionFunction = altDiff, directions = 8, symm = F)
#' #'     slope <- geoCorrection(hd)
#' #'
#' #'     # calculate speed
#' #'     adj <- adjacent(demr, cells = 1:ncell(demr), pairs = T, directions = 8)
#' #'     speed <- slope
#' #'     speed[adj] <- exp(-3.5*abs(slope[adj] + 0.05)) ##tobler's hiking function
#' #'     rspeed <- raster(speed)
#' #'     plot(rspeed)
#' #'     rspeed[roads_r]<- NA
#' #'     plot(rspeed)
#' #'
#' #'     ar <- overlay(rspeed, roads_r, fun=function(x,y){return(x+y)})
#' #'     ar <- cover(roads_r, rspeed)
#' #'
#' #'     # add speed of travel for roads
#' #'    # rdsr <- raster(rdsRast)
#' #'     conductance <- geoCorrection(ar) # speed
#' #'
#' #'
#' #'
#' #'     # calculate conductance, Conductance = speed/distance
#' #'     conductance <- geoCorrection(speed) # speed
#' #'
#' #'     #This looks a lot like a measure that we are more familiar with:
#' #'     #traveltime = distance/speed
#' #'     #In fact, the conductance values we have calculated are the reciprocal of travel time.
#' #'     #1/traveltime = speed/distance = conductance
#' #'
#' #'     acost_old <- accCost(conductance, start)
#' #'     toc()
#' #'     #59.32 sec elapsed
#' #'
#' #'
#' #' # NEW version:"
#' #'
#' #'    # other option least cost path
#' #'     tic()
#' #'     slope <- calculate_slope(demr, neighbours = 8, exaggeration= FALSE)
#' #'     cost <- apply_cost(
#' #'       slope = slope,
#' #'       cost_function = "tobler offpath",
#' #'       neighbours = 8,
#' #'       crit_slope = 12,
#' #'       max_slope = NULL,
#' #'       percentile = 0.5
#' #'     )
#' #'     acost_lcp <- accCost(cost, start)
#' #'     toc()
#' #'     #49.07 sec elapsed
#' #'
#' #'
#' #'
#' #'
#' #'     acost <- acost/3600
#' #'     acost <- rast(acost)
#' #'     lays <- c(ancDat,acost)
#' #'
#' #'
#' #'
#' #'
#' #'
#' #'
#' #'
#' #'
#' #'
#' #'
#' #'
#' #'     dem <- (3/5) * 6*exp(-3.5*abs(tan(slope) + 0.05)) * (40/60)## this converts km/hr to minutes/25m pixel
#' #'     # 40 x 25 = 1lm / 60 minutes from hours
#' #'     dem_toblers <- 1/dem %>% round(3)
#' #'
#' #'     altAll  <- terra::cover(rdsRast, dem_toblers)
#' #'     #terra::plot(altAll)
#' #'
#' #'     print("walking terrain surface (minutes by 25m pixal) prepared")
#' #'
#' #'     gc()
#' #'
#' #'   } else { # heli = TRUE
#' #'
#' #'     #   # prepare walking terrain function
#' #'     slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "radians") # convert these radians to rise/run in next line
#' #'
#' #'     # testing other function
#' #'     rdem <- raster(dem)
#' #'
#' #'     sl <- calculate_slope(dem = rdem, neighbours = 8, exaggeration = TRUE)
#' #'
#' #'
#' #'     dem <- (3/5) * 6*exp(-3.5*abs(tan(slope) + 0.05)) * (40/60)## this converts km/hr to minutes/25m pixel
#' #'     # 40 x 25 = 1lm / 60 minutes from hours
#' #'     dem_toblers <- 1/dem %>% round(3)
#' #'
#' #'     altAll  <- dem_toblers
#' #'     #terra::plot(altAll)
#' #'
#' #'     print("walking terrain surface (minutes by 25m pixal) prepared")
#' #'
#' #'     gc()
#' #'
#' #'   }
#' #'   return(altAll)
#' #'
#' #' }
#' #'
#' #
#' ## OLD VERSION
#' #
#' # prep_cost_layers <- function(vec_dir, dem, heli = FALSE) {
#' #
#' #   if(heli == FALSE) {
#' #
#' #     demr <- raster(dem)
#' #
#' #
#' #     ## read in the major roads
#' #     roads <- sf::st_read(file.path(vec_dir, "road_major.gpkg"), quiet = TRUE) %>%
#' #       sf::st_zm()
#' #
#' #     roads$ROAD_CLASS[roads$trail == 1] <- "trail"
#' #     roads <- roads[,c("ROAD_SURFACE","ROAD_CLASS", "ROAD_NAME_FULL")]
#' #     roads <- roads %>% dplyr::rename('road_surface' = ROAD_CLASS,
#' #                                      'surface' = ROAD_SURFACE,
#' #                                      'name' = ROAD_NAME_FULL)
#' #     rdsAll <-  data.table::as.data.table(roads) %>% sf::st_as_sf()
#' #     rSpd <- dplyr::tibble(
#' #       "road_surface" = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "ferry", "driveway","unclassifed"),
#' #       "speed" = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1, 4.5, 30))
#' #     #"speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 3000, 4.5, 3000))
#' #
#' #     #   # convert
#' #     rSpd <- as.data.table(rSpd)
#' #     rdsAll <- merge(rdsAll, rSpd, by = "road_surface", all = F)
#' #     rdsAll <- rdsAll[,"speed"] #km/h
#' #     allRast <- raster(rdsAll, resolution = 25)
#' #
#' #     #   # create a roads raster (buffered)
#' #     rdsAll <- sf::st_buffer(rdsAll, dist = 25, endCapStyle = "SQUARE", joinStyle = "MITRE")
#' #     rdsAll <- sf::st_cast(rdsAll, "MULTIPOLYGON")
#' #     rdsRast <- terra::rasterize(rdsAll, dem, field = "speed", fun = "max")
#' #     rdsRast[is.nan(rdsRast[])] <- NA
#' #     rdsRast <- raster(rdsRast)
#' #     #rm(allRast)
#' #
#' #     print("road layers prepared")
#' #
#' #     #   # prepare the water data
#' #
#' #     water <- sf::st_read(file.path(vec_dir, "water.gpkg"),quiet = TRUE) %>%
#' #       dplyr::filter(WATERBODY_TYPE != "W") %>%
#' #       dplyr::mutate(cost = 10000) %>%
#' #       sf::st_cast("MULTIPOLYGON") %>%
#' #       dplyr::select(cost)
#' #     water_r <- terra::rasterize(water, dem, field = "cost", fun = "max")
#' #
#' #     dem[water_r] <- 0
#' #     rm(water_r)
#' #
#' #     print("water layer prepared")
#' #
#' #
#' #     altAll <- merge(rdsRast, demr)
#' #
#' #
#' #     # set the threshold in which to determine the slope calculations vs road travel calculation in the transition layer. This may need to be adjusted depedning on terrain.
#' #     alt.threshold <- 250 # Date Creek
#' #
#' #     trFn <- function(x){
#' #       if(x[1] > alt.threshold & x[2] > alt.threshold){
#' #         x[1]-x[2]
#' #       }else{
#' #         min(x[1],x[2])
#' #       }
#' #     }
#' #     # identify the areas treated as road and those to be calculatd for slope
#' #     rdIdx <- which(values(altAll) < 100)            # road index
#' #     slpIdx <- which(values(altAll) > alt.threshold) # slope index
#' #     #rdAdj <- adjacent(altAll, cells = rdIdx, pairs = T, directions = 8)
#' #     adj <- adjacent(altAll, cells = slpIdx, pairs = T, directions = 8)
#' #     adj <- adj[!adj[,1] %in% rdIdx ,]
#' #     adj <- adj[!adj[,2] %in% rdIdx ,]
#' #
#' #     # calculate the altitude difference
#' #     tr <- transition(altAll, trFn , directions = 8, symm = F) ##altDiff and speed (km/h)
#' #     #convert to slope  i.e. altitude diff / distance travelled
#' #     tr1 <- geoCorrection(tr) ##divided by 25 - slope and conductance (km/h/m)
#' #
#' #     # plot(raster(tr1))
#' #
#' #     # apply Toblers hiking function to the slope areas
#' #     tr1[adj] <- (3/5)*(6*exp(-3.5*abs(tr1[adj] + 0.08))) ##tobler's hiking function * 3/5 - gives km/h
#' #     tr1 <- tr1*1000 ##now roads are correct conductance (h/m), and walking in m/h
#' #     tr2 <- geoCorrection(tr1) ##have to geocorrect this part again
#' #     tr1[adj] <- tr2[adj] ##tr1 values are now all conductance in h/metre
#' #
#' #     # output transition layer to use in creating TSP paths
#' #     #saveRDS(tr1, file.path(out_path,"input_raster", "transition_layer.rds"))
#' #
#' #     # calculate the cost layer for travel
#' #     acost <- accCost(tr1, start)
#' #     plot(acost)
#' #     #acost[acost >3000]<- NA
#' #
#' #     terra::rast(acost)
#' #     writeRaster
#' #
#' #     outpath <- fid$sampling_input_landscape[2]
#' #
#' #     names(cost) <- "acost"
#' #
#' #     terra::writeRaster(acost, file.path(fid$sampling_input_landscape[2], "acost_old.tif"), overwrite = TRUE)
#' #     #terra::plot(sample_cost_masked)
#' #
#' #
#' #
#' #   ###
#' ' #' TESTING COST LAYER
#' #' #'
#' #' #' Use roads speed and slope to calculate initial cost of travel to be used in cost layer
#' #' #'
#' #' #' @param vec_dir directory where clean vector layers are stored
#' #' #' @param dem digital terrain model .SpatRast
#' #' #' @param heli TRUE or FALSE to indicate if road or heli design
#' #' #'@importFrom magrittr "%>%"
#' #' #' @return **SpatRast** cost surface to be used in cost layer creation
#' #' #' @export
#' #' #' @examples
#' #' #' prep_cost_layers(vec_dir, dem)
#' #' #'
#' #' #'
#' #'
#' #' # Prepare the input layers for the cost layers
#' #'
#' #' prep_cost_layers11 <- function(vec_dir, dem, heli = FALSE) {
#' #' #'
#' #'   cities <- st_read(file.path(vec_dir, "major_towns_bc.gpkg"))
#' #'   nearest_town = "datecreek_test"
#' #'   start <- cities[cities$NAME == nearest_town,"NAME"]%>%
#' #'     as("Spatial")
#' #'
#' #'   dem <- terra::rast(file.path( fid$cov_dir_1020[2],"25m", "dem.tif"))
#' #'   demr <- raster::raster(dem)
#' #'
#' #'   ## read in the major roads
#' #'   roads <- sf::st_read(file.path(vec_dir, "road_major.gpkg"), quiet = TRUE) %>%
#' #'       sf::st_zm()
#' #'
#' #'   # prepare roads layer
#' #'   roads$ROAD_CLASS[roads$trail == 1] <- "trail"
#' #'   roads <- roads[,c("ROAD_SURFACE","ROAD_CLASS", "ROAD_NAME_FULL")]
#' #'   roads <- roads %>% dplyr::rename('road_surface' = ROAD_CLASS,
#' #'                                    'surface' = ROAD_SURFACE,
#' #'                                    'name' = ROAD_NAME_FULL)
#' #'   rdsAll <-  data.table::as.data.table(roads) %>% sf::st_as_sf()
#' #'   rSpd <- data.table::data.table(
#' #'     road_surface = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "ferry", "driveway","unclassifed"),
#' #'     speed_kmh = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1, 4.5, 30))
#' #'   #"speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 3000, 4.5, 3000))
#' #'   rSpd[,speed := speed_kmh/3.6] ##convert to m/s
#' #'
#' #'   rdsAll <- merge(rdsAll, rSpd, by = "road_surface", all = F)
#' #'   rdsAll <- rdsAll[,"speed"]
#' #'
#' #'   #   # create a roads raster (buffered)
#' #'   rdsAll <- sf::st_buffer(rdsAll, dist = 25, endCapStyle = "SQUARE", joinStyle = "MITRE")
#' #'   rdsAll <- sf::st_cast(rdsAll, "MULTIPOLYGON")
#' #'   rdsRast <- terra::rasterize(rdsAll, dem, field = "speed", fun = "max")
#' #'   rdsRast[is.nan(rdsRast[])] <- NA
#' #'
#' #'  # roads_r <- raster(rdsRast)
#' #'   water <- sf::st_read(file.path(vec_dir, "water.gpkg"),quiet = TRUE) %>%
#' #'       dplyr::filter(WATERBODY_TYPE != "W") %>%
#' #'       dplyr::mutate(cost = 10000) %>%
#' #'       sf::st_cast("MULTIPOLYGON") %>%
#' #'       dplyr::select(cost)
#' #'     water_r <- terra::rasterize(water, dem, field = "cost", fun = "max")
#' #'
#' #'     dem[water_r] <- NA
#' #'     rm(water_r)
#' #'
#' #'
#' #'   rdsRastr <- raster(rdsRast)
#' #'   altAll <- merge(rdsRastr, demr)
#' #'
#' #'   # set the threshold in which to determine the slope calculations vs road travel calculation in the transition layer. This may need to be adjusted depedning on terrain.
#' #'     alt.threshold <- 0 # Date Creek
#' #'
#' #'     trFn <- function(x){
#' #'       if(x[1] > alt.threshold & x[2] > alt.threshold){
#' #'         x[1]-x[2]
#' #'       }else{
#' #'         min(x[1],x[2])
#' #'       }
#' #'     }
#' #'     # identify the areas treated as road and those to be calculatd for slope
#' #'     rdIdx <- which(values(altAll) < 100)            # road index
#' #'     slpIdx <- which(values(altAll) > alt.threshold) # slope index
#' #'     #rdAdj <- adjacent(altAll, cells = rdIdx, pairs = T, directions = 8)
#' #'     adj <- adjacent(altAll, cells = slpIdx, pairs = T, directions = 8)
#' #'     adj <- adj[!adj[,1] %in% rdIdx ,]
#' #'     adj <- adj[!adj[,2] %in% rdIdx ,]
#' #'
#' #'     # calculate the altitude difference
#' #'     tr <- transition(altAll, trFn , directions = 8, symm = F) ##altDiff and speed (km/h)
#' #'     #convert to slope  i.e. altitude diff / distance travelled
#' #'     tr1 <- geoCorrection(tr) ##divided by 25 - slope and conductance (km/h/m)
#' #'
#' #'     # plot(raster(tr1))
#' #'
#' #'     # apply Toblers hiking function to the slope areas
#' #'     tr1[adj] <- (3/5)*(6*exp(-3.5*abs(tr1[adj] + 0.08))) ##tobler's hiking function * 3/5 - gives km/h
#' #'     tr1 <- tr1*1000 ##now roads are correct conductance (h/m), and walking in m/h
#' #'     tr2 <- geoCorrection(tr1) ##have to geocorrect this part again
#' #'     tr1[adj] <- tr2[adj] ##tr1 values are now all conductance in h/metre
#' #'
#' #'     # output transition layer to use in creating TSP paths
#' #'     saveRDS(tr1, file.path(fid$sampling_input_landscape[2], "transition_layer_oldv.rds"))
#' #'
#' #'     # calculate the cost layer for travel
#' #'     acost <- accCost(tr1, start)
#' #'     plot(acost)
#' #'     #acost[acost >3000]<- NA
#' #'
#' #'     terra::rast(acost)
#' #'     terra::writeRaster(acost, file.path(fid$sampling_input_landscape[2], "acost_old.tif"), overwrite = TRUE)
#' #'     #terra::plot(sample_cost_masked)
