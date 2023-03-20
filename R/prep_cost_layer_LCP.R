#' Create Cost Layer from Slope
#'
#' @param x x \code{SpatRaster} Digital Elevation Model (DEM)
#' @param cost_function cost_function \code{character} or \code{function}. Cost function applied to slope values. See details for implemented cost functions. tobler (default)
#' @param neighbours \code{numeric} value. Number of directions used in the conductance matrix calculation. Expected numeric values are 4, 8, 16, 32, 48, or matrix object. 16 (default)
#' @param roads \code{sf} of road network
#' @param crit_slope \code{numeric} value. Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Slope values that are greater than the specified max_slope are given a conductivity value of 0. If cost_function argument is 'campbell 2019' or 'campbell' then max_slope is fixed at 30 degrees slope to reflect the maximum slope that the cost function is parametised to. NULL (default)
#' @param percentile   \code{numeric} value. Travel rate percentile only used in 'campbell 2019' cost_function. Expected numeric values are 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99. 0.5 (default)
#' @param exaggeration \code{logical}. if TRUE, positive slope values (up-hill movement) multiplied by 1.99 and negative slope values (down-hill movement) multiplied by 2.31
#' @param exaggeration  if TRUE, positive slope values (up-hill movement) multiplied by 1.99 and negative slope values (down-hill movement) multiplied by 2.31
#' @param multistart  if TRUE, allocate very high speed for paved roads (currently workaround of no multistart point)
#' @author Joseph Lewis (https://github.com/josephlewis/leastcostpath)
#' @return \code{conductanceMatrix} that numerically expresses the difficulty of moving across slope based on the provided cost function
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' slope_cs2 <- create_slope_cs(x = r,
#' cost_function = function(x) {(6 * exp(-3.5 * abs(x + 0.05))) / 3.6}, neighbours = 4)
#' loc <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(839769, 4199443)),crs = terra::crs(r)))
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' slope_cs <- create_slope_cs(x = r, roads = roads, cost_function = "tobler", neighbours = 4)
#' cc <- create_accum_cost(x = slope_cs, origin = loc, rescale = TRUE)

#library(devtools)
#install_github("josephlewis/leastcostpath")
# library(leastcostpath)
# library(terra)
# library(sf)
# # need several functions from this
# source("R/prep_cost_layer_LCP_utils.R")
# #need several functions from this
#
# x = terra::rast("D:/PEM/DateCreek_AOI/1_map_inputs/covariates/25m/dem_preproc.tif")
#
# x = terra::rast(file.path("D:/PEM_DATA/DateCreek_AOI/DateCreek_AOI/10_clean_inputs/20_covariates", "25m", "dem.tif"))
# start <- st_read("inputs/major_towns_bc.gpkg")
# origin <- start[start$NAME == "datecreek_test","geom"]
# #origin <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(892344.7, 1154340)),crs = terra::crs(x)))
# points(vect(origin))
# vec_dir <- "D:\\PEM_DATA\\DateCreek_AOI\\DateCreek_AOI\\10_clean_inputs\\10_vector\\"
# # prepare roads layer
# roads <- sf::st_read(file.path(vec_dir, "road_major.gpkg"), quiet = TRUE) %>%
#   sf::st_zm()
# library(sf)
# roads <- st_read("inputs/road_vetted.gpkg")
# plot(x)
# points(vect(origin))
# lines(vect(roads))
#

prep_cost_layers_lcp <- function(x, cost_function = "tobler offpath", neighbours = 16, roads, crit_slope = 12, max_slope = NULL, percentile = 0.5, exaggeration = FALSE, multistart =FALSE) {
  # #source("R/prep_cost_layer_LCP_utils.R")
  # x = terra::rast(file.path("D:/PEM_DATA/DateCreek_AOI/DateCreek_AOI/10_clean_inputs/20_covariates", "25m", "dem.tif"))
  # roads = st_read("D:/PEM_DATA/DateCreek_AOI/DateCreek_AOI/10_clean_inputs/10_vector/road_major.gpkg")
  # neighbours = 8
  # cost_function = "tobler offpath"
  # crit_slope = 12
  # max_slope = 40
  # percentile = 0.5
  # exaggeration = FALSE
  # multistart = TRUE

  if(terra::is.lonlat(x)) {
    stop("supplied digital elevation model (DEM) is invalid. x argument expects DEM with a projected coordinate system")
  }


  neighbours <- neighbourhood(neighbours = neighbours)


  # prepare slope component

  # identify cells and which are NA
  cells <- which(!is.na(terra::values(x)))
  na_cells <- which(is.na(terra::values(x)))


  # prepare roads layer
  roads$ROAD_CLASS[roads$trail == 1] <- "trail"
  roads <- roads[,"ROAD_CLASS"]
  colnames(roads)[1] <- "road_surface"
  # roads <- roads[,c("ROAD_SURFACE","ROAD_CLASS", "ROAD_NAME_FULL")]
  # roads <- roads %>% dplyr::rename('road_surface' = ROAD_CLASS,
  #                                  'surface' = ROAD_SURFACE,
  #                                  'name' = ROAD_NAME_FULL)
  rdsAll <-  data.table::as.data.table(roads) %>% sf::st_as_sf()


  if(multistart == FALSE){
  rSpd <- data.table::data.table(
    road_surface = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "ferry", "driveway","unclassifed"),
    speed_kmh = c(30, 30, 50, 4.5, 50, 80, 80, 50, 80, 80, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1, 4.5, 30))
  #"speed" = c(3000, 3000, 5000, 4.5, 5000, 8000, 8000, 50, 8000, 8000, 3000, 3000, 4.5, 3000, 6000, 4.5, 0.1, 3000, 4.5, 3000))
  } else {
    rSpd <- data.table::data.table(
      road_surface = c("resource", "unclassified", "recreation", "trail", "local", "collector", "highway", "service", "arterial", "freeway", "strata", "lane", "private", "yield", "ramp", "restricted", "water", "ferry", "driveway","unclassifed"),
      speed_kmh = c(30, 30, 50, 4.5, 50, 1000, 1000, 50, 1000, 1000, 30, 30, 4.5, 30, 60, 4.5, 0.1, 0.1, 4.5, 30))

    }


  rSpd[,speed := speed_kmh/3.6] ##convert to m/s

  rdsAll <- merge(rdsAll, rSpd, by = "road_surface", all = F)
  rdsAll <- rdsAll[,"speed"]

  #   # create a roads raster (buffered)
  rdsAll <- sf::st_buffer(rdsAll, dist = 25, endCapStyle = "SQUARE", joinStyle = "MITRE")
  rdsAll <- sf::st_cast(rdsAll, "MULTIPOLYGON")
  rdsRast <- terra::rasterize(rdsAll, x, field = "speed", fun = "max")
  rdsRast[is.nan(rdsRast[])] <- NA

  # identify cells and which are NA
  road_cells <- which(!is.na(terra::values(rdsRast)))

  not_road_cells <- which(is.na(terra::values(rdsRast)))

  # get cells that are adjacent (cells = cells or road_cells)
  radj <- terra::adjacent(x = rdsRast, cells = cells, directions = neighbours, pairs = TRUE)
  # remove any that are na
  radj <- radj[!radj[,2] %in% na_cells,]

  # get the actual values
  road_values <- terra::values(rdsRast)[,1]

  #Calculate the rise/run
  rspeed <- road_values[radj[,1]]

  # prepare slope component

  # identify cells and which are NA
  cells <- which(!is.na(terra::values(x)))
  na_cells <- which(is.na(terra::values(x)))

  # get cells that are adjacent
  adj <- terra::adjacent(x = x, cells = cells, directions = neighbours, pairs = TRUE)
  # remove any that are na
  adj <- adj[!adj[,2] %in% na_cells,]

  # get the actual values
  elev_values <- terra::values(x)[,1]

  message("calculating slope...")

  #Calculate the rise/run
  rise <- (elev_values[adj[,2]] - elev_values[adj[,1]])
  # calculate the distance from each the adjacent pixal
  run <- calculate_distance(x = x, adj = adj)

  mathematical_slope <- rise/run

  if(exaggeration) {
    mathematical_slope <- ifelse(mathematical_slope > 0, mathematical_slope * 1.99, mathematical_slope * 2.31)
  }

  ncells <- length(cells) + length(na_cells)

  # selectd which cost function to use
  cf <- cost(cost_function = cost_function, crit_slope = crit_slope, percentile = percentile)

  if(is.function(cost_function)) {
    message(c("Applying ", deparse(body(cost_function)[[2]]), " cost function"))
  } else{
    message(c("Applying ", cost_function, " cost function"))
  }

  speed <- cf(mathematical_slope)

  # update speed for pixals where there is a road

  rspeed[is.na(rspeed)] <- speed

  speed <- rspeed  #speed (km/h) - i think it's in m/s?

  conductance <- speed/run

  if(!is.function(cost_function)) {
    if(cost_function %in% c("campbell 2019", "campbell")) {
      # convert 30 degrees slope to percentage slope
      max_slope <- tan(30*pi/180)*100
    }
  }

  if(!is.null(max_slope)) {
    # convert percentage max slope to mathematical max slope
    # might consider putting in a steepness % of 15% as max?

    max_slope <- max_slope/100
    index <- abs(mathematical_slope) >= max_slope
    conductance[index] <- 0
  }

  cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells, sparse = TRUE)
  cs_matrix[adj] <- conductance

  cs <- list("conductanceMatrix" = cs_matrix,
             "costFunction" = cost_function,
             "maxSlope" = ifelse(!is.null(max_slope), paste0(max_slope*100, "%"), NA),
             "exaggeration" = exaggeration,
             "criticalSlope" = ifelse(test = !is.function(cost_function), yes = ifelse(test = cost_function == "wheeled transport", yes = paste0(max_slope, "%"), no = NA), no = NA),
             "percentile" = ifelse(test = !is.function(cost_function), yes = ifelse(test = cost_function == "campbell 2019", yes = percentile, no = NA), no = NA),
             "neighbours" = sum(neighbours, na.rm = TRUE),
             "resolution" = terra::res(x),
             "nrow" = terra::nrow(x),
             "ncol" = terra::ncol(x),
             "extent" = x@ptr$extent$vector,
             "crs" = terra::crs(x, proj = TRUE))

  class(cs) <- "conductanceMatrix"

  return(cs)

}

# cs <- prep_cost_layers_lcp(x, roads = roads)
# acost <- create_accum_cost_multistart(cs, origins = origin)
# plot(acost)

