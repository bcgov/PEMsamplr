#' #'Vehicle Routing Problem
#' #'
#' #'@param x leastcostpath conductance matrix
#' #'@param clhs_points sample locations (sf)
#' #'@param start_pos start position (sf)
#' #'@param number_days number of days to sample for
#' #'@return list of vrp details
#' #'@import reticulate
#' #'@import sf
#' #'@import terra
#' #'@import foreach
#' #'@import leastcostpath
#' #'@author Kiri Daust
#' #'
#' #'@export
#'
#' # ###Example for testing
#' # library(sf)
#' # library(gdistance)
#' # library(sp)
#' # library(foreach)
#' # tr1 <- readRDS("inputs/transition_layer.rds")
#' # pnts <- st_read("inputs/ICHmc1_clhs_sample_1.gpkg")
#' # plot(pnts)
#' # startLoc <- pnts[10,]
#' # pnts_in <- pnts[-10,]
#' # test <- create_vrp(tr1,pnts_in, startLoc, 3)
#'
#'
#' #tr1 <- gdistance::transition(cost,transitionFunction = function(x)1/mean(x),8)
#' #test <- terra::costDistance(cost, target = 3000)
#' # library(leastcostpath)
#' # cs <- create_cs(cost)
#' # dist <- create_FETE_lcps(cs,pnts,cost_distance = T)
#'
#' #load("Conductance_Layer.rdata")
#' # x <- cs
#' # clhs_points <- test
#' # start_pos <- origin
#' # number_days <- 5
#'
#' # library(PEMsamplr)
#' # vrp_res <- create_vrp(cs,clhs_points = test,start_pos = origin, number_days = 5)
#'
#'
#' create_vrp <- function(x, clhs_points, start_pos, number_days){
#'
#'   message("sourcing python...")
#'   reticulate::source_python("R/mTSP.py")
#'   clhs_points$PID <- seq_along(clhs_points$geom)
#'   clhs_points <- clhs_points['PID']
#'   start_pos$PID <- -1
#'   clhs_points <- rbind(clhs_points, start_pos['PID'])
#'
#'   temp <- lcp_dist_mat(x, clhs_points, clhs_points)
#'   dMat2 <- as.matrix(temp)
#'   dMat2[is.infinite(dMat2)] <- NA
#'
#'   infpts <- colSums(dMat2, na.rm = T)
#'   dMat2<- dMat2[-which(infpts == 0),-which(infpts == 0)]
#'   clhs_points <- clhs_points[-which(infpts == 0),]
#'   dMat2 <- dMat2/60
#'
#'   #maxTime <- as.integer(min(dMat2[upper.tri(dMat2)])*5+45*5)
#'   maxTime <- as.integer(8*60)
#'   objVals <- as.integer(rep(maxTime*60L*100,nrow(dMat2)-1))
#'   ndays <- as.integer(number_days)
#'   indStart <- as.integer(rep(nrow(dMat2)-1,ndays))
#'
#'   message("Running VRP...")
#'   vrp <- py_mTSP(dat = dMat2,num_days = ndays,
#'                  start = indStart, end = indStart,
#'                  max_cost = maxTime, plot_time = 45L,
#'                  penalty =  objVals, arbDepot = F,GSC = 1L)
#'
#'   result <- vrp[[1]]
#'
#'   message("Creating Spatial paths...")
#'   paths <- foreach(j = 0:(length(result)-1), .combine = rbind) %do% {
#'     if(length(result[[as.character(j)]]) > 2){
#'       message("Day ",j,"...")
#'       p1 <- result[[as.character(j)]]+1
#'       out <- foreach(i = 1:(length(p1)-1), .combine = rbind) %do% {
#'         temp1 <- clhs_points[p1[i],]
#'         temp2 <- clhs_points[p1[i+1],]
#'         temp3 <- leastcostpath::create_lcp(x,origin = temp1,destination = temp2)
#'         temp3$Segment = i
#'         temp3
#'       }
#'       out$DayNum = j
#'       out
#'     }
#'
#'   }
#'
#'   #paths <- st_transform(paths, 3005)
#'   plot(paths['DayNum'])
#'
#'   p2 <- clhs_points
#'   p2$PID <- seq_along(p2$PID)
#'   p2 <- p2[,"PID"]
#'   p2$DayNumber <- NA
#'   p2$Order <- NA
#'   for(i in 0:(length(result)-1)){
#'     p1 <- result[[as.character(i)]]+1
#'     p1 <- p1[c(-1,-length(p1))]
#'     p2$DayNumber[p1] <- i
#'     p2$Order[p1] <- 1:length(p1)
#'   }
#'   p2$StartLocation <- F
#'   p2$StartLocation[nrow(p2)] <- T
#'   if(any(is.na(p2$DayNumber[!p2$StartLocation]))) warning("Could not fit all points. Try increasing number of days or check distances")
#'   #p2 <- st_transform(p2, st_crs(paths))
#'   plot(p2["DayNumber"],pch = 16)
#'   #plot(paths, add = T)
#'   outvrp <- list(Path = paths, Points = p2)
#'   return(outvrp)
#' }
#'
#'
#' # setup_python <- function(){
#' #   reticulate::conda_create("pemr")
#' #   enviros <- reticulate::conda_list()
#' #   reticulate::use_python(enviros[enviros$name == "pemr","python"])
#' #   reticulate::conda_install("ortools",envname = "pemr", pip = T)
#' #   reticulate::py_module_available("ortools")
#' # }
#'
