#
# require(doParallel)
# registerDoParallel(cl)
#
#
# create_vrp <- function(pnts, outdir ){
#
#
#   pnts <- sample_points # starting location
#   start <-
#   out_dir <-
#
#
#   pnts <- tibble::rowid_to_column(pnts, "name")
#   pnts_sub <- pnts["name"]
#
#   start <- st_as_sfc(start)
#   names(start) = "name"
#   startPnts <- st_as_sf(data.frame(name = "Start", geometry = start))
#   pnts <- rbind(pnts_sub, startPnts) # add start to the list of points
# pnts2 <- as(pnts, "Spatial")   # convert to spatial points data frame
#
# ## create distance matrix between sample points
# test <- costDistance(tr1, pnts2, pnts2) # use transition layer from accum cost layer
# dMat2 <- as.matrix(test)
# dMat2 <- dMat2*60 # convert to
# dMat2[is.infinite(dMat2)] <- 1000
#
# ##penalty based on quality of points
# objVals <- sample_points_all$final_obj_continuous
# objVals <- max(objVals) - objVals
#
# maxTime <- 10L ##hours
# ## time per transect
# plotTime <- 45L ##mins
#
# minPen <- (maxTime*60L)/2L
# maxPen <- (maxTime*60L)*2L
# objVals <- scales::rescale(objVals, to = c(minPen,maxPen))
# objVals <- as.integer(objVals)
#
# n = nrow(dMat2)-1
# ndays <- as.integer(length(objVals)/4)
# indStart <- as.integer(rep(n,ndays))
#
# vrp <- py_mTSP(dat = dMat2, num_days = ndays, start = indStart, end = indStart,
#                max_cost = maxTime*60L, plot_time = plotTime,
#                penalty =  objVals, arbDepot = F, GSC = 8L)
#
# result <- vrp[[1]]
#
#
# ## create spatial paths
# paths <- foreach(j = 0:(length(result)-1), .combine = rbind) %do% {
#   #j = 1
#   if(length(result[[as.character(j)]]) > 2){
#     cat("Drop site",j,"...\n")
#     p1 <- result[[as.character(j)]]+1
#     out <- foreach(i = 1:(length(p1)-1), .combine = rbind) %do% {
#       #i = 2
#       temp1 <- pnts[p1[i],]
#       temp2 <- pnts[p1[i+1],]
#       temp3 <- shortestPath(tr1, st_coordinates(temp1),
#                             st_coordinates(temp2),output = "SpatialLines") %>% st_as_sf()
#       temp3$Segment = i
#       temp3
#     }
#     out$DropSite = j
#     out
#   }
#
# }
#
# paths <- st_transform(paths, 3005)
# st_write(paths, dsn = file.path(out_path, "TSP_3.gpkg"), layer = "Paths", append = T, driver = "GPKG")
#
# ## label points
# p2 <- pnts
# p2$PID <- seq_along(p2$name)
# p2 <- p2[,"PID"]
# p2$DropLoc <- NA
# p2$Order <- NA
# for(i in 0:(length(result)-1)){
#   p1 <- result[[as.character(i)]]+1
#   p1 <- p1[-1]
#   p2$DropLoc[p1] <- i
#   p2$Order[p1] <- 1:length(p1)
# }
# p2 <- st_transform(p2, 3005)
# st_write(p2, dsn = file.path(out_path, "TSP_3.gpkg"),layer = "Points", append = T,overwrite = T, driver = "GPKG")
# }
