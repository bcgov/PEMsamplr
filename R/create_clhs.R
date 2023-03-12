#'Create CLHS sample plan
#'
#'This function takes a terra stack of covariates (and cost), and uses conditional latin hypercube sampling to create the specified number of points in slices.
#'
#'@param all_cov Terra rast of LS covariates and cost layer
#'@param num_slices Number of slices
#'@param to_include sf dataframe of already sample points to include in plan
#'@param n_points Number of points per slice
#'@param min_dist Minimum distance between points. Note that this is not guaranteed, especially with lots of points in a small area
#'@param num_sample Number of samples to run CLHS on
#'@return sf dataframe with sampled points and labels for slice number and point number
#'@author Kiri Daust
#'@import clhs
#'@import data.table
#'@importFrom terra spatSample extract
#'@import sf
#'
#'@export

###testing
# library(clhs)
# library(terra)
# library(data.table)
# library(sf)
#
# covs <- c(rast("../PEM_Data/cost.tif"),
#           rast("../PEM_Data/dah_LS.tif"),
#           rast("../PEM_Data/landform_LS.tif"),
#           rast("../PEM_Data/mrvbf_LS.tif"))
# msk <- rast("../SBSmc2_exclude_mask.tif")
# covs2 <- mask(covs, msk)
# all_cov <- covs2
# num_slices = 2
# to_include = NULL
# n_points = 5
# min_dist = 1000
# num_sample = 5000000
#
create_clhs <- function(all_cov, num_slices, to_include = NULL,
                        n_points = 5, min_dist = 600, num_sample = 5000000){
  if(num_slices < 1) stop("Hold up! Must have at least one slice.")

  layer_names <- names(all_cov)
  samp_dat <- terra::spatSample(all_cov , size = num_sample, method = "regular", xy = TRUE, as.df = F) # sample raster
  samp_dat <- samp_dat[!is.na(samp_dat[,"cost"]) & !is.infinite(samp_dat[,ncol(samp_dat)]),] ##shouldn't hard code

  coords <- samp_dat[,c("x","y")]
  curr_dat <- samp_dat[,layer_names]

  ##setup initial data
  if(is.null(to_include)){ ##nothing to include
    inc_idx <- NULL
    size = n_points
  }else{
    inc_pts <- terra::extract(all_cov, to_include) ##test this
    inc_pts <- inc_pts[,-(1)]
    inc_pts <- sf::st_as_sf(inc_pts)
    inc_idx <- 1:nrow(inc_pts)
    size = n_points + nrow(inc_pts)
    curr_dat <- rbind(to_include, curr_dat) ##comine with inlcuded data
    include_coords <- st_coordinates(to_include)
    coords <- rbind(coords, include_coords)
  }

  if(num_slices == 1){
    print("Gen-R-ating one slice...")

    for(i in 1:5){
      templhs <- clhs(curr_dat, size = size,
                      must.include = inc_idx,
                      iter = 20000 ,
                      simple = FALSE,
                      progress = TRUE,
                      cost= "cost",
                      use.cpp = T,
                      latlon = coords,
                      min.dist = min_dist)
      if(sum(templhs$final_obj_distance) == 0) break
    }


  }else{
    print("Gen-R-ating multiple slices...")
    #inc_idx = NULL
    for(snum in 1:num_slices){
      # snum = 2
      #
      for(i in 1:5){
        templhs <- clhs(curr_dat,
                        size = snum * size,
                        must.include = inc_idx,
                        iter = 20000 ,
                        simple = FALSE,
                        progress = TRUE,
                        cost= "cost",
                        use.cpp = T,
                        latlon = coords,
                        min.dist = min_dist)
        if(sum(templhs$final_obj_distance) == 0){
          break
        }else{
          cat("Points too close. Trying again...")
        }
      }
      #print(templhs$final_obj_distance)
      inc_idx <- templhs$index_samples
      # print(templhs$final_obj_distance)
      # print(inc_idx)
    }
  }
  # uncomment to plot points and check distance
  out <- as.data.table(samp_dat[templhs$index_samples,])
  out[,`:=`(slice_num = rep(num_slices:1,each = n_points),
             point_num = rep(1:n_points, times = num_slices))]
   out_sf <- st_as_sf(out,coords = c("x","y"),crs = 3005)
  # st_distance(out_sf,out_sf)
  # plot(out_sf['slice_num'], pch = 16)
  return(out_sf)
}
