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
# all_cov <- lays
# num_slices = 3
# to_include = NULL


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

    spoints <- clhs(s, size = size,
                    must.include = inc_idx,
                    iter = 10000 ,
                    simple = FALSE,
                    progress = TRUE,
                    cost= "cost",
                    use.cpp = T,
                    latlon = coords,
                    min.dist = min_dist)

  }else{
    print("Gen-R-ating multiple slices...")
    spoints <- for(snum in 1:num_slices){
     # snum = 1
      templhs <- clhs(curr_dat,
                      size = n_points * snum,
                      must.include = inc_idx,
                      iter = 5000 ,
                      simple = FALSE,
                      progress = TRUE,
                      cost= "cost",
                      use.cpp = T,
                      latlon = coords,
                      min.dist = min_dist)
      inc_idx <- templhs$index_samples
      # print(templhs$final_obj_distance)
      # print(inc_idx)
    }
  }
  out <- as.data.table(samp_dat[templhs$index_samples,])
  out[,`:=`(slice_num = rep(num_slices:1,each = n_points),
            point_num = rep(1:n_points, times = num_slices))]
  out_sf <- st_as_sf(out,coords = c("x","y"),crs = 3005)
  return(out_sf)
}
