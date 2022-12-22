#'Create CLHS sample plan long form method
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

library(clhs)


create_clhs_min_dist <- function(all_cov = all_cov,
                                 num_slices = 1,
                                 to_include = NULL,
                                 n_points = 5,
                                 min_dist = 600,
                                 num_sample = 5000000){

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


  for(i in 1:num_slices){ # For each slice, perform cLHS (if there is sampleable area left from previous slices)
    print("Gen-R-ating multiple slices...")

    spoints <- for(snum in 1:num_slices){

      snum = 1
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

      # check minimum distance
      clhs_sampled <- st_as_sf(samp_dat[templhs$index_samples,], coords = c("x",'y'))

      for(j in 1:nrow(clhs_sampled)){ # Filter the close together samples from the cLHS run
         #j = 1
        if(!is.na(clhs_sampled[j, ])){
          distances <- data.frame(distance = st_distance(clhs_sampled, clhs_sampled[j, ])) %>%
            tibble::rownames_to_column() %>%
            dplyr::mutate_all(as.numeric) %>%
            dplyr::filter(distance > min_dist  | distance == 0)
          clhs_sampled <- clhs_sampled[distances$rowname, ]
        }
      }

      if(length(clhs_sampled$cost)< n_points ){
        rerun <- rerun + 1
        #sample_points
        print("points selected are less than sizes less than selected, re-running clhs")
      } else {

        # if correct number of samples is selected then continue to next slice

        print("all points generated")

        #clhs_sampled_buff <- st_buffer(clhs_sampled, dist = rad_exclusion) # Extract and buffer the cLHS points
        #lays <- mask(lays, clhs_sampled_buff, inverse = TRUE)      # Mask the sampleable area
        #sample_points <- bind_rows(sample_points, clhs_sampled)
      }
    }
  }





    }

    # out <- as.data.table(samp_dat[templhs$index_samples,])
    # out[,`:=`(slice_num = rep(num_slices:1,each = n_points),
    #             point_num = rep(1:n_points, times = num_slices))]
    #


      for(j in 1:nrow(clhs_sampled)){ # Filter the close together samples from the cLHS run
        # j = 1
        if(!is.na(clhs_sampled[j, ])){
          distances <- data.frame(distance = st_distance(clhs_sampled, clhs_sampled[j, ])) %>%
            rownames_to_column() %>%
            mutate_all(as.numeric) %>%
            dplyr::filter(distance > rad_exclusion | distance == 0)
          clhs_sampled <- clhs_sampled[distances$rowname, ]
        }
      }

      # if number of sites is less than selected add a interator and this clhs willl be re-run using the repeat loop below
      if(length(clhs_sampled$slice_num)< slice_size){
        rerun <- rerun + 1
        #sample_points
        print("points selected are less than sizes less than selected, re-running clhs")
      } else {

        # if correct number of samples is selected then continue to next slice

        clhs_sampled_buff <- st_buffer(clhs_sampled, dist = rad_exclusion) # Extract and buffer the cLHS points
        lays <- mask(lays, clhs_sampled_buff, inverse = TRUE)      # Mask the sampleable area
        sample_points <- bind_rows(sample_points, clhs_sampled)
      }
    }
  }

  sample_points

  # if any of the runs do not contain the full number of samples then we will run more clhs in a loop until they contain the full number required

  if (rerun > 0) {

    tmp <- st_sfc()
    class(tmp)[1] <- "sfc_POINT" # for points
    sample_points_extra <- st_sf(DAH=integer(0),
                                 MRVBF=integer(0),
                                 LFC = integer(0),
                                 cost=integer(0),
                                 slice_num = integer(0), geometry=tmp)%>%
      st_set_crs(3005)


    for(re in 1:rerun) {
      #re = 1
      repeat{
        clhs_slice <- clhs(lays,
                           size = slice_size,
                           iter = 100000,
                           simple = FALSE,
                           progress = TRUE,
                           cost = "cost",
                           use.cpp = T) # Run cLHS on the sampleable area

        clhs_sampled <- st_as_sf(clhs_slice$sampled_data) %>%
          mutate(final_obj_continuous = clhs_slice$final_obj_continuous) %>%
          mutate(slice_num = slice_size + re)

        for(j in 1:nrow(clhs_sampled)){ # Filter the close together samples from the cLHS run
          # j = 1
          if(!is.na(clhs_sampled[j, ])){
            distances <- data.frame(distance = st_distance(clhs_sampled, clhs_sampled[j, ])) %>%
              rownames_to_column() %>%
              mutate_all(as.numeric) %>%
              dplyr::filter(distance > rad_exclusion | distance == 0)
            clhs_sampled <- clhs_sampled[distances$rowname, ]
          }
        }
        if(length(clhs_sampled$cost)== slice_size){
          print("correct number of pts generated")
          sample_points_extra <- sample_points_extra
          st_crs(sample_points_extra) <- 3005
          sample_points_extra <- rbind(clhs_sampled, sample_points_extra)
          clhs_sampled_buff <- st_buffer(sample_points_extra, dist = rad_exclusion) # Extract and buffer the cLHS points
          lays <- mask(lays, clhs_sampled_buff, inverse = TRUE)      # Mask the sampleable area for the next clhs repeat (only if successful!)

          break # stop the repeat clhs loop if corect number achieved
        }
        # if the number is still less than required repeat the above code until correct number is produced
        print("rerunning clhs")
      }
    } # repeat for number of reruns required

    sample_points <- bind_rows(sample_points, sample_points_extra )
  } # end of extra repeats to add

  xx <- sample_points
  sample_points <- xx

  #rownames(sample_points) <- NULL
  sample_points$subzone <- b
  unique_slices <- as.data.frame(table(sample_points$slice_num))
  unique_slices$order =  seq(1:num_slices)
  unique_slices$Var1 = as.numeric(as.character(unique_slices$Var1))

  sample_points <- sample_points %>%
    left_join(unique_slices, by = c("slice_num" = "Var1")) %>%
    dplyr::select(-c(Freq, slice_num)) %>%
    dplyr::rename(slice_num = order) %>%
    arrange(slice_num)
  #sort(sample_points$slice_num)

  unique_slices <- as.data.frame(table(sample_points$slice_num))

  for(i in 1:length(unique(sample_points$slice_num))){ # Adds appropriate slice number
    j <- unique(sample_points$slice_num)[i]
    if(j == unique_slices[i, 1]){
      j <- which(sample_points$slice_num == as.numeric(j))
      for(k in 1:length(j)){
        l <- j[k]
        sample_points[l, "slice_size"] <- k
      }
    }
  }

  sample_points <- mutate(sample_points, total = 1:nrow(sample_points)) %>%
    mutate(id = paste0(subzone, "_", slice_num, ".", slice_size, "_", total)) %>%
    dplyr::select(names(.)[!(names(.) %in% names(lays))])

  st_write(sample_points, dsn = file.path(clhs_outpath, paste0("clhs_pts",rot,"_", b,".gpkg")), append = T, driver = "GPKG")
  #sample_points

} # end of rotation toc()
}










































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
