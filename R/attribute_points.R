#' Add covariate predictor data to training point file
#'
#' Generates base landscape covariates from a 25m TRIM DEM using SAGA GIS and converts to classes for use in the sample plan cLHS
#'
#' This script has been tested with SAGA 8.4 on Windows
#' Depending on your system the path to `saga_cmd` may need to be specified.
#'
#'
#' @param training a file gpkg of training points
#' @param cov_dir folder containing covariate files
#' @param res_dir resolution subfolder
#' @param output Location of where attributed training file will be exported

#'
#' @keywords SAGA, covariates, predictors, raster
#' @export
#' ##

# setwd("D:/GitHub/PEMsamplr")
# dtm <- ("./temp_data/dem.tif")
# SAGApath <- "C:/SAGA/"
# layers = "all"
# output = "./landscape_covariates"
# sieve_size = 10

attibute_points <- function(training, cov_dir, res_dir){

    cov_dat <- rast(list.files(file.path(cov_dir, res_dir), pattern = ".tif$",
                             full.names = TRUE))

    atts <- terra::extract(cov_dat, allPts)

  att_all <- cbind(st_as_sf(allPts), atts)

  # TO DO
  # need to remove the id and rename id for each point?

  # fix names abreviuation problem
  names(att_all)
  new_names <- gsub(".tif", "", list.files(file.path(cov_dir, res_folder), pattern = ".tif$"))
  if(names(att_all))

    #st_write(att_all,"s1_clean_neighbours_allatts.gpkg") ###final dataset

    st_write(att_all, dsn = file.path(final_path, paste0("att_",res_folder), model_id), delete_layer = TRUE)


}

