#' Add covariate predictor data to training point file
#'
#' @param dat_pts a file gpkg of training points
#' @param cov_dir folder containing covariate files
#' @return an sf object
#' @keywords SAGA, covariates, predictors, raster
#' @import sf
#' @import terra
#' @export
#' @examples
#' tpoints_ne <- attribute_points(dat_pts, cov_dir)

attribute_points <- function(dat_pts, cov_dir){

  if(class(cov_dir) == "character") {
    print ("reading in raster stack")
    lor <- list.files(cov_dir, pattern = ".sdat$", full.names = T, recursive = T)
    cov_dir <- terra::rast(lor)

  }

  atts <- terra::extract(cov_dir, dat_pts)
  att_all <- cbind(st_as_sf(dat_pts), atts)

  return(att_all)

#   # TO DO
#   # need to remove the id and rename id for each point?
#
#   # fix names abreviuation problem
#   names(att_all)
#   new_names <- gsub(".tif", "", list.files(file.path(cov_dir, res_folder), pattern = ".tif$"))
#   if(names(att_all))

 }
