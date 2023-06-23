#' Prepare training pts attribute
#'
#' Prepares training points by adding neighbours, attributing values
#'
#' @param clean_dir location of clean data point file
#' @param res numeric resolution of data to be attributed
#' @param cov_dir location of output file
#' @importFrom terra rast
#' @importFrom sf st_read
#' @return datatable with training points
#' @export
#'
#' @examples
#' prep_tps_att (clean_dir = fid$trainpts_maps[2], res = 5,cov_dir = fid$cov_dir_1020[2])

prep_tps_att <- function(clean_dir = fid$trainpts_maps[2],
                              res = 5,
                              cov_dir = fid$cov_dir_1020[2]){

  res_folder = paste0(res,"m")

  # read in the raster template used for modelling (i.e 5m resolution)
  trast <- terra::rast(file.path(cov_dir, res_folder, "template.tif"))

  processed_transects <- sf::st_read(file.path(clean_dir, "proc_s1_transects.gpkg"))

  # convert lines to points
  allpts <- convert_lines_pts(processed_transects, trast)

  ## Add neighbours
  tpoints_ne <- add_neighbours(allpts,trast)

  print("adding neighbours")
  # attribute points
  allrasts <- file.path(cov_dir, res_folder)
  allpts <- attribute_points(tpoints_ne, allrasts)

  st_write(allpts, dsn = file.path(fid$trainpts_att[2], "allpts.gpkg"), delete_layer = TRUE)

  return(allpts)
}
