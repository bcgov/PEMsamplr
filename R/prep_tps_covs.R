#' Prepare training pts bgcs
#'
#' @param tpts dataframe containing attributed and bgc selected points
#' @param mcols string list of cov name
#' @param res resolution at which modelling will be done default = 5
#' @param corr_val numeric values of correlated cutoff, default = 0.7
#' @param corr_plot TRUE/FALSE default = FALSE
#' @param rastdir location of covarariates root folder, will use res to determine covars
#' @param outdir location of output files for model, default selected based on strandard stucture
#' @return dataframe of final training pts and output csv with reducedlist of covars
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#' @importFrom terra spatSample rast
#' @importFrom sf st_as_sf st_read st_buffer st_cast
#' @export
#'
#' @examples
#' final_tp <- prep_tps_covs(tpts, mcols,res = 5 ,corr_val = 0.7, corr_plot = TRUE,rastdir = fid$cov_dir_1020[2],outdir = fid$model_inputs0310[2])

prep_tps_covs <- function(tpts, mcols,res = 5 , corr_val = 0.7, corr_plot = TRUE,
                          rastdir = fid$cov_dir_1020[2],
                          outdir = fid$model_inputs0310[2]){
  # tpts = tpts
  # mcols = mcols
  # res = 5
  # corr_val = 0.7
  # corr_plot = TRUE
  # rastdir = fid$cov_dir_1020[2]
  # outdir = fid$model_inputs0310[2]


  res_size = paste0(res, "m")
  temp_rast_dir <- file.path(rastdir,res_size)
  rast_list <- list.files(file.path(temp_rast_dir), pattern = ".sdat$|.tif$", full.names = T, recursive = T)
  rast_list <- rast_list[tolower(gsub(".sdat$|.tif$", "", basename(rast_list))) %in% tolower(mcols)]

  # read in the raster template used for modelling (i.e 5m resolution)
  rstack <- terra::rast(rast_list)

  subsmpl <- terra::spatSample(rstack, size = 1000000, method = "regular", xy = FALSE, na.rm = TRUE) # sample raster
  reduced_vars <- reduce_features(subsmpl, cutoff = corr_val, corr_plot = corr_plot)

  write.csv(reduced_vars, file.path(outdir, "reduced_covariate_list.csv"),row.names = FALSE)

  # select covars and format training pts
  mpts <- tpts %>%
    dplyr::select(-c(order, point_type, observer, transition, data_type, struc_stage, struc_mod, date_ymd, time_hms, edatope, comments, photos)) %>%
    dplyr::select(id, fnf, x, y, bgc_cat, mapunit1, mapunit2, position, transect_id, tid, slice, any_of(reduced_vars))

  return(mpts)

}

