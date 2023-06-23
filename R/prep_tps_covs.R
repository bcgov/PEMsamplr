#' Prepare training pts bgcs
#'
#' @param tpts dataframe containing attributed and bgc selected points
#' @param mcols string list of cov name
#' @param outdir location of output files for model, default selected based on strandard stucture
#'
#' @return dataframe of final training pts
#' @export
#'
#' @examples
#' prep_tps_covs(tpts, mcols, outdir = fid$model_inputs0310[2])

prep_tps_covs <- function(tpts, mcols, outdir = fid$model_inputs0310[2]){

  mpts <- tpts %>%
    dplyr::select(-c(order, point_type, observer, transition, data_type, struc_stage, struc_mod, date_ymd, time_hms, edatope, comments, photos)) %>%
    dplyr::select(id, fnf, x, y, bgc_cat, mapunit1, mapunit2, position, transect_id, tid, slice, any_of(mcols))

  lcols <- names(mpts)[12:length(names(mpts))]
  saveRDS(lcols, file.path(outdir, "full_covariate_list.rds"))

  write.csv(mpts, file.path(outdir, "training_pts.csv"),row.names = FALSE)

  return(mpts)

}
