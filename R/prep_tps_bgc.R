#' Prepare training pts bgcs
#'
#' @param allpts attributed points
#' @param mapkey mapkey
#' @param field string field withtin the mapkey to model, default = "FULL"
#' @param bec_path string filepath to bec.gpkg
#' @param min_no minimum number of points for a map unit, default = 10
#'
#' @return datatable points
#' @export
#'
#' @examples
#' prep_tps_bgc(allpts, mapkey, field = "Full",bec_path = file.path(fid$shape_dir_1010[2], "bec.gpkg"),min_no = 10)

prep_tps_bgc <- function(allpts,
                         mapkey,
                         field = "Full",
                         bec_path = file.path(fid$shape_dir_1010[2], "bec.gpkg"),
                         min_no = 10){

  # read in bec data
  bec <- sf::st_read(bec_path) %>% sf::st_cast(., "MULTIPOLYGON")

  # define units to map using mapkey file
  mpts <- define_mapunits(allpts, mapkey, field)

  # match to the key and filter for forest and non_forest points
  subzones <- unique(bec$MAP_LABEL)
  subzones <- tolower(gsub("\\s+","",subzones))

  out <- mpts %>%
    cbind(st_coordinates(.)) %>%
    mutate(fnf = ifelse(grepl(paste0(subzones, collapse = "|"), tolower(mapunit1)), "forest", "non_forest"))

  if(!"MAP_LABEL" %in% names(out)){
    out <- out %>%
      st_join(st_transform(bec[, "MAP_LABEL"], st_crs(.)), join = st_nearest_feature)
  }

  out <- out %>%
    st_drop_geometry() %>%
    dplyr::select(fnf, everything()) %>%
    dplyr::rename(bgc_cat = MAP_LABEL) %>%
    dplyr::rename_all(.funs = tolower)

  # this is a fix for Date Creek Data
  if("stuc_stage" %in% names(out)){
    out <- out %>%
      dplyr::rename("struc_stage" = stuc_stage,
                    "struc_mod" = stuc_mod)

  }

  tpts <- out %>%
    mutate(mapunit1 = as.factor(mapunit1),
           mapunit2 = as.factor(mapunit2))

  # check data type
  tpts <- tpts %>%
    filter(data_type != "repeat") %>%
    filter(data_type != "incidental")

  # check minimum number of mapunits and filter out

  tpts1 <- filter_min_mapunits(tpts, min_no = min_no)

  return(tpts1)
}

