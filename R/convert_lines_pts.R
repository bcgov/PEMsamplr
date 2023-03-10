#' Convert Line segments to training points
#'
#' @param processed_transects sf object of all attributed line segments
#' @param trast raster template with the resolution
#' @author Genevieve Perkins
#' @return sf object points based on raster
#' @export
#' @import magrittr
#' @import sf
#' @importFrom terra rast extract vect
#' @examples
#' convert(lines, trast)
#'

convert_lines_pts <- function(processed_transects, trast) {

  if ("ID" %in% colnames(processed_transects) == FALSE) {
    print ("adding ID column")
    processed_transects <- processed_transects %>%
     dplyr::mutate(ID = seq(1, length(processed_transects$order), 1))
  }

  processed_transects_id <- sf::st_drop_geometry(processed_transects)

  # transv <- terra::vect(file.path(cleandat, "proc_s1_transects_datecreek.gpkg"))

  lBuff <- processed_transects %>%
    sf::st_buffer(.,
                  dist = 2.5,
                  endCapStyle = "FLAT",
                  joinStyle = "MITRE") %>%
    sf::st_cast(., "MULTIPOLYGON")

  # convert to spatVect
  transv <- terra::vect(lBuff)

  # extract the XY values of raster cells where crossed lines
  xys <- terra::extract(trast, transv, xy = TRUE)

  # add the additional points back to full dataset
  raster_points_xy <- xys %>%
    sf::st_as_sf(coords = c("x", "y"), crs = 3005) %>%
    sf::merge(processed_transects_id) %>%
    dplyr::select(-ID,-template)

  # add slice and tid (transect id)

  allpts <- raster_points_xy %>%
    dplyr::mutate(tid = tolower(gsub("_[[:alpha:]].*", "", transect_id))) %>%
    dplyr::mutate(slice = sub('.*(?=.$)', '', gsub("\\..*", "", tid), perl = T))

  return(allpts)

}
