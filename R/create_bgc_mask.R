#' Create cost no sample area
#'
#' Creates and applies a mask to permanently changed areas where sample is not be conducted.
#' This includes (lakes, and permanently changed landscape i.e roads)
#'
#' @param vec_dir text string with folder location of base vector layers
#' @param cost_masked SpatRast of cost layer with high cost penalty applied
#' @param out_dir text string with location of output vector layers
#' @importFrom magrittr "%>%"
#' @return **SpatRast** A masked cost raster layer
#' @export
#'
#' @examples
#' create_cost_nosample <- function(vec_dir, cost)
#'

create_bgc_mask <- function(vec_dir, cost_masked, out_dir) {

  # exclude water:
  bec<- sf::st_read(file.path(vec_dir, "bec.gpkg"))

  boi <- unique(bec$MAP_LABEL)

  for (b in boi) {
    # b = boi[1]

    subzone <- bec %>%
      dplyr::filter(MAP_LABEL %in% b)

    subzone_buff <- sf::st_buffer(subzone, dist = -150)

    boi_mask <- terra::mask(cost_masked, subzone_buff)
    names(boi_mask) = 'cost'
    boi_mask <- 1 + (boi_mask *0)
    terra::writeRaster(boi_mask, file.path(out_dir, paste0(b,"_exclude_mask.tif")), overwrite = TRUE)

    mask_poly_boi <- terra::as.polygons(boi_mask, dissolve = TRUE)
    mask_poly_boi <- st_as_sf(mask_poly_boi)
    st_write(mask_poly_boi, file.path(out_dir,paste0(b, "_exclude_poly.gpkg")), delete_layer = TRUE)
  }
  return(TRUE)

}
