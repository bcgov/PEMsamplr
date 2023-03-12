#' Check landscapes per BGC
#'
#' Assess how well the binned landscape are described for each of the BEC units of
#' interest
#'
#' @param bec vector spatial data (geopackage) of BEC within the study area
#' @param binned_landscape spatrast generated using the create_binned_landscape function
#' @return **data frame**
#' @export
#'
#' @examples
#'
#' check_bgc_landscapes(bec, binned_landscape)


check_bgc_landscapes <- function(bec, binned_landscape){

  # if not a
  if(any((class(bec) == "character"))) {

    sfbgc <- sf::st_read(bec, quiet = T)

  } else {

    sfbgc = bec
  }
  vbgc <- terra::vect(sfbgc)
  rbgc <- terra::rasterize(vbgc, landscapes, field = "MAP_LABEL")

  # stack
  rout <- c(rbgc, binned_landscape)
  routdf <- as.data.frame(rout)

  routdf <- na.omit(routdf)

  return(routdf)

}

