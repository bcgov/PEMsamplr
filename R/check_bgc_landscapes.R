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

# check the variability in landscape by BGC

check_bgc_landscapes <- function(bec, binned_landscape){

  # if not a
  vbgc <- terra::vect(bec)
  rbgc <- terra::rasterize(vbgc, landscapes, field = "MAP_LABEL")

  # stack
  rout <- c(rbgc, binned_landscape)
  routdf <- as.data.frame(rout)

  routdf <- na.omit(routdf)

  return(routdf)

}

