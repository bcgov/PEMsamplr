#' Check BGC sample cost
#'
#' Assess the costs of sampling areas for each BGC to help assess how well the cost layer describes to study area
#'
#' @param bgc Biogeoclimatic classification vector (sf polygon/multipolygon)
#' @param binned_landscape Raster (SpatRast) with binned landscapes, output of create_binned_landscape() function
#' @param cost Raster layer (SpatRast) with cost layer
#' @return data.frame
#' @export
#'
#' @examples
#'check_bgc_cost(bgc, binned_landscape, cost)
#'
check_bgc_cost <- function(bgc, binned_landscape, cost){

  # create a bgc raster
  rbgc <- terra::rasterize(bgc, binned_landscape, field = "MAP_LABEL", fun= "min")
  rcost <- c(rbgc, binned_landscape, cost)

  rcdf <- as.data.frame(rcost, xy = TRUE)
  rcdf <- na.omit(rcdf)
  rcdf <- rcdf %>% dplyr::select(-c(x, y))

  #names(rcdf) = c("BGC_LABEL","landscape_variable_validation", "cost")

  rcdf_class <- rcdf %>%
    dplyr::mutate(cost_code = dplyr::case_when(
      cost < 250 ~ "low",
      cost > 250 & cost < 500 ~ "moderate",
      cost > 500 & cost < 800 ~ "high",
      cost > 800 & cost < 1000 ~ "very high",
      cost > 1000 ~ "prohibative",
      TRUE ~ as.character("unknown")
    ))

  return(rcdf_class)

}

