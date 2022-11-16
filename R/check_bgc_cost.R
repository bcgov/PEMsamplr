#' Check BGC sample cost
#'
#' Assess the costs of sampling areas for each BGC to help assess how well the cost layer describes to study area
#'
#' @param bgc Biogeoclimatic classification vector (sf polygon/multipolygon)
#' @param binned_landscape Raster (SpatRast) with binned landscapes, output of create_binned_landscape() function
#' @param cost Raster layer (SpatRast) with cost layer
#'
#' @return data.frame
#' @export
#'
#' @examples
#'check_bgc_cost(bgc, binned_landscape, cost)
#'

#
# temp <- file.path("temp", "date")
#
# cov <- "D:\\PEM_DATA\\BEC_DevExchange_Work\\KIC_SE_AOI\\1_map_inputs\\covariates\\25m_trim\\"
# costdir <- "D:\\PEM_DATA\\BEC_DevExchange_Work\\KIC_SE_AOI\\2_sample_design\\stage1_StudyDesign\\input_raster\\"
# bgcdir <- "D:\\PEM_DATA\\BEC_DevExchange_Work\\KIC_SE_AOI\\0_raw_inputs\\base_layers"
#
# binned_landscape <- terra::rast(file.path(cov, "landscape_variable_validation.tif"))
# cost <- terra::rast(file.path(costdir, "acost.tif"))
# names(cost) = "cost"
# #bgc <- terra::vect(file.path(bgcdir, "bec.gpkg"))
# bgc <- sf::st_read(file.path(bgcdir, "bec.gpkg"))

# post porcessing for the Vinette
#   p3 <- ggplot2::ggplot(rcdf_class, ggplot2::aes(landscape_variable_validation, fill = cost_code)) +
#     ggplot2::geom_histogram(bins = 30)
#
#
#   p2 <- ggplot(all3, aes(landscape_variable_validation, fill = cost_code)) +
#     geom_histogram()+
#     facet_wrap(~bgc)
#
#   p2
#
# }


check_bgc_cost <- function(bgc, binned_landscape, cost){

  # Check the format is sf or SpatVect
  # class(bgc)[1]

  # create a bgc raster
  rbgc <- terra::rasterize(bgc, binned_landscape, field = "BGC_LABEL")
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

