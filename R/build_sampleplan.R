#' build_sampleplan
#'
#' @param boi string with name of bgc varies ie: "ICHmc2"
#' @param clhs_set sring name of repreat clhs output to use in final sample plan, ie: "ICHmc2_clhs_sample_3.gpkg"
#' @param sampleplan_input filepath where repeat clhs outputs are saved
#' @param basedata_input filepath of base sample 1 inputs, root folder for landscape covariates and exclusion folders
#' @param output  filepath where gpkg with transects are saved
#' @param outname string output file name, default is "s1_sampling"
#' @importFrom sf st_read st_crs st_layers
#' @importFrom magrittr "%>%"
#' @importFrom terra rast
#' @return sf final sampleplan
#' @export
#'
#' @examples
#' build_sampleplan("ICHmc2", "ICHmc2_clhs_sample_3.gpkg", sampleplan_input = fid$samplingplan_clhs[2],
#'                 basedata_input = fid$sampling_input_2010[2]
#'                 out_path <- fid$sampleplan_final_transect[2]),outname = "s1_sampling")

build_sampleplan = function(boi, clhs_set, sampleplan_input, basedata_input, output, outname = "s1_sampling"){

#boi <- "ICHmc2"
#clhs_set <- "ICHmc2_clhs_sample_3.gpkg"
#out_path <- fid$sampleplan_final_transect[2]
#sampleplan_input = fid$samplingplan_clhs[2]
#basedata_input = fid$sampling_input_2010[2]
#outname = "s1_sampling"

# define selected clhs option
sample_points <- sf::st_read(list.files(sampleplan_input, pattern = clhs_set, full.names = TRUE))

# read in cost layer
cost <- terra::rast(file.path(basedata_input, "landscape_covariates", "cost.tif"))

# read in mask_poly
mask_poly <- sf::st_read(list.files(file.path(basedata_input, "exclusion"),
                                    pattern = paste0(boi,"_exclude_poly.gpkg"), full.names = TRUE))
  st_crs(mask_poly) <- 3005

# Generate the transects and poinst and output as geopacakage
build_site_transects(sample_points, cost, mask_poly, centroid_distance = 400, out_path)

# export a tracking sheet for the samples
allpoints <- grep("points_all", sf::st_layers(file.path(out_path, paste0(outname,".gpkg")))$name, value = T)
return(allpoints)

}
