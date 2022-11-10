
# check the variability in landscape by BGC

#
#
# bgc = bgc file (shape/geo)
#
#
# bgc <- sf::st_read("D:\\PEM_DATA\\PEMsamplr\\temp\\bec.gpkg")
# landbins <- create_landscape_bins(input <- c("D:\\PEM_DATA\\PEMprepr\\temp\\covar\\dah_LS.tif",
#     "D:\\PEM_DATA\\PEMprepr\\temp\\covar\\mrvbf_LSss.tif",
#     "D:\\PEM_DATA\\PEMprepr\\temp\\covar\\landform_LSss.tif"))
#
#
#
# check_bgc_cost <- function(bgc, landbins){
#
#   # if not a
#   vbgc <- terra::vect(bgc)
#   rbgc <- terra::rasterize(vbgc,landbins, field = "BGC_LABEL")
#
#
#
# # get histograms per BGC
# # stack the BGC along with the landscape variable validation classes
# fileoi <- c("bgc.tif", "landscape_variable_validation.tif")
#
# covariates <- list.files(saga_files,full.names = T)
# covariatesoi <- covariates[basename(covariates) %in% fileoi]
# ancDat <- raster::stack(covariatesoi)
#
# ancDat.df <- as.data.frame(ancDat)
#
# # select BGCs for SE KIC
# #ancDat.df <- ancDat.df %>% filter (bgc %in% c(20,4,21,16,23,14,26,10)) # SE
# #ancDat.df <- ancDat.df %>% filter(bgc %in% c(139, 103, 155, 163)) # SW
# #ancDat.df <- ancDat.df %>% filter(bgc %in% c(157, 158, 83, 134, 148, 141)) # NE
# #ancDat.df <- ancDat.df %>% filter(bgc %in% c(157, 158, 83, 134, 148, 141)) # NW
# ancDat.df <- ancDat.df %>% filter(bgc %in% c(99, 98, 165, 152, 162, 119)) # NW
#
#
# ancDat.df <- ancDat.df %>% filter(bgc %in% c(99, 98, 165, 152, 162, 119)) # NW
#
#
# ancDat.df <- na.omit(ancDat.df)
#
#
# p1 <- ggplot(ancDat.df, aes(landscape_variable_validation)) +
#   geom_histogram()+
#   facet_wrap(~bgc)
#
# p1
#
#
