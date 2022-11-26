#' Generate landscape level covariate rasters for cLHS sample plan
#'
#' Generates base landscape covariates from a 25m TRIM DEM using SAGA GIS and converts to classes for use in the sample plan cLHS
#'
#' This script has been tested with SAGA 8.4 on Windows
#' Depending on your system the path to `saga_cmd` may need to be specified.
#'
#'
#' @param dtm is a 25m dtm raster object ideally cropped to the AOI watershed boundaries
#' @param SAGApath Is the location of SAGA on your system.  On linux systems with SAGA GIS installed Use `SAGApath = ""`
#' @param output Location of where rasters will be saved.
#' @param layers The covariates that will be generated.  A full list of covariates is listed at: ADD
#' @param sieve_size Remove isolated clusters of below the threshold number of cells
#'
#' @keywords SAGA, covariates, predictors, raster
#' @export
#' ##

# setwd("D:/GitHub/PEMsamplr")
# dtm <- ("./temp_data/dem.tif")
# SAGApath <- "C:/SAGA/"
# layers = "all"
# output = "./landscape_covariates"
# sieve_size = 10
#   unattributed_files <- list.files(output_cleaned_dir, pattern = ".gpkg$", full.names = TRUE)
#   #unattributed_files <- unattributed_files[4:8]
#
#   template <- terra::rast(file.path(cov_dir, res_folder,"template.tif"))
#
#   cov_dir2 <- list.files(file.path(cov_dir, res_folder), pattern = ".tif$",
#                              full.names = TRUE)
#   att_folder <- file.path(final_path, paste0("att_",res_folder))
#   if(!dir.exists(att_folder)) dir.create(att_folder, recursive = TRUE)
#
#
# add_neighbours <- function(trainpts, template, cov_dir){
#
#     model_id <- gsub(".gpkg$", "_att.gpkg", basename(trainpoints))
#
#     #cycle through all the different types of points
#
#
#   #for(i in unattributed_files) {
#
#     #i = unattributed_files[1]
#     cov_dat <- list.files(file.path(cov_dir, res_folder), pattern = ".tif$",
#                full.names = TRUE)
#     template <- terra::rast(file.path(cov_dir,res_folder,"template.tif"))
#
#     # Extra neighbouring pixals for all points
#     dat_pts <- st_read(trainpts, quiet = TRUE)
#     dat_pts$ptsID <- 1:nrow(dat_pts)
#     dat_atts <- as.data.table(st_drop_geometry(dat_pts))
#     pts <- vect(dat_pts)
#     cellNums <- cells(template, pts)
#     cell_lookup <- data.table(ID = pts$ptsID,cell = cellNums)
#
#     adjCells <- adjacent(template,cells = cellNums[,2],directions = "queen",include = T)
#     adjCells <- as.data.table(adjCells)
#     setnames(adjCells, c("Orig",paste("Adj",1:8,sep = "")))
#     adjCells[,ID := 1:nrow(adjCells)]
#     adjLong <- melt(adjCells, id.vars = "ID", value.name = "CellNum", variable.name = "Position")
#     setorder(adjLong,"ID","Position")
#     values(template) <- 1:ncell(template)
#     cellnums <- 1:ncell(template)
#     template[!cellnums %in% adjLong$CellNum] <- NA
#     pts <- as.points(template,values = T, na.rm = T)
#     pts2 <- st_as_sf(pts)
#     pts2 <- as.data.table(pts2)
#     setnames(pts2,c("CellNum","geometry"))
#     allPts <- merge(pts2, adjLong, by = "CellNum", all = T)
#     #allPts[cell_lookup, ptID := i.ID, on = c(CellNum = "cell.cell")]
#     allPts <- merge(allPts, dat_atts, by.x = "ID",by.y = "ptsID",all = T)
#     allPts <- vect(st_as_sf(allPts))
#
#     return(allPts)
#
# }

