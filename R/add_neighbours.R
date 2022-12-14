#' Adds adjacent neighbours to transect data
#'
#' For use in the accuracy metrics this function adds all surrounding pixels to each origin pixel in the training data
#'
#'
#'
#' @param trainpts training points/pixels from sampled transects
#' @param template the baseline raster template for the entire map area
#' @param cov_dir Folder location where the covariate/predictors rasters are stored
#'
#' @keywords training data, accuracy assessment, neighbours
#' @export
#' ##


#
add_neighbours <- function(trainpts, template, cov_dir){

    model_id <- gsub(".gpkg$", "_att.gpkg", basename(trainpoints))

    #cycle through all the different types of points


  #for(i in unattributed_files) {

    #i = unattributed_files[1]
    cov_dat <- list.files(file.path(cov_dir, res_folder), pattern = ".tif$",
               full.names = TRUE)
    template <- terra::rast(file.path(cov_dir,res_folder,"template.tif"))

    # Extra neighbouring pixals for all points
    dat_pts <- st_read(trainpts, quiet = TRUE)
    dat_pts$ptsID <- 1:nrow(dat_pts)
    dat_atts <- as.data.table(st_drop_geometry(dat_pts))
    pts <- vect(dat_pts)
    cellNums <- cells(template, pts)
    cell_lookup <- data.table(ID = pts$ptsID,cell = cellNums)

    adjCells <- adjacent(template,cells = cellNums[,2],directions = "queen",include = T)
    adjCells <- as.data.table(adjCells)
    setnames(adjCells, c("Orig",paste("Adj",1:8,sep = "")))
    adjCells[,ID := 1:nrow(adjCells)]
    adjLong <- melt(adjCells, id.vars = "ID", value.name = "CellNum", variable.name = "Position")
    setorder(adjLong,"ID","Position")
    values(template) <- 1:ncell(template)
    cellnums <- 1:ncell(template)
    template[!cellnums %in% adjLong$CellNum] <- NA
    pts <- as.points(template,values = T, na.rm = T)
    pts2 <- st_as_sf(pts)
    pts2 <- as.data.table(pts2)
    setnames(pts2,c("CellNum","geometry"))
    allPts <- merge(pts2, adjLong, by = "CellNum", all = T)
    #allPts[cell_lookup, ptID := i.ID, on = c(CellNum = "cell.cell")]
    allPts <- merge(allPts, dat_atts, by.x = "ID",by.y = "ptsID",all = T)
    allPts <- vect(st_as_sf(allPts))

    return(allPts)

}

