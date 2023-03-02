#' Adds adjacent neighbours to transect data
#'
#' For use in the accuracy metrics this function adds all surrounding pixels to each origin pixel in the training data
#'
#' @param dat_pts training points/pixels from sampled transects
#' @param template the baseline raster template for the entire map area
#' @return an sfpoints object
#' @keywords training data, accuracy assessment, neighbours
#' @export
#' @examples
#' tpoints_ne <- add_neighbours(tpoints,trast)


add_neighbours <- function(dat_pts, template){

    dat_pts$ptsID <- 1:nrow(dat_pts)
    dat_atts <- as.data.table(st_drop_geometry(dat_pts))
    pts <- terra::vect(dat_pts)
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
    pts2 <- sf::st_as_sf(pts)
    pts2 <- as.data.table(pts2)
    setnames(pts2,c("CellNum","geometry"))
    allPts <- merge(pts2, adjLong, by = "CellNum", all = T)
    #allPts[cell_lookup, ptID := i.ID, on = c(CellNum = "cell.cell")]
    allPts <- merge(allPts, dat_atts, by.x = "ID",by.y = "ptsID",all = T)
    allPts <- terra::vect(st_as_sf(allPts))
    allPts <- sf::st_as_sf(allPts)
    return(allPts)

}

