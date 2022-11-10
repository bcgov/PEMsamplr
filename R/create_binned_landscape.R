#' Create binned landscape
#'
#' Create a landscape with binned variables to assess the landscape environmental space.
#'
#' Can use either folder or individual files to assess landscape covariate space
#'
#' @param input text string with either folder location or individual files
#'
#' @return **SpatRaster**
#'
#' @export
#'
#' @examples
#'
#' create_landscape_bins()

create_landscape_bins <- function(input){

  # specify the folder
  # if folder then do this

  suppressWarnings({
    if(dir.exists(input) & length(input) == 1){
      print ("using entire directory")

      rastlist <- list.files(input, full.names = T, include.dirs = FALSE, pattern = "\\..*")
      ancDat <- terra::rast(rastlist)


    } else { print("using individual files")
      # specify the names
      # if names then do this....

      ancDat <- terra::rast(input)

    }
  })

  # find unique combinations and assign an id column
  combinations <- terra::unique(ancDat)
  comb.df <- as.data.frame(combinations)
  comb.df <- na.omit(comb.df) # remove NA values

  comb.df$id = seq(1,length(comb.df[,1]),1)
  ancDat.df <- as.data.frame(ancDat, xy = TRUE)
  anc_class <- dplyr::left_join(ancDat.df, comb.df)

  out_rast <- terra::rast(anc_class, type="xyz", crs= terra::crs(ancDat), digits=6)

  return(out_rast)

}
