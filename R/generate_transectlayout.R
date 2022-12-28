#' Generate Transect layout
#'
#' @param input_path filepath location where final sample plan or plans are stored
#
#' @author Genevieve Perkins
#' @import foreach
#' @import sf
#' @return sf vector of lines
#' @export
#'
#' @examples
#' create_transectlayout("inputfolder")
#'

generate_transectlayout <- function(input_path){

  trans <- list.files(input_path, pattern = ".gpkg$", full.names = TRUE, recursive = FALSE)

  transect_layout <- foreach(x = trans, .combine = rbind) %do% {
    clhs_layers <- sf::st_layers(x)
    lines <- which(clhs_layers[["geomtype"]] %in% c("Line String", "Multi Line String"))
    if(length(lines)) {
      transects <- foreach(y = clhs_layers$name[lines], .combine = rbind) %do% {
        transect <- sf::st_read(x, y, quiet = TRUE) %>%
          dplyr::rename_all(dplyr::recode, geom = "geometry") %>%
          dplyr::rename_all(.funs = tolower) %>%
          dplyr::select(id) %>%
          dplyr::mutate(id = as.character(id)) %>%
          sf::st_transform(3005)
      }
    }
  }

  transect_layout <- unique(transect_layout)

   return(transect_layout)


}
