
#' Select lowest cost clhs option
#'
#' @param clhs_outpath filepath in which the clhs multiple outputs are saved
#'
#' @return plot to show the cost outputs per generated sample plan repeat
#' @export
#' @importFrom foreach foreach
#' @importFrom sf st_layers st_read st_drop_geometry
#' @importFrom dplyr mutate select
#' @import ggplot2
#'
#' @examples
#' select_lowcost_clhs(fid$samplingplan_clhs[2])

select_lowcost_clhs = function(clhs_outpath ){

  ftemp <- list.files(clhs_outpath, pattern = ".gpkg$", full.names = TRUE)

  all_samples <- foreach(fs = 1:length(ftemp), .combine = "rbind" ) %do% {

    ff <- ftemp[fs]
    layers <- st_layers(ff)$name

    sample_points_all <- foreach(l = 1:length(layers), .combine = rbind) %do% {
      ll = layers[l]
      sptemp <- st_read(ff, layer = ll)
      sptemp <- sptemp %>%
        dplyr::mutate(clhs_repeat = basename(ff)) %>%
        dplyr::mutate(bgc = stringr::str_extract(layers, "[^_]+"))
      sptemp
    }

  }

  # summarise the costs per sample plan
  repsum <- all_samples %>%
    sf::st_drop_geometry() %>%
    dplyr::select(clhs_repeat, bgc, cost)

  repsum <- repsum %>%
    dplyr::group_by(clhs_repeat, bgc) %>%
    dplyr::mutate(tcost = sum(cost)) %>%
    dplyr::select(-cost) %>%
    dplyr::distinct()

  # plot the total costs by subzone
  p1 <- ggplot2::ggplot(repsum, aes(y = tcost, x =  clhs_repeat )) +
    ggplot2::geom_point()+
    ggplot2::facet_wrap(~bgc, scales = "free_x")+
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  p1

  return(p1)
}
