
#' Build site transects
#' Build the associates triangles, buffered triangles and selected paired triangle to sample, based on lowest cost.
#'
#' @param sample_points **sf** spatial object of clhs points
#' @param cost **SpatRast** cost layer generate for sample plan
#' @param mask_poly **sf** spatial object of mask for specific bgc
#' @param centroid_distance Numeric value at which the triangles are placed apart, default is 400 based on albers crs meters
#' @param out_path text string with location in which output sample plan as a geopackage is written
#' @importFrom magrittr "%>%"
#' @importFrom LearnGeom CreateRegularPolygon
#' @import sf
#' @import foreach
#' @return TRUE
#' @export
#' @examples
#' build_site_transects(sample_points, cost, centroid_distance = 400, out_path)

.rot = function(a){
  out <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  return(out)
}

.Tri_build <- function(id, x, y){
  tris <- LearnGeom::CreateRegularPolygon(3, c(as.numeric(paste(x)),
                                    as.numeric(paste(y))), 145) # number of sides, center pt and length of sides

  MoonLineCentre <- data.frame(tris) %>%
    sf::st_as_sf(., coords = c("X", "Y"), crs = 3005) %>%
    dplyr::mutate(id = id) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise() %>%
    sf::st_cast("POLYGON") %>%
    sf::st_cast("MULTILINESTRING") #%>%
  #st_set_crs(newproj)
  return(MoonLineCentre)
}

.rotFeature <- function(Feature, PivotPt, Bearing) {
  # where Feature is the Shape to be rotated, eg:  #Feature <- tri
  # Bearing is the compass bearing to rotate to    #PivotPt <- pt.sf
  # PivotPt is the point to rotate around          #Bearing <- 15

  ## extract the geometry
  Feature_geo <- sf::st_geometry(Feature)
  PivotPoint  <- sf::st_geometry(PivotPt)

  ## Convert bearing from degrees to radians
  d <- ifelse(Bearing > 180, pi * ((Bearing-360)/ 180) ,  pi * (Bearing / 180))

  rFeature <- (Feature_geo - PivotPoint) * .rot(d)   + PivotPoint
  rFeature <- sf::st_set_crs(rFeature, st_crs(Feature))

  Feature$geometry <- st_geometry(rFeature) ## replace the original geometry
  return(Feature)
}



build_site_transects <- function(sample_points, cost, mask_poly, centroid_distance = 400, out_path) {

  # testing lines
  #xx <-sample_points
  #xy <- cost
  #out_path = sampleplan_outdir

  sample_points <- dplyr::select(sample_points, c(slice_num, point_num, bgc)) %>%
    dplyr::arrange(slice_num, point_num ) %>%
    dplyr::mutate(cid = seq(1, nrow(sample_points),1))

  b <- unique(sample_points$bgc)


  # convert geom to geometry
  g <- sample_points
  name<- "geometry"
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  sample_points <- g

  # create paired outputs
  sample_points_clhs <- sf::st_as_sf(sample_points) %>%
    sf::st_transform(3005) %>%
    dplyr::mutate(aoi = NA)

  rotation_angles <- seq(0, 315, 45) # Rotation degrees

  sample_points_rotations <- st_sf(st_sfc()) %>% st_set_crs(3005)

  print( "generating site points")

  for(i in 1:nrow(sample_points_clhs)){
    #i = 1
    pnt <- sample_points_clhs[i,]
    pGeom <- st_geometry(pnt)
    pGeom <- pGeom + c(0, centroid_distance)
    pnt_feat <- st_set_geometry(pnt, pGeom)

    rotated_points <-  st_sf(st_sfc()) %>% st_set_crs(3005)

    rotated_points <- foreach(Bear = rotation_angles, .combine = rbind) %do%{
      #Bear = rotation_angles[5]
      Feature_geo <- st_geometry(pnt_feat)
      PivotPoint  <- st_geometry(pnt)
      ## Convert bearing from degrees to radians
      d <- ifelse(Bear > 180, pi * ((Bear -360)/ 180) ,  pi * (Bear / 180))
      rFeature <- (Feature_geo - PivotPoint) * .rot(d)   + PivotPoint
      rFeature <- st_set_crs(rFeature, st_crs(pnt_feat))
      pnt_feat$geometry <- st_geometry(rFeature) ## replace the original geometry
      pnt_feat$Rotation <- Bear
      pnt_feat <- pnt_feat %>% st_set_crs(3005)
    }

    sample_points_rotations <- rbind(rotated_points, sample_points_rotations)
  }

  sample_points_rotations <- sf::st_as_sf(sample_points_rotations, crs = 3005) %>%
    dplyr::mutate(rotation = plyr::mapvalues(Rotation, rotation_angles, c("N", "NE", "SE", "W", "E", "NW", "SW", "S"))) %>%
    dplyr::filter(!is.na(Rotation)) %>%
    sf::st_join(mask_poly, join = st_intersects) %>%
    dplyr::mutate(aoi = dplyr::case_when(
      is.na(cost) ~ FALSE,
      TRUE ~ TRUE
    ))  %>%
    dplyr::select(-cost) %>%
    cbind(cost = terra::extract(cost, ., ID = FALSE))

  sample_points_low_cost <- sample_points_rotations %>%
    dplyr::group_by(slice_num, point_num) %>%
    dplyr::filter(aoi == TRUE) %>%
    dplyr::slice(which.min(cost)) %>%
    dplyr::ungroup()


  # if (length(sample_points_low_cost$id) != length(unique(sample_points_rotations$id))) {
  #
  #   print("selecting paired transect manually")
  #
  #  no_aoi_tris <- setdiff(unique(sample_points_rotations$id), unique(sample_points_low_cost$id))
  #
  #  sample_points_low_cost_no_aoi <- sample_points_rotations %>%
  #    filter(id %in% no_aoi_tris) %>%
  #    group_by(id) %>%
  #    slice(which.min(cost)) %>%
  #    ungroup()
  #
  #  sample_points_low_cost <- sample_points_low_cost %>%
  #    rbind(sample_points_low_cost_no_aoi)
  #
  #  }

  sample_points_low_cost <- sample_points_low_cost %>%
    dplyr::select(-c(cost, Rotation, aoi))

  sample_points_clhs <- sample_points_clhs %>%
    dplyr::mutate(rotation = "cLHS") %>%
    dplyr::select(-aoi)

  sample_points_rotations <- sample_points_rotations %>%
    dplyr::select(-cost, -Rotation, -aoi) %>%
    dplyr::filter(!is.na(rotation))

  all_points <- rbind(sample_points_clhs, sample_points_rotations)  %>%
    dplyr::mutate(id = paste(paste(bgc, paste(slice_num, point_num,sep = "."), cid, sep = "_"),rotation, sep = "_"))

  paired_sample <- rbind(sample_points_clhs, sample_points_low_cost)  %>%
    dplyr::mutate(id = paste(paste(bgc, paste(slice_num, point_num,sep = "."), cid, sep = "_"),rotation, sep = "_"))

  # Create triangle around each point and randomly rotate
  print( "generating site transects")

  all_triangles <- st_sf(st_sfc()) %>% st_set_crs(3005)

  for(i in 1:nrow(all_points)){
    #i = 1
    poc <- all_points[i, ]

    triangle <- .Tri_build(id = poc$id, x =  st_coordinates(poc)[1], y =  st_coordinates(poc)[2])
    random_rotation <- runif(1, min = 0, max = 360)
    triangle <- .rotFeature(triangle, poc, random_rotation)

    #  if(is.na(poc$rotation)){
    #    triangle$id <- poc$id
    #  } else {
    #    triangle$id <- paste(poc$id, poc$rotation, sep = "_")
    #  }
    all_triangles <- rbind(all_triangles, triangle)
  }

  # check that all triangles fall within mask poly

  paired_triangles <- all_triangles[all_triangles$id %in% paired_sample$id,]


  print( "writting out spatial data")

  outname = "s1_sampling.gpkg"

  #####write Transects####################

  st_write(all_points, file.path(out_path,outname),
           layer = paste0(b,"_points_all"), delete_layer = TRUE)

  st_write(all_triangles, file.path(out_path, outname),
           layer = paste0(b,"_transects_all"), delete_layer = TRUE)

  st_write(paired_sample, file.path(out_path, outname),
           layer = paste0(b,"_points"),  delete_layer = TRUE)


  ####write buffer#########################
  triangle_buff <- st_buffer(all_triangles, dist = 10)

  st_write(triangle_buff, file.path(out_path, outname), layer = paste0(b,"_transects_all_buffered"), delete_layer = TRUE)

  st_write(paired_sample, file.path(out_path, outname), layer = paste0(b,"_points"),  delete_layer = TRUE)

  #paired_triangles
  st_write(paired_triangles, file.path(out_path, outname), layer = paste0(b,"_transects"),  delete_layer = TRUE)

  ####write buffer#########################
  ptriangle_buff <- st_buffer(paired_triangles, dist = 10)
  st_write(ptriangle_buff, file.path(out_path, outname), layer = paste0(b,"_transects_buffered"), delete_layer = TRUE)

  # write out clhs points only
  st_write(sample_points_clhs, file.path(out_path, outname), layer = paste0(b,"_points_clhs"), delete_layer = TRUE)


}

#
# .pairedPoint <- function(POC, Dist, Rotations){ #Where bearing is the bearing recorded in the transect
#   # This function is dependent on other PEM functions: rot, rotFeature
#   PROJ <- st_crs(POC)
#   pGeom <- st_geometry(POC)
#   pGeom <- pGeom + c(0,Dist)
#   feat <- st_set_geometry(POC, pGeom)
#   pts <- foreach(Bear = Rotations, .combine = rbind) %do%{
#     Bear = Rotations
#     temp <- rotFeature(feat, POC, Bear)
#     temp <- st_set_crs(temp, PROJ)
#     temp$Rotation <- Bear
#     temp
#   }
#   return(pts)
# }


