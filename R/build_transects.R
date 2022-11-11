#
# library(foreach)
# library(LearnGeom)
#
# rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
#
# ## Feature rotation
# rotFeature <- function(Feature, PivotPt, Bearing) {
#   # where Feature is the Shape to be rotated, eg:  #Feature <- tri
#   # Bearing is the compass bearing to rotate to    #PivotPt <- pt.sf
#   # PivotPt is the point to rotate around          #Bearing <- 15
#
#   ## extract the geometry
#   Feature_geo <- st_geometry(Feature)
#   PivotPoint  <- st_geometry(PivotPt)
#
#   ## Convert bearing from degrees to radians
#   d <- ifelse(Bearing > 180, pi * ((Bearing-360)/ 180) ,  pi * (Bearing / 180))
#
#   rFeature <- (Feature_geo - PivotPoint) * rot(d)   + PivotPoint
#   rFeature <- st_set_crs(rFeature, st_crs(Feature))
#
#   Feature$geometry <- st_geometry(rFeature) ## replace the original geometry
#   return(Feature)
# }
#
#
#
# pairedPoint <- function(POC, Dist, Rotations){ #Where bearing is the bearing recorded in the transect
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
#
#
#
# ## Create Triangles -------------------------------------------------
# ## Build Triangle from a ID, x and y coordinate
#
# Tri_build <- function(id, x, y){
#   tris <- CreateRegularPolygon(3, c(as.numeric(paste(x)),
#                                     as.numeric(paste(y))), 145) # number of sides, center pt and length of sides
#
#   MoonLineCentre <- data.frame(tris) %>%
#     st_as_sf(., coords = c("X", "Y"), crs = 3005) %>%
#     mutate(id = id) %>%
#     group_by(id) %>%
#     dplyr::summarise() %>%
#     st_cast("POLYGON") %>%
#     st_cast("MULTILINESTRING") #%>%
#   #st_set_crs(newproj)
#   return(MoonLineCentre)
# }
#
#
#
#
# ## Create a series of triangles -------------------------------------
# ## calls the rotation functions above
# ## Generate Potential lines /'MoonTransect' around a given POC
# tri_set <- function(POC, Shape){  ## Where C is a point feature
#   ## Where is Shape is the feature to be rotated
#
#   allLines <- Shape ## this will be appended to later
#
#   allLines$Rot <- 0  ## no rotation for the first transect
#
#   ## Loop through a sequence of bearings
#   for(d in seq(15,345, by = 15)){
#
#     Shape$Rot <- d  ## Adds an attribute field to indicate the bearing it was rotated to
#     tmp <- rotFeature(Shape, POC, d)  ## calls rotFeature function above
#
#     allLines <- rbind(allLines, tmp)
#   }
#   return(allLines)
# }
