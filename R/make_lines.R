#' make_lines()
#'
#' Converts GPS data/ waypoints to line features.  Attribute data is maintained.
#' Currently, this function generates points to transect by converting the _POINT_
#' feature data to LINES sf features.
#' A placeholder has been made to generate transect lines via the transect method created by M. Coghill.
#'
#' @param GPSPoints is a _simple features_ POINT object.
#'        Minimum attributes in this file include fields named _'id'_ and _'time'_.
#'        Waypoints will be sorted by these two entries.
#' @param Transects is the original planned transect simple feature.
#'        The _'id'_ attribute in this feature will be used to group points by their transect id.
#'        This transect id will be named 'TID' in the output data.
#' @param method Multiple methods to be made available:
#' - _pts2lines_ takes a sf POINT object and converts it to lines.
#' _This is currently the only function available
#' - _tracklog_ uses the tracklog and the sample points to generate the lines. _Not implemented yet_.
#' @param sortby Field in the points data to sort the points by.  Defaults to  _"none"_ (i.e. assumes no sorting of data)
#' @param tBuffer is optional with a default of 20m.  This is the buffer distance to place around the transect.  Waypoints outside this distance will not be considered.
#' @param PROJ is an optional the PROJ projection code with a default of BC Albers (3005).  Data imported will be transfored to this projection and final data will be exported in this projection.
#' @keywords points, lines, convertion
#' @export
#' @importFrom magrittr "%>%"
#' @import sf
#' @examples
#' ## Convert GPS waypoints to line features (i.e. transect data)
#' transects <- convert_pts2lines(gpsData, PlannedTransects)

make_lines <- function(GPSPoints = NA,
                       GPSTracks = NA,
                       Transects, method = "pts2lines",
                       sortby = "none", tBuffer = 20, PROJ = 3005) {


  # # # testing
  #    GPSPoints = points
  # # # # GPSTracks = all_tracks
  #     Transects = transect_layout
  #     method = "pts2lines"
  #     sortby = "order"
  #    tBuffer = 20
  #     PROJ = 3005

  if (method == "pts2lines") {
    ## Transects
    planT <- Transects %>%
      dplyr::mutate(TID = dplyr::row_number()) %>%
      sf::st_buffer(.,tBuffer) %>% dplyr::select(TID)

    ## Transform data to PROJ
    planT <- planT %>% sf::st_transform(PROJ)
    GPSPoints <- GPSPoints %>% sf::st_transform(PROJ)

    ## Spatial join attributes
    GPSPoints <- sf::st_join(GPSPoints, planT)

    ## f(n) start
    ## ADD TRANSECT ID -- and the arrange TID, time
    # # Sort Data ---------
    #  if (sortby != "none") {
    #    # sortby <- "name"
    #    #sortField <- eval(parse(text = paste0("GPSPoints$", sortby)))
    #    GPSPoints <- GPSPoints %>%
    #      group_by(TID) %>%
    #      arrange(sortby) %>%
    #      ungroup()
    #   # GPSPoints[order(sortField),]
    #  }

    GPSPoints <- GPSPoints %>%
      tibble::rowid_to_column("ID") # ID is needed table manipulation below

    ## convert GPSPoints to a table for manipulation
    GPSPoints <- cbind(GPSPoints, sf::st_coordinates(GPSPoints))
    GPSPoints <- GPSPoints %>% st_zm %>% sf::st_drop_geometry()


    # loop through each transect
    transects_id <- unique(GPSPoints$TID)
    all_lines <- foreach(x = transects_id, .combine = rbind) %do% {

      #x <- transects_id[1] # testing line
      GPSPoints_transect <- GPSPoints %>%
        dplyr:::filter(TID == x)

      ## Define the Line Start and End Coordinates
      ## Add XY coordinates as

      lines <- GPSPoints_transect %>%
        dplyr::mutate(Xend = dplyr::lead(X),
                      Yend = dplyr::lead(Y)) %>%  # collect the coordinates of the next point
        dplyr::filter(!is.na(Yend)) #%>% # drops the last row (start point with no end)


      ## Use data.table with sf to create the line geometry
      dt <- data.table::as.data.table(lines)
      sf <- dt[,
               {
                 geometry <- sf::st_linestring(x = matrix(c(X, Xend, Y, Yend), ncol = 2))
                 geometry <- sf::st_sfc(geometry)
                 geometry <- sf::st_sf(geometry = geometry)
               }
               , by = ID
      ]

      ## Replace the geometry
      lines$geometry <- sf$geometry

      ## Declare as a simple feature
      lines <- sf::st_as_sf(lines, sf_column_name = "geometry") %>%
        sf::st_set_crs(PROJ)

      lines

    }


    ## Need to remove excess lines -- currently there are lines that run between the plots
    all_lines$within <- as.logical(rowSums(unlist(st_within(all_lines, planT, sparse = FALSE)) == TRUE))
    all_lines <- all_lines[all_lines$within == TRUE, ]  ## removes lines not contained in Transect area

    ## There are a few invalid geometries
    all_lines$valid <- as.logical(sf::st_is_valid(all_lines))

    ## lwgeom package will validate all invalid geometries
    all_lines <- sf::st_make_valid(all_lines)
    # lines <- lines[lines$valid == TRUE,]  ## removes lines not contained in Transect area

    all_lines <- all_lines %>% dplyr::select(-c(Xend,Yend,within, valid))
    # lines <- lines %>% dplyr::select(-c(Xend,Yend, valid))
    #lines <- lines %>% dplyr::select(TID, id, name, time, SiteSeries:Confidence)
    all_lines <- all_lines %>% dplyr::select(-c(X,Y,TID, ID))

    return(all_lines)

  } else {
    ## Begin transect method ----------------------------------------------------
    ## TO DO: use the foreach package and utilize %dopar% where possible
    if (method == "tracklog") {

      # Get tracklog line files, should be in the same place as point files

      # Process tracks for each tracklog
      planT <- Transects %>%
        st_transform(PROJ) %>%
        dplyr::mutate(TID = dplyr::row_number()) %>%
        # rename(TID = id)  %>% ## rename the id to not conflict with the GPS id field
        sf::st_buffer(.,tBuffer)

      message("Processing Tracklogs")

      cl <- parallel::makeCluster(parallel::detectCores())
      doParallel::registerDoParallel(cl)
      tracks_processed <- foreach(t = unique(GPSTracks$id), .combine = rbind,
                                  .packages = c("sf", "dplyr", "foreach")) %dopar% {

                                    track_raw <- GPSTracks[GPSTracks$id == t, ] %>%
                                      st_transform(PROJ) %>%
                                      st_zm()

                                    # Filter out singular line geometries (i.e.: points from line data)
                                    for(i in 1:nrow(track_raw)) {
                                      if(length(track_raw$geometry[[i]]) > 2){
                                        track_raw[i, "filter"] <- TRUE
                                      } else {
                                        track_raw[i, "filter"] <- FALSE
                                      }
                                    }

                                    track_raw <- dplyr::filter(track_raw, filter == TRUE) %>%
                                      st_intersection(planT) %>%
                                      group_by(name) %>%
                                      slice(1) %>%
                                      ungroup()

                                    track_append <- foreach(i = unique(track_raw$ID), .combine = rbind) %do% {
                                      track_filter <- track_raw[track_raw$ID == i, ]
                                      for(j in 1:nrow(track_filter)) {
                                        if(st_geometry_type(track_filter[j, ]) != "LINESTRING") {
                                          new_geom <- st_coordinates(track_filter[j, ])[, c("X", "Y")] %>%
                                            st_linestring() %>%
                                            st_sfc(crs = PROJ)
                                          track_filter[j, ] <- st_set_geometry(track_filter[j, ], new_geom) %>%
                                            st_cast("LINESTRING")

                                        }
                                      }
                                      track_filter <- st_cast(track_filter, "LINESTRING")
                                      new_geom <- st_coordinates(track_filter)[, c("X", "Y")] %>%
                                        st_linestring() %>%
                                        st_sfc(crs = PROJ)

                                      st_geometry(track_filter[1, ]) <- new_geom
                                      return(track_filter[1, ])
                                    }

                                    # Need to check if the tracklog became a multilinestring after intersection
                                    # or not. Performing this quick fix on full tracklog seems to work better
                                    # than using the multiline_to_line function (more used for the smaller
                                    # map unit calls)

                                    track_append <- st_cast(track_append, "LINESTRING")
                                  } %>% dplyr::filter(st_length(.) > units::set_units(650, m))

      parallel::stopCluster(cl)

      if(!all(is.na(tracks_processed$ID))) {
        # The following applies a fix for tracks with multiple tracklog files,
        # eg: POC to TP1 was a line, TP1 to TP2 was a separate line, etc.
        # This part will merge any of those inconsistencies into a single LINESTRING
        # for each triangle as well as identify the start and end points of each transect

        track_make <- function(x, type) {
          output <- foreach(i = unique(x$ID)[!is.na(unique(x$ID))], .combine = rbind,
                            .packages = c("sf", "foreach"), .export = "PROJ") %dopar% {
                              track_filter <- dplyr::filter(x, ID == i)

                              if(nrow(track_filter) > 1) {
                                track_matrix <- foreach(j = 1:nrow(track_filter), .combine = rbind) %do% {
                                  st_geometry(track_filter[j, "geometry"])[[1]]
                                }

                                track_merged <- st_sfc(st_linestring(track_matrix), crs = PROJ)
                                track_filter <- track_filter[1, ] %>%
                                  st_set_geometry(track_merged)
                              }

                              track_matrix_open <- st_geometry(track_filter[1, "geometry"])[[1]]
                              if(type == "open") {
                                track_merged_open <- st_sfc(st_linestring(track_matrix_open), crs = PROJ)
                                track_filter_open <- st_set_geometry(track_filter, track_merged_open)
                              } else if(type == "closed") {
                                track_matrix_closed <- rbind(track_matrix_open, track_matrix_open[1, ])
                                track_merged_closed <- st_sfc(st_linestring(track_matrix_closed), crs = PROJ)
                                track_filter_closed <- st_set_geometry(track_filter, track_merged_closed)
                              } else if(type == "first") {
                                track_matrix_first <- matrix(track_matrix_open[1:(nrow(track_matrix_open) / 2), ], ncol = 2)
                                track_merged_first <- st_sfc(st_linestring(track_matrix_first), crs = PROJ)
                                track_filter_first <- st_set_geometry(track_filter, track_merged_first)
                              } else if(type == "last") {
                                track_matrix_last <- matrix(track_matrix_open[(nrow(track_matrix_open) / 2) : nrow(track_matrix_open), ], ncol = 2)
                                track_merged_last <- st_sfc(st_linestring(track_matrix_last), crs = PROJ)
                                track_filter_last <- st_set_geometry(track_filter, track_merged_last)
                              }
                            }
          return(output)
        }

        cl <- parallel::makeCluster(parallel::detectCores())
        doParallel::registerDoParallel(cl)
        tracks_open <- track_make(tracks_processed, "open")
        tracks_closed <- track_make(tracks_processed, "closed")
        tracks_first <- track_make(tracks_processed, "first")
        tracks_last <- track_make(tracks_processed, "last")

        track_start_point <- st_line_sample(tracks_open, sample = 0) %>%
          st_sf() %>%
          st_join(planT, join = st_is_within_distance, dist = 30, largest = TRUE)
        track_end_point <- st_line_sample(tracks_open, sample = 1) %>%
          st_sf() %>%
          st_join(planT, join = st_is_within_distance, dist = 30, largest = TRUE)

        track_end_start_lines <- foreach(i = 1:nrow(track_start_point), .combine = rbind,
                                         .packages = "sf") %dopar% {
                                           j <- match(track_start_point$ID[i], track_end_point$ID)

                                           if(!is.na(j)) {
                                             track_end_start_line <- rbind(track_end_point[j, ],
                                                                           track_start_point[i, ]) %>%
                                               st_coordinates()
                                             track_end_start_line <- track_end_start_line[, 1:2] %>%
                                               st_linestring() %>%
                                               st_sfc(crs = PROJ) %>%
                                               st_sf() %>%
                                               st_join(planT, largest = TRUE) %>%
                                               st_cast("LINESTRING")
                                           }
                                         }
        parallel::stopCluster(cl)
      }

      ###########################################################################

      points_raw <- GPSPoints %>%
        dplyr::filter(id %in% tracks_processed$ID)

      remaining_pts <- GPSPoints %>%
        dplyr::filter(!id %in% tracks_processed$ID)

      message("Segmenting tracklog files")

      # If the start or end points are not within a given distance of the tracklog start point,
      # draw lines between the points until it reaches the tracklog start point
      # Same thing with the end point



      # Perform a check to ensure that there is a POC and POT for each transect.
      # Currently, the headings for each study area are inconsistent, may implement a
      # matching pattern check in the future to automate that process, or maybe
      # that could be something identified at the top of the function.

      cl <- parallel::makeCluster(parallel::detectCores())
      doParallel::registerDoParallel(cl)
      points_processed <- foreach(i = 1:length(unique(points_raw$id)), .combine = rbind,
                                  .export = c("PROJ", "extend_lines"), .packages = c("sf", "tidyverse")) %dopar% {

                                    j = unique(points_raw$id)[i]
                                    points_filter <- dplyr::filter(points_raw, id == j)
                                    poc_check <- any(as.character(points_filter[, "point_type"][[1]]) %in% "POC")
                                    pot_check <- any(as.character(points_filter[, "point_type"][[1]]) %in% "POT")

                                    if(poc_check == FALSE){
                                      points_poc <- points_filter[1, ]
                                      points_filter <- points_filter[-1, ]
                                      points_poc$point_type <- "POC"
                                    } else {
                                      points_poc <- points_filter[points_filter[, "point_type"][[1]] == "POC", ] %>%
                                        drop_na(name)
                                    }

                                    if(pot_check == FALSE){
                                      points_pot <- points_filter[nrow(points_filter), ]
                                      points_filter <- points_filter[-nrow(points_filter), ]
                                      points_pot$point_type <- "POT"
                                    } else {
                                      points_pot <- points_filter[points_filter[, "point_type"][[1]] == "POT", ] %>%
                                        drop_na(name)
                                    }

                                    if(!is.na(match(points_poc$id, track_start_point[track_start_point$ID == j, ]$ID))) {
                                      line_draw_start <- st_is_within_distance(points_poc, track_start_point[track_start_point$ID == j, ], dist = tBuffer, sparse = FALSE)
                                      line_draw_prox <- st_is_within_distance(points_poc, tracks_first[tracks_first$ID == j, ], dist = tBuffer, sparse = FALSE)

                                      if(isFALSE(line_draw_start) && isFALSE(line_draw_prox)) {
                                        line_draw_check <- st_is_within_distance(points_filter, track_start_point[track_start_point$ID == j, ], dist = tBuffer, sparse = FALSE)

                                        line_draw_omit <- tail(1:nrow(line_draw_check), n = round(1 / 3 * nrow(line_draw_check)))
                                        new_line_draw_start <- as.matrix(line_draw_check[-line_draw_omit, ])

                                        if(any(new_line_draw_start)) {
                                          first_t <- 1:(which(line_draw_check[, 1] %in% TRUE)[1] - 1)
                                        } else {
                                          first_t <- 1
                                        }

                                        new_line <- c(st_geometry(points_filter)[first_t], st_geometry(track_start_point[track_start_point$ID == j, ])) %>%
                                          st_combine() %>%
                                          st_cast("LINESTRING")
                                        new_geom <- rbind(new_line[[1]], st_geometry(tracks_open[tracks_open$ID == j, ])[[1]]) %>%
                                          st_linestring() %>%
                                          st_sfc(crs = PROJ)
                                        st_geometry(tracks_open[tracks_open$ID == j, ]) <- new_geom
                                        st_geometry(track_start_point[track_start_point$ID == j, ]) <- st_line_sample(new_geom, sample = 0)
                                        points_filter[first_t, ] <- st_jitter(points_filter[first_t, ], amount = 0.01)
                                        points_poc <- st_jitter(points_poc, amount = 0.01)
                                      } else {
                                        st_geometry(points_poc) <- st_geometry(
                                          track_start_point[track_start_point$ID == j, ][match(points_poc$id, track_start_point[track_start_point$ID == j, ]$ID), ]) %>%
                                          st_jitter(amount = 0.01) # Jitters POC point slightly away from the track start point so that join lines can be created. If point is exactly where it should be, no line will be drawn so this was a necessary step.
                                      }
                                    }

                                    if(!is.na(match(points_pot$id, track_end_point[track_end_point$ID == j, ]$ID))) {
                                      line_draw_end <- st_is_within_distance(points_pot, track_end_point[track_end_point$ID == j, ], dist = tBuffer, sparse = FALSE)
                                      line_draw_prox <- st_is_within_distance(points_pot, tracks_last[tracks_last$ID == j, ], dist = tBuffer, sparse = FALSE)

                                      if(isFALSE(line_draw_end) && isFALSE(line_draw_prox)) {
                                        line_draw_check <- st_is_within_distance(points_filter, track_end_point[track_end_point$ID == j, ], dist = tBuffer, sparse = FALSE)

                                        line_draw_omit <- head(1:nrow(line_draw_check), n = round(1 / 3 * nrow(line_draw_check)))
                                        new_line_draw_end <- as.matrix(line_draw_check[-line_draw_omit, ])

                                        if(any(new_line_draw_end)) {
                                          last_t <- sort(nrow(line_draw_check):(tail(which(line_draw_end[, 1] %in% TRUE), n = 1) + 1))
                                        } else {
                                          last_t <- nrow(line_draw_check)
                                        }

                                        new_line <- c(st_geometry(track_end_point[track_end_point$ID == j, ]), st_geometry(points_filter)[last_t]) %>%
                                          st_combine() %>%
                                          st_cast("LINESTRING")
                                        new_geom <- rbind(st_geometry(tracks_open)[[1]], new_line[[1]]) %>%
                                          st_linestring() %>%
                                          st_sfc(PROJ)
                                        st_geometry(tracks_open[tracks_open$ID == j, ]) <- new_geom
                                        st_geometry(track_end_point[track_end_point$ID == j, ]) <- st_line_sample(new_geom, sample = 1)
                                        points_filter[last_t, ] <- st_jitter(points_filter[last_t, ], amount = 0.01)
                                        points_pot <- st_jitter(points_pot, amount = 0.01)
                                      }

                                      st_geometry(points_pot) <- st_nearest_points(points_pot, tracks_last[tracks_last$ID == j, ]) %>%
                                        extend_lines() %>%
                                        st_line_sample(sample = 1)
                                    }

                                    points_mid <- subset(points_filter, !(points_filter[, "point_type"][[1]] %in% c("POC", "POT")))
                                    points_full <- rbind(points_poc, points_mid, points_pot)
                                  }

      # Code for cutting tracklog into segments
      # This is done by creating lines from the raw point data to the tracklog,
      # and extending that line in order to cross the tracklog as an intersecting point
      #####################################


      snap_lines <- foreach(i = unique(points_processed$id), .combine = rbind, .packages = "sf", .export = "extend_lines") %dopar% {
        snap <- st_nearest_points(
          points_processed[points_processed$id == i, ],
          tracks_open[tracks_open$ID == i, ]
        ) %>%
          extend_lines() %>%
          st_sf()
      }

      snap_lines_intersections <- st_intersects(snap_lines, tracks_open)

      # Dealing with sharp angles/Cutting lines that didn't end result in intersections:
      # Some of the extended lines from above didn't end up cutting across the tracklog,
      # likely because the point snapped to a tracklog vertex, and the tracklog was
      # turning at a sharp angle at that vertex. When a line reaches that vertex, it won't
      # end up crossing the tracklog at that point (imagine drawing a line between these two
      # symbols:   .^   and the point is snapping to the tip of the exponent symbol), so for
      # those points a new cutting line is created at the end of the old cutting line, which
      # should be fairly perpendicular to the tracklog and allow for proper cutting to occur.
      for(i in 1:length(snap_lines_intersections)) {
        if(length(snap_lines_intersections[[i]]) == 0) {
          st_geometry(points_processed[i, ]) <- st_cast(st_line_sample(snap_lines[i, ], sample = 1), "POINT")
        }
      }

      new_snap_lines <- foreach(i = unique(points_processed$id), .combine = rbind, .packages = "sf", .export = "extend_lines") %dopar% {
        new_snap <- st_nearest_points(
          points_processed[points_processed$id == i, ],
          tracks_open[tracks_open$ID == i, ]
        ) %>%
          extend_lines() %>%
          st_sf() %>%
          st_join(points_processed, largest = TRUE) # Creates attributed cutting lines from the point information
      }

      # Next the tracklog is split using the cutting lines made above
      track_split <- st_collection_extract(
        st_split(tracks_open, st_combine(new_snap_lines)), "LINESTRING"
      )

      # Next, the segmented tracklog needs to be attributed. To do this properly,
      # points need to be placed on the tracklog such that a point only touches a single
      # line segment. These points then have a line drawn from them to the closest
      # processed points, the line is extended to cross the tracklog once again, the
      # extended line is attributed, and finally the tracklog segment is attributed from
      # the extended line.
      track_moved_points <- st_line_sample(track_split, sample = 0.01)
      track_join_index <- st_nearest_feature(points_processed, track_moved_points)
      track_join_lines <- st_nearest_points(
        points_processed,
        track_moved_points[c(track_join_index)],
        pairwise = TRUE
      ) %>%
        extend_lines() %>%
        st_sf() %>%
        st_join(points_processed, suffix = c(".x", ""), largest = TRUE)

      track_attributed <- st_join(
        track_split,
        track_join_lines,
        join = st_intersects,
        suffix = c(".x", ""),
        largest = TRUE
      ) %>%
        dplyr::select(colnames(track_join_lines), TID)

      # Depending on the tracklog, some track segments are attributed with NA, likely
      # due to self crossings. The following code detects any of those patterns and
      # merges them with the track log attributes found above it in the data table. If
      # the first row is NA, or not POC, then the POC is found and placed at the top of
      # the table, then processing proceeds. NA's are then summarized (dissolved) with
      # the attributes above it in the data table. Finally, if any piece of the track
      # is found outside of the transect template 10m buffer zone, it's erased.

      track_final <- foreach(i = unique(track_attributed$TID), .combine = rbind, .packages = "sf") %dopar% {
        track_unique <- dplyr::filter(track_attributed, TID == i)

        if(!all(is.na(unique(track_unique$name)))) {
          if(track_unique[1, "point_type"][[1]] != "POC" ||
             is.na(track_unique[1, "point_type"][[1]])) {
            poc_line <- track_unique %>%
              dplyr::filter(point_type == "POC")

            track_unique <- track_unique %>%
              dplyr::filter(point_type != "POC" |
                              is.na(point_type))

            track_unique <- rbind(poc_line, track_unique)
          }

          for(j in 2:nrow(track_unique)) {
            if(is.na(track_unique$name[j])) {
              track_unique$name[j] <- track_unique$name[j - 1L]
            }
          }

          track_unique <- group_by(track_unique, name) %>%
            summarise_at(names(track_unique)[!names(track_unique) %in% c("name", "geometry")], first) %>%
            st_cast()
        }
      }

      ## Process points without tracklog association
      if(nrow(remaining_pts) > 0) {
        message("Appending points without tracklog data")
        points_append <- foreach(i = unique(remaining_pts$id), .combine = rbind,
                                 .packages = c("sf", "tidyverse")) %dopar% {
                                   points_filter <- dplyr::filter(remaining_pts, id == i) %>%
                                     mutate(sort = as.numeric(gsub("[[:alpha:]]", "", name))) %>%
                                     arrange(sort) %>%
                                     dplyr::select(-sort)
                                   for(j in 1:(nrow(points_filter) - 1)) {
                                     new_geom <- st_linestring(rbind(st_geometry(points_filter[j, ])[[1]],
                                                                     st_geometry(points_filter[j + 1, ])[[1]])) %>%
                                       st_sfc(crs = PROJ)
                                     st_geometry(points_filter[j, ]) <- new_geom
                                   }
                                   points_filter <- points_filter[-nrow(points_filter), ]
                                 }
        points_append <- st_join(points_append, planT, join = st_nearest_feature) %>%
          dplyr::select(names(track_final))
      }
      parallel::stopCluster(cl)

      track_final <- rbind(track_final, points_append)
      message("Converting MULTILINESTRING's to LINESTRING's")
      lines <- multiline_to_line(track_final) %>%
        tibble::rowid_to_column("ID")  %>% # ID is needed for further manipulation later on
        distinct(., .keep_all = TRUE)
    }
    return(lines)
  }
}

