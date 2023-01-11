#' Format field data
#'
#' Standardizes format from raw field data to standard point data
#'
#' @param transect_layout_buf sf spatial object with transect buffered data
#' @param datafolder text string with location of raw files in shp or gpk format
#'
#' @author Genevieve Perkins
#' @import foreach
#' @return sf point data
#' @export
#'
#' @examples
#'
#' clean_pts <- format_fielddata(inputfolder, transect_layout)


format_fielddata <- function(datafolder, transect_layout_buf){

 #datafolder <- rawdat

  points <- list.files(file.path(datafolder), pattern = ".gpkg$|.shp$", full.names = TRUE, recursive = TRUE)
 # points <-  points[1:90]

  all_points <- foreach(x = points, .combine = rbind) %do% {

  # x = points[88]

    s1_layers <- sf::st_layers(x)
    pts <- which(s1_layers[["geomtype"]] %in% c("Point","3D Point","3D Measured Point"))

    if(length(pts)>0) {

     # print(x)

      points_read <- sf::st_read(x, quiet = TRUE) %>%
        sf::st_transform(3005) %>%
        sf::st_zm() %>%
        dplyr::rename_all(.funs = tolower)

      start_length = length(points_read$name)

      # 1) transect name

      if("f01_transec" %in% names(points_read)){
        dnames = names(points_read)
        colnames(points_read) <- gsub("f0", "x0", dnames)
        points_read <- points_read %>%
          dplyr::mutate(x10_edatope = f10_edatope)
      }


      if("x01_trans" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transect_id = x01_trans)
      }

      if("x01_transec" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transect_id = x01_transec)
      }
      if("x01_transect_id" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transect_id = x01_transect_id)
      }

      # 2) observer
      if("x02_observe" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(observer = x02_observe)
      }

      if("x02_observer" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(observer = x02_observer)
      }

      # 3) point_type
      if("x03_pt_type" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(point_type = x03_pt_type)
      }

      # 4) Mapunit1
      if("x04_mapunit" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit1 = x04_mapunit)
      }

      # 5) Mapunit2
      if("x06_mapunit" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit2 = x06_mapunit)
      }

      if("x06_mapunit2" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit2 = x06_mapunit2)
      }


      # 6) transtition
      if("x05_transit" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transition = x05_transit)
      }
      if("x05_transition" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transition = x05_transition)
      }


      # 7) Stand struc
      if("x07_struc_" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(stuc_stage = x07_struc_)
      }

      if("x07_struct_" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(stuc_stage = x07_struct_)
      }

      if("x07_struct_stage" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(stuc_stage = x07_struct_stage)
      }


      # 8) Stand struc
      if("x08_struct" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(stuc_mod = x08_struct)
      }

      # 8) Stand struc
      if("x08_struct_" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(stuc_mod = x08_struct_)
      }

      if("x08_struct_stage_mod" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(stuc_mod = x08_struct_stage_mod)
      }


      # 9) Edatope
      if("x10_edatope" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(edatope = x10_edatope)
      }


      # 10) comments

      if("x09_comment" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(comments = x09_comment)
      }


      if("x09_comments" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(comments = x09_comments)
      }


      # 11 timestamp # this still needs lots of work to check the

      if("timestamp" %in% names(points_read)){

        points_read <- points_read %>%
          dplyr::mutate(date_ymd = lubridate::as_date(points_read$timestamp))

        if(stringr::str_length(points_read$timestamp[1])>10){

          points_read <- points_read %>%
            dplyr::mutate(date_time = lubridate::as_datetime(points_read$timestamp))

          #  points_read <- points_read %>%
          #    dplyr::mutate(time_hms =  hms::as_hms(timestamp))

          points_read <- points_read %>%
            dplyr::mutate(time_hms = format(date_time, format = "%H:%M:%S"))

        } else {

          points_read <- dplyr::mutate(points_read, time_hms = NA)

        }

      } # end of time section


      if("name" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::mutate(order = as.numeric(gsub("Placemark ", "", name)))
      }

       #print(length(points_read$order))
      #names(points_read)

      # Transect id

      points_read <- points_read %>%
        sf::st_join(., transect_layout_buf, join = st_intersects) %>%
        dplyr::rename_all(.funs = tolower) %>%
        dplyr::distinct()

      points_read <- points_read %>%
        dplyr::mutate(id = gsub("\\s", "", id)) %>%
        dplyr::mutate(transect_id = id)

      #print(length(points_read$order))

      points_read <- points_read %>%
        dplyr::mutate(data_type = ifelse(is.na(transect_id), "incidental", "s1"))


      if(all(is.na(points_read$observer))){

        stop(print("observer name missing in original data, check and re-run, check:"))
        print( x )

        } else {

      # print ("filling observer names")

      points_read <- .fill_observer(points_read)

      }

      # check the mapunit 1 is filled if mapunit 2 is not na

      points_read <- points_read %>%
        dplyr::mutate(mapunit1 = dplyr::case_when(
          is.na(mapunit1) & !is.na(mapunit2) ~ mapunit2,
          is.null(mapunit1) & !is.na(mapunit2) ~ mapunit2,
          mapunit1 == " " & !is.na(mapunit2) ~ mapunit2,
          TRUE ~ as.character(mapunit1)
        ) )


      # check is mapunit 1 and 2 are identical
      # still in testing

      # cc <- points_read %>%
      #   dplyr::select(mapunit1, mapunit2)%>%
      #   dplyr::filter(mapunit2 != " ")%>%
      #   st_drop_geometry() %>%
      #   dplyr::distinct()


      points_read <- points_read %>%
        dplyr::mutate(mapunit2 = dplyr::case_when(
          mapunit1 == mapunit2 ~ NA_character_,
          TRUE ~ as.character(mapunit2)
        ) )

      # add back sight/ line of site check

      # STILL TO ADD




      # add missing columns if not in data
    if("photos" %in% names(points_read)) {

    } else {
      points_read <- points_read %>%
        dplyr::mutate(photos = NA)
    }


     points_read <- points_read %>%
        dplyr::select(any_of(c("order", "mapunit1", "mapunit2", "point_type", "transect_id",
                               "observer", "transition",  "stuc_stage", "stuc_mod",
                               "date_ymd", "time_hms", "edatope","comments", "photos", "data_type"))) %>%
        dplyr::group_by(transect_id) %>%
        dplyr::arrange(as.numeric(order), by_group = TRUE) %>%
        dplyr::ungroup()

      sf::st_geometry(points_read) <- "geom"

      endlength = length(points_read$order)

      if(endlength != start_length){

        print("length of input file does not match cleaned file review raw data:")
        print(x)
      }

      points_read

    }

  }

  return(all_points)
  # add fid unique value
}




.fill_observer <- function(input_data){

  #input_data <- points_read

  observer_key <- input_data %>%
    dplyr::select(transect_id, observer) %>%
    dplyr::rename("observer_fill" = observer) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct()  %>%
    na.omit() %>%
    dplyr::mutate(observer_fill = trimws(observer_fill, which = "both")) %>%
    dplyr::filter(observer_fill != "")

  if(length(observer_key$transect_id) != length(unique(observer_key$transect_id))) {

    stop(print(" number of observers does not match unique transect number"))

  }

  input_data <- input_data %>%
    dplyr::group_by(transect_id) %>%
    tidyr::fill(observer, .direction = "downup") %>%
    dplyr::ungroup()

  return(input_data)
}

