#' Format field data
#'
#' Standardizes format from raw field data to standard point data
#'
#' @param transect_layout_buf sf spatial object with transect buffered data
#' @param datafolder text string with location of raw files in shp or gpk format
#' @author Genevieve Perkins
#' @import foreach
#' @return sf point data
#' @export
#'
#' @examples
#'
#' clean_pts <- format_fielddata(inputfolder, transect_layout)

format_fielddata <- function(datafolder = NULL, transect_layout_buf){

  #datafolder <- rawdat

  if(is.null(datafolder)) {
    print("raw data folder location is missing")
  }

  points <- list.files(file.path(datafolder), pattern = ".gpkg$|.shp$", full.names = TRUE, recursive = TRUE)
  # points <-  points[c(1:3,5:18)]
  # 4 = turn off duplicates
  # 9 = names issues

  all_points <- foreach(x = points, .combine = rbind) %do% {

    # x = points[18]
    print(x)

    s1_layers <- sf::st_layers(x)
    pts <- which(s1_layers[["geomtype"]] %in% c("Point","3D Point","3D Measured Point"))

    if(length(pts)>0) {

      #print(x)

      points_read <- sf::st_read(x, quiet = TRUE) %>%
        sf::st_transform(3005) %>%
        sf::st_zm() %>%
        dplyr::rename_all(.funs = tolower)


      sf::st_geometry(points_read) <- "geom"

      start_length = length(points_read$geom)

      # if( "name" %in% names(points_read)){
      #
      #   start_length = length(points_read$name)
      #
      # } else {
      #
      #   start_length = length(points_read$geometry)
      # }

      # 1) transect name

      if("f01_transec" %in% names(points_read)){
        dnames = names(points_read)
        colnames(points_read) <- gsub("f0", "x0", dnames)
        points_read <- points_read %>%
          dplyr::mutate(x10_edatope = f10_edatope)
      }

      if("f01_transe" %in% names(points_read)){
        dnames = names(points_read)
        colnames(points_read) <- gsub("f0", "x0", dnames)

      }

      if("f01_transect_id" %in% names(points_read)){
        dnames = names(points_read)
        colnames(points_read) <- gsub("f0", "x0", dnames)

      }

      if("x01_trans" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transect_id = x01_trans)
      }

      if("x01_transec" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transect_id = x01_transec)
      }

      if("x01_transe" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::mutate() %>%
          dplyr::rename(transect_id = x01_transe)
      }

      if("x01_transect_id" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::mutate() %>%
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

      if("x02_observ" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::mutate() %>%
          dplyr::rename(observer = x02_observ)
      }


      if("x1observer" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(observer = x1observer)
      }

      # 3) point_type
      if("x03_pt_type" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(point_type = x03_pt_type)
      }
      # 3) point_type
      if("x03_pt_typ" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(point_type = x03_pt_typ)
      }

      # 3) point_type
      if("x6pointtype" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(point_type = x6pointtype)
      }
      # 3) point_type
      if("pt_type" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(point_type = pt_type)
      }


      # 4) Mapunit1
      if("x04_mapunit" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit1 = x04_mapunit)
      }

      # 4) Mapunit1
      if("x04_mapuni" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit1 = x04_mapuni)
      }

      if("x04_mapunit1" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit1 = x04_mapunit1)
      }

      #  Mapunit1
      if("x2mapunit1" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit1 = x2mapunit1)
      }
      #  Mapunit1
      if("x2mapunit" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit1 = x2mapunit)
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

      if("x4mapunit2" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit2 = x4mapunit2)
      }

      # 5) Mapunit2
      if("x06_mapuni" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(mapunit2 = x06_mapuni)
      }

      # 6) transtition
      if("x05_transit" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transition = x05_transit)
      }

      if("x05_transi" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transition = x05_transi)
      }

      if("x05_transition" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transition = x05_transition)
      }
      if("x3transitio" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(transition = x3transitio)
      }


      # 7) Stand struc
      if("x07_struc_" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_stage = x07_struc_)
      }

      if("x07_struct" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_stage = x07_struct)
      }

      if("x07_struct_" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_stage = x07_struct_)
      }

      if("x07_struct_stage" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_stage = x07_struct_stage)
      }
      if("x7structsta" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_stage = x7structsta)
      }


      # 8) Stand struc
      if("x08_struct" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_mod = x08_struct)
      }

      # 8) Stand struc
      if("x08_struct_" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_mod = x08_struct_)
      }

      if("x08_struct_stage_mod" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(struc_mod = x08_struct_stage_mod)
      }


      # 9) Edatope
      if("x10_edatope" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(edatope = x10_edatope)
      }

      # 9) Edatope
      if("x6edatope" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(edatope = x6edatope)
      }


      # # 9) Edatope
      # if("f10_edatope" %in% names(points_read)){
      #   points_read <- points_read %>%
      #     dplyr::rename(edatope = f10_edatope)
      # }
      #

      # 10) comments
      if("x5comments" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(comments = x5comments)
      }
      if("x09_comment" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(comments = x09_comment)
      }

      if("x09_comments" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(comments = x09_comments)
      }

      if("x5comments" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(comments = x5comments)
      }
      if("x09_commen" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::rename(comments = x09_commen)
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

      #fix the point order

      if("name" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::mutate(order = as.numeric(gsub("Placemark ", "", name)))
      }

      if("objectid" %in% names(points_read)){
        points_read <- points_read %>%
          dplyr::mutate(order = as.numeric(objectid))
      }

      if(("order" %in% names(points_read)) == FALSE){
        points_read <- points_read %>%
          dplyr::mutate(order = as.numeric(seq(1, length(points_read$geom),1)))
      }

      #print(length(points_read$order))
      #names(points_read)

      # Transect id
      if("id" %in% names(points_read)){

        #print("transect id already present")

      } else {
        points_read <- points_read %>%
          sf::st_join(., transect_layout_buf, join = st_intersects) %>%
          dplyr::rename_all(.funs = tolower) %>%
          dplyr::distinct()

        points_read <- points_read %>%
          dplyr::mutate(id = gsub("\\s", "", id)) %>%
          dplyr::mutate(transect_id = id)
      }
      #  st_write(points_read, "testpts.gpkg")


      #print(length(points_read$order))

      points_read <- points_read %>%
        dplyr::mutate(data_type = ifelse(is.na(transect_id), "incidental", "s1"))

      # convert "" to NA then check observer
      points_read <- points_read %>%
        dplyr::mutate(observer = stringr::str_trim(observer)) %>%
        dplyr::mutate(observer = dplyr::na_if(observer, ""))

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

        # add missing columns if not in data
        if("pdfmaps_ph" %in% names(points_read)) {
          points_read <- points_read %>%
            dplyr::mutate(photos = pdfmaps_ph)

        } else {

          points_read <- points_read %>%
            dplyr::mutate(photos = NA)
        }
      }

      # add missing columns if not in data

      if("comments" %in% names(points_read)) {

      } else {

        points_read <- points_read %>%
          dplyr::mutate(comments = NA)
      }
    }


    # add missing columns if not in data
    if("edatope" %in% names(points_read)) {

    } else {

      points_read <- points_read %>%
        dplyr::mutate(edatope = NA)
    }


    # add missing columns if not in data
    if(("date_ymd" %in% names(points_read))== FALSE) {
      points_read <- points_read %>%
        dplyr::mutate(date_ymd = NA,
                      time_hms = NA)
    }

    # add missing columns if not in data
    if("struc_stage" %in% names(points_read)) {

    } else {

      points_read <- points_read %>%
        dplyr::mutate(struc_stage = NA)
    }
    # add missing columns if not in data
    if("struc_mod" %in% names(points_read)) {

    } else {

      points_read <- points_read %>%
        dplyr::mutate(struc_mod = NA)
    }
    # subset and export

    points_read <- points_read %>%
      dplyr::select(any_of(c("order", "mapunit1", "mapunit2", "point_type", "transect_id",
                             "observer", "transition",  "struc_stage", "struc_mod",
                             "date_ymd", "time_hms", "edatope","comments", "photos", "data_type"))) %>%
      dplyr::group_by(transect_id) %>%
      dplyr::arrange(as.numeric(order), by_group = TRUE) %>%
      dplyr::ungroup()

    sf::st_geometry(points_read) <- "geom"

    endlength = length(points_read$order)

    if(endlength != start_length){

      # stop(print("length of input file does not match cleaned file review raw data:"))
      print(x)
    }

    points_read

    }

return(all_points)
# add fid unique value
 }




.fill_observer <- function(input_data){

  # input_data <- points_read

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

