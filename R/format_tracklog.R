#' Format Track log data
#'
#' Standardize tracks collected during the field sampling process
#'
#' @param datafolder text string with location of raw files in shp or gpk format
#' @param transect_layout_buf sf spatial object with transect buffered data
#'
#' @return sf multiline vector
#' @export
#'
#' @examples
#' clean_tracks <- format_tracklog(inputfolder, transect_layout)
#
format_tracklog <- function(datafolder, transect_layout_buf){

# datafolder <- rawdat

  lines <- list.files(file.path(datafolder), pattern = ".gpkg$|.shp$", full.names = TRUE, recursive = TRUE)
 # lines <- lines[1000:1070]

  all_lines <- foreach(x = lines, .combine = rbind) %do% {

   #x = lines[2]

    s1_layers <- sf::st_layers(x)
    lns <- which(s1_layers[["geomtype"]] %in% c("LINE","LINESTRING","3D Line String", "3D Measured Multi Line String"))

    if(length(lns)>0) {

      print(x)

      tdat <- sf::st_read(x, quiet = TRUE) %>%
        sf::st_transform(3005) %>%
        sf::st_zm() %>%
        dplyr::rename_all(.funs = tolower)

      # if more than onw linestring then check?


      #start_length = length(tdat$name)

      if("timestamp" %in% names(tdat)){

        tdat<- tdat %>%
          dplyr::mutate(date_ymd = lubridate::as_date(tdat$timestamp))

        if(stringr::str_length(tdat$timestamp[1])>10){

          tdat <- tdat %>%
            dplyr::mutate(date_time = lubridate::as_datetime(tdat$timestamp))

          #  points_read <- points_read %>%
          #    dplyr::mutate(time_hms =  hms::as_hms(timestamp))

          tdat <- tdat %>%
            dplyr::mutate(time_hms = format(date_time, format = "%H:%M:%S"))

        } else {

          tdat <- dplyr::mutate(tdat, time_hms = NA)

        }

      } # end of time section


      # Transect id

      if(anyNA(st_is_valid(tdat) == T)){
        inval <- st_is_valid(tdat)
        which(inval == TRUE)

        fixed <- tdat[which(inval == TRUE),]
        tdat <- fixed

        }

      tdat <- tdat %>%
        sf::st_join(., transect_layout_buf, join = st_intersects) %>%
        dplyr::rename_all(.funs = tolower) %>%
        dplyr::distinct()

      tdat <- tdat %>%
        dplyr::mutate(id = gsub("\\s", "", id)) %>%
        dplyr::mutate(transect_id = id)

      #print(length(points_read$order))

      tdat <- tdat %>%
        dplyr::mutate(data_type = ifelse(is.na(transect_id), "incidental", "s1"))


      # add missing columns if not in data
      if("photos" %in% names(tdat)) {

      } else {
        tdat <- tdat %>%
          dplyr::mutate(photos = NA)
      }


      tdat <- tdat %>%
        dplyr::select(any_of(c("transect_id","date_ymd", "time_hms", "photos", "data_type")))

      sf::st_geometry(tdat) <- "geom"

      tdat

    }

  }

  return(all_lines)
  # add fid unique value
}


