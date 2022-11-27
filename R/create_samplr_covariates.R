#' Generate landscape level covariate rasters for cLHS sample plan
#'
#' Generates base landscape covariates from a 25m TRIM DEM using SAGA GIS and converts to classes for use in the sample plan cLHS
#'
#' This script has been tested with SAGA 8.4 on Windows
#' Depending on your system the path to `saga_cmd` may need to be specified.
#'
#'
#' @param dtm is a 25m dtm raster object ideally cropped to the AOI watershed boundaries
#' @param SAGApath Is the location of SAGA on your system.  On linux systems with SAGA GIS installed Use `SAGApath = ""`
#' @param output Location of where rasters will be saved.
#' @param sieve_size Remove isolated clusters of below the threshold number of cells
#' @import spatialEco magrittr
#' @keywords SAGA, covariates, predictors, raster
#' @export
#' ##

# # get a base raster that is correct size
#  aoi_raw <- system.file("extdata", "aoi.gpkg", package ="PEMprepr")
#  aoi_raw <- sf::st_read(aoi_raw)
#  aoi <- PEMprepr::aoi_snap(aoi_raw, "shrink")
#  t25 <- create_template(aoi, 25)
#  library(bcmaps)
#  trim_raw <- cded_raster(aoi)
#  trim <- terra::rast(trim_raw)
#  dtm <- terra::project(trim, t25)
# sieve_size = 10
#
#  create_samplr_covariates(dtm,
#                               output =  filelist$sampling_input_landscape[[2]],
#                               SAGApath ="C:/SAGA/saga-7.7.0_x64/",
#                                sieve_size = 10)
#


create_samplr_covariates <- function(dtm, SAGApath = "",
                              output = "./cv-rasters",
                              sieve_size = 10){


  # # testing lines
  # # get a base raster that is correct size
  # aoi_raw <- system.file("extdata", "aoi.gpkg", package ="PEMprepr")
  # aoi_raw <- sf::st_read(aoi_raw)
  # aoi <- PEMprepr::aoi_snap(aoi_raw, "shrink")
  # t25 <- create_template(aoi, 25)
  # library(bcmaps)
  # trim_raw <- cded_raster(aoi)
  # trim <- terra::rast(trim_raw)
  # dtm <- terra::project(trim, t25)
  # filelist <- setup_folders("canyoncreek")
  #
  #
  # dtm
  # SAGApath = "C:/SAGA/saga-7.7.0_x64/"
  # output = filelist$sampling_input_2010[[2]]
  # #layers = "all"
  # sieve_size = 10
  #
  # end testing lines

  ##### Link SAGA to R --------------------------------------------------
  if(Sys.info()['sysname']=="Windows"){
    saga_cmd <- paste0(SAGApath, "saga_cmd.exe")
    fns      <- "\\" ### file name separator
  } else {
    saga_cmd <- "saga_cmd"
    fns      <- "/" ### file name separator

  }  ;
  z<- system(paste(saga_cmd, "-v"), intern = TRUE)  ## prints that SAGA version number -- confirming it works.
  z <- print(z)
  v <- suppressWarnings(as.numeric(unlist(strsplit(z, "[[:punct:][:space:]]+")[1])))
  v <- v[!is.na(v)][1:2]
  v <- as.numeric(paste(v[1], v[2], sep = "."))

  if (v < 7.6) {
    warning("SAGA-GIS is less that 7.6.  Not all covariates will generate.  Upgrade your SAGA, visit https://sourceforge.net/projects/saga-gis/files/")
  }


  # OUTPUTS: ------------------------------------------------------------
  ifelse(!dir.exists(file.path(output)),              #
         dir.create(file.path(output)), print("Directory Already Exists"))        #create tmpOut

  #rn <- terra::res(dtm)[1] ## Get the resolution

  saga_tmp_files <- paste(output)
  ifelse(!dir.exists(file.path(saga_tmp_files)),              #if tmpOut Does not Exists
         dir.create(file.path(saga_tmp_files)), print("Directory Already Exists"))        #create tmpOut

  ## Convert to Saga format for processing ---------------------------------------
  sDTM <- "dtm.sdat"
  sDTM <- paste(saga_tmp_files, sDTM, sep= "/")
  terra::writeRaster(dtm, sDTM, overwrite = TRUE)

  ############ Covariate File Names #############################################
  ### This is ugly! re-write
  sDTM <- paste(saga_tmp_files, "dtm.sdat", sep= fns)
  sinksroute <- paste(saga_tmp_files, "sinkroute.sgrd",sep = fns)
  sinksfilled <- paste(saga_tmp_files, "filled_sinks.sgrd", sep = fns)
  MRVBF <- paste(saga_tmp_files, "mrvbf.sgrd", sep = fns)
  MRRTF <- paste(saga_tmp_files, "mrrtf.sgrd", sep = fns)
  dah <- paste(saga_tmp_files, "dah.sgrd", sep = fns)


  # fill sinks in DEM
  sysCMD <- paste(saga_cmd, "ta_preprocessor 5", "-ELEV" ,
                  sDTM,
                  "-FILLED", sinksfilled,
                  "-MINSLOPE ", 0.1
  )
  system(sysCMD)


  ## 1.  Landscape MRVBF
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html

  sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                  sDTM,
                  "-MRVBF", MRVBF,
                  "-MRRTF", MRRTF,                       # Outputs
                  "-T_SLOPE", 16,
                  "-T_PCTL_V", 0.4,
                  "-T_PCTL_R", 0.35,    # Default Parameters
                  "-P_SLOPE", 4.0,
                  "-P_PCTL", 3.0,
                  "-UPDATE", 0,
                  "-CLASSIFY", 0,
                  "-MAX_RES", 100
  )
  system(sysCMD)

  # sieve and then threshold

  mrvbf_r <- terra::rast(gsub(".sgrd", ".sdat", MRVBF)) %>%
    terra::sieve(threshold = sieve_size, directions=8)# %>%
    #terra::subst(from = 0, to = NA)

   #terra::plot(mrvbf_r)

   terra::writeRaster(mrvbf_r, file.path(saga_tmp_files, "mrvbf_LS.tif"), overwrite = TRUE)



  ## 2.  Landscape Diuranal Anisotropic Heating
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html

  sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                  sDTM,
                  "-DAH", dah,    # Output
                  "-ALPHA_MAX", 202.5   # Default Parameters
  )
  system(sysCMD)

  # reclass and threshold dah

  dah_r <- terra::rast(gsub(".sgrd", ".sdat", dah))
  dah_threshold <- 0.2

  # filter based on the 0.3 for Date Cree
  # - review the slope (style into three classes
  #                     - <25% slope or 0.43 radians,
  #                     - 45% slope or 0.43 - 0.78 radians
  #                     - >45% slope
  #                     - once this is stlyed then you can adjust the grouping on the DAH to match
  #                     - Deception = -0.2 to 0.2.
  #                     - Date Creek = -0.3 to 0.3.
  #                     - Peter Hope = -0.2 to 0.2

  m <- c( -10, (dah_threshold*-1), 1,
          (dah_threshold*-1 ), dah_threshold, 2,
          dah_threshold, 10,  3)
  rclmat <- matrix(m, ncol=3, byrow =TRUE)

  rc <- terra::classify(dah_r, rclmat)

  rc <- rc %>%
    terra::sieve(threshold = sieve_size, directions=8)

  terra::writeRaster(rc, file.path(saga_tmp_files, "dah_LS.tif"), overwrite = TRUE)


  ## 3. Landform Class

  land_class <- create_landform_classes(dtm, scale = 75, sn = 3, ln = 7, n.classes = "six")

  # sieve landclass
  land_class <- land_class %>%
    terra::sieve(threshold = sieve_size, directions=8) %>%
    terra::subst(from = 0, to = NA)

  terra::writeRaster(land_class, file.path(saga_tmp_files, "landform_LS.tif"), overwrite = TRUE)



  # remove temp Saga files
  # unlink(paste(output, "saga", sep = "/"), recursive = TRUE)
  # delete the files

}

