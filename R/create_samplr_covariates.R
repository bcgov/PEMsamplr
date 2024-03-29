#' Generate landscape level covariate rasters for cLHS sample plan
#'
#' Generates base landscape covariates from a 25m TRIM DEM using SAGA GIS and converts to classes for use in the sample plan cLHS
#'
#' This script has been tested with SAGA 8.4 on Windows
#' Depending on your system the path to `saga_cmd` may need to be specified.
#' @param dtm is a 25m dtm raster object ideally cropped to the AOI watershed boundaries
#' @param SAGApath Is the location of SAGA on your system.  On linux systems with SAGA GIS installed Use `SAGApath = ""`
#' @param output Location of where rasters will be saved.
#' @param sieve_size Remove isolated clusters of below the threshold number of cells
#' @param rtemplate template of 25m raster to match final output to
#' @param dah_threshold dah threshold to classify the dah layer into the three respective classes
#' @param saga_param named list. Parameters that are fed to SAGA MRVBF function. IF supplied it must contain all parameters specified in the SAGA command.
#' @param covariates character vector of covariates to create. Can be any of c("mrvbf", "dah", "landform")
#' @importFrom magrittr "%>%"
#' @keywords SAGA, covariates, predictors, raster
#' @return **SpatRast** series of rast objects
#' @export
#' @examples
#' create_samplr_covariates(dtm = dtm,rtemplate = r25,SAGApath ="C:/SAGA/saga-7.7.0_x64/",output =  output,covariates = c("mrvbf", "dah", "landform"),sieve_size = 10,dah_threshold = 0.2,saga_param = list(T_SLOPE = 64, TPCTL_V = 6, T_PCTL_R = 2,P_SLOPE = 4.0, P_PCTL = 3.0, UPDATE = 1,CLASSIFY = 1, MAX_RES = 100))

create_samplr_covariates <- function(dtm = dtm,
                                     rtemplate = rtemplate,
                                     SAGApath = "",
                                     output = NULL,
                                     covariates = c("mrvbf", "dah", "landform"),
                                     sieve_size = 10,
                                     dah_threshold = 0.2,
                                     saga_param = list(T_SLOPE = 64, TPCTL_V = 6, T_PCTL_R = 2,
                                                    P_SLOPE = 4.0, P_PCTL = 3.0, UPDATE = 1,
                                                    CLASSIFY = 1, MAX_RES = 100)
                                     ){

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

  # set covariates to include all 3 covariates if no input is given

  if (is.null(covariates)) covariates <- c("mrvbf", "dah", "landform")
 ## 1.  Landscape MRVBF
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html

  if (any(covariates == "mrvbf")){

    if (is.null(saga_param)){

      saga_param <- list(T_SLOPE = 64, TPCTL_V = 6, T_PCTL_R = 2,
                         P_SLOPE = 4.0, P_PCTL = 3.0, UPDATE = 1,
                         CLASSIFY = 1, MAX_RES = 100)
    }

  sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                  sDTM,
                  "-MRVBF", MRVBF,
                  "-MRRTF", MRRTF,                       # Outputs
                  "-T_SLOPE", saga_param$T_SLOPE,
                  "-T_PCTL_V", saga_param$TPCTL_V,
                  "-T_PCTL_R", saga_param$T_PCTL_R,    # Default Parameters
                  "-P_SLOPE", saga_param$P_SLOPE,
                  "-P_PCTL", saga_param$P_PCTL,
                  "-UPDATE", saga_param$UPDATE,
                  "-CLASSIFY", saga_param$CLASSIFY,
                  "-MAX_RES", saga_param$MAX_RES
  )
  system(sysCMD)

  # sieve and then threshold

  mrvbf_r <- terra::rast(gsub(".sgrd", ".sdat", MRVBF)) %>%
    terra::sieve(threshold = sieve_size, directions=8)# %>%

  mrvbf_rcrop <- terra::crop(mrvbf_r, rtemplate)

  terra::writeRaster(mrvbf_rcrop, file.path(saga_tmp_files, "mrvbf_LS.tif"), overwrite = TRUE)

}
  ## 2.  Landscape Diuranal Anisotropic Heating
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html


  if (any(covariates == "dah")){

  sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                  sDTM,
                  "-DAH", dah,    # Output
                  "-ALPHA_MAX", 202.5   # Default Parameters
  )
  system(sysCMD)

  # reclass and threshold dah

  dah_r <- terra::rast(gsub(".sgrd", ".sdat", dah))

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

  rc_crop <- terra::crop(rc, rtemplate)

  terra::writeRaster(rc_crop, file.path(saga_tmp_files, "dah_LS.tif"), overwrite = TRUE)
}

  ## 3. Landform Class
  if (any(covariates == "landform")){
  land_class <- create_landform_classes(dtm, scale = 75, sn = 3, ln = 7, n.classes = "six")

  # sieve landclass
  land_class <- land_class %>%
    terra::sieve(threshold = sieve_size, directions=8) %>%
    terra::subst(from = 0, to = NA)

  land_class <- terra::crop(land_class, rtemplate)
  names(land_class)<- "landclass"

  terra::writeRaster(land_class, file.path(saga_tmp_files, "landform_LS.tif"), overwrite = TRUE)

  # remove temp Saga files
  # unlink(paste(output, "saga", sep = "/"), recursive = TRUE)
  # delete the files
  }
  to_delete <- grep(".tif", list.files(file.path(saga_tmp_files), full.names = TRUE), value = T, invert = TRUE)
  file.remove(to_delete)

  return(TRUE)

}
