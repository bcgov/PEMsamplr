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
#' @param layers The covariates that will be generated.  A full list of covariates is listed at: ADD
#' @param sieve_size Remove isolated clusters of below the threshold number of cells
#'
#' @keywords SAGA, covariates, predictors, raster
#' @export
#' ##

# setwd("D:/GitHub/PEMsamplr")
# dtm <- ("./temp_data/dem.tif")
# SAGApath <- "C:/SAGA/"
# layers = "all"
# output = "./landscape_covariates"
# sieve_size = 10

create_samplr_covariates <- function(dtm, SAGApath = "C:/SAGA/",
                              output = "./landscape_covariates",
                              layers = "all",
                              sieve_size = 10){
  {
  ### In future this would be good to set as a lookup table and then have a single
  # sub-function that uses the table parameters
    dtm2 <- raster::raster(dtm)
    dtm <- terra::rast(dtm)

  ####### Options -- All the possible covariates ########
  options <- c("slope_aspect_curve","dah", "TPI" , "MultiResFlatness")


  ####### flag all to run #######################
  if (layers == "all") {  ## currently gives warning ... but functions as expected.
    layers <- options
  }


  ####### Error handling -- unspecified layers ############
  err.layers <- setdiff(layers, options)

  if (length(err.layers) == 1) {
    stop(paste(err.layers, "is not a valid covariate" ))
  }

  if (length(err.layers) > 1) {
    print(err.layers)
    stop("Specified covariates are not a valid options" )
  }


  ############# Set up Environment ########################

  ##### Link SAGA to R --------------------------------------------------
  if(Sys.info()['sysname']=="Windows"){saga_cmd = paste0(SAGApath, "saga_cmd.exe")
  } else {saga_cmd = "saga_cmd"}  ;
  z<- system(paste(saga_cmd, "-v"), intern = TRUE)  ## prints that SAGA version number -- confirming it works.
  print(z)


  # OUTPUTS: ------------------------------------------------------------
  ifelse(!dir.exists(file.path(output)),              #if tmpOut Does not Exists
         dir.create(file.path(output)), print("Directory Already Exists"))        #create tmpOut

  saga_tmp_files <- paste0(output,"/saga/")
  ifelse(!dir.exists(file.path(saga_tmp_files)),              #if tmpOut Does not Exists
         dir.create(file.path(saga_tmp_files)), print("Directory Already Exists"))        #create tmpOut


### A HACK THAT WOULD BE GOOD TO FIX
  ## Convert to Saga format for processing ---------------------------------------
  rtnwd <- getwd() ## wd to return to
  setwd(saga_tmp_files)

  sDTM <- "dtm.tif"
  # sDTM <- paste0(saga_tmp_files, sDTM)
  #terra::writeRaster(sDTM, saga_tmp_files, overwrite = TRUE)  # drivername = "GTiff",save SAGA Version using rgdal
  raster::writeRaster(dtm2, sDTM,  overwrite = TRUE)#drivername = "GTiff",
  ## Bit of a hack here -- SAGA does not like the output from raster package
  ## save it as gTiff, re-open using rgdal and export as SAGA ...
  dtm <- rgdal::readGDAL(sDTM)

  sDTM <- "dtm.sdat"
  ## If the file exists delete and save over.
  if(file.exists(sDTM)){
    unlink(sDTM)
    rgdal::writeGDAL(dtm, sDTM, drivername = "SAGA")  ## TRUE
  } else {
    rgdal::writeGDAL(dtm, sDTM, drivername = "SAGA" )               ## FALSE
  }
  ## END HACK ------------------



  ############################### BEGIN GENERATING cOVARIATES#################

  ##### >> 2 -- Slope Aspect and Curvature -------------------------------

  if ("slope_aspect_curve" %in% layers) {
    # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html
    slope <- "slope.sgrd"
    aspect <- "aspect.sgrd"

    sysCMD <- paste(saga_cmd, "ta_morphometry 0", "-ELEVATION",
                    # file.path(gsub("sdat","sgrd", sDTM)),     # Input DTM
                    sDTM,
                    "-SLOPE", slope,
                    "-ASPECT", aspect,                     # Outputs
                    "-METHOD", 6,
                    "-UNIT_SLOPE", 0,
                    "-UNIT_ASPECT", 0       # Default Parameters
    )
    system(sysCMD)
  }

   ##### >> 8 -- Landscape MRVBF -----------------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html
  if ("MultiResFlatness" %in% layers) {

    MRVBF <- "mrvbf.sgrd"
    MRRTF <- "mrrtf.sgrd"

    # MRVBF = file.path(tmpOut, MRVBF)
    # MRRTF  = file.path(tmpOut, MRRTF)

    # use new landscape parameters
    sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)),
                    sDTM,
                    "-MRVBF", MRVBF,
                    "-MRRTF", MRRTF,                       # Outputs
                    "-T_SLOPE", 64,
                    "-T_PCTL_V", 6,
                    "-T_PCTL_R", 2,    # Default Parameters
                    "-P_SLOPE", 4.0,
                    "-P_PCTL", 3.0,
                    "-UPDATE", 1,
                    "-CLASSIFY", 1,
                    "-MAX_RES", 100
    )
    system(sysCMD)
  }

  ##### >> 12 -- Diuranal Anisotropic Heating -----------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html

  if ("dah" %in% layers) {
    dah <- "dah.sgrd"
    sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)), # Input DTM
                    sDTM,
                    "-DAH", dah,    # Output
                    "-ALPHA_MAX", 202.5   # Default Parameters
    )
    system(sysCMD)
  }



  ################ Covariate Generation Complete ####################

  #### Convert .sdat to to GeoTif --------------------------------
  setwd(rtnwd)
  setwd("D:/GitHub/PEMsamplr")

  tmpFiles <- paste(output, "saga", sep = "/")
  l <- list.files(path = tmpFiles, pattern = "*.sdat")
  l <- l[!grepl(".xml", l)] ## removes xmls from the list
  print(l)

  ## OutFile Suffix Use resolution as suffix for out filename
  r <- terra::rast(paste(tmpFiles, l[1], sep= "/"))
  #subFolder <- raster::res(r)[1]  ##
  #suf <- paste0("_", subFolder, ".tif")
  outList <- gsub(".sdat", "_ls.tif", l)

  ## Loop through files and convert to tif
  for(i in 1:length(l)){

    ## parms for testing
    # i <- 1

    #actions
    r <- l[i]
    inFile <- paste(tmpFiles, r, sep = "/")
    # print(inFile)
    r <- terra::rast(inFile)

    outFile <- paste(output,  outList[i], sep = "/")  ## Names output

    ifelse(!dir.exists(file.path(paste(output,  sep = "/"))), #if tmpOut Does not Exists
           dir.create(file.path(paste(output,  sep = "/"))),
           "Directory Already Exisits")        #create tmpOut

    terra::writeRaster(r, outFile, overwrite = TRUE)  ## Saves to landscape_cov


  }

  ## Remove tmp saga files
  unlink(paste(output, "saga", sep = "/"), recursive = TRUE)
}

#####Convert covariates into classes##############

#### Landform classes
#source("D:/GitHub/PEMsamplr/R/create_landform_classes.R")
land_class <- create_landform_classes (dtm2)
land_class <- terra::rast(land_class) %>%
  terra::sieve(threshold = sieve_size, directions=8) %>%
  terra::subst(from = 0, to = NA)
#terra::plot(land_class)
outFile <- paste(output,  "landform_ls.tif", sep = "/")
terra::writeRaster(land_class, outFile, overwrite = TRUE)

### DAH classes
#source("D:/GitHub/PEMsamplr/R/create_aspect_classes.R")
dah <- terra::rast(file.path(paste(output,  "dah_ls.tif", sep = "/")))
aspect_class <- create_aspect_classes (dah) %>%
  terra::sieve(threshold = sieve_size, directions=8) %>%
  terra::subst(from = 0, to = NA)
terra::plot(aspect_class)
outFile <- paste(output,  "dah_ls.tif", sep = "/")
terra::writeRaster(aspect_class, outFile, overwrite = TRUE)

## MRVBF classes
mrvbf <- terra::rast(file.path(paste(output,  "mrvbf_ls.tif", sep = "/"))) # %>%
  # terra::sieve(threshold = sieve_size, directions=8) %>%
  # terra::subst(from = 0, to = NA)
terra::plot(mrvbf )
outFile <- paste(output,  "mrvbf_ls.tif", sep = "/")
terra::writeRaster(mrvbf, outFile, overwrite = TRUE)


}

