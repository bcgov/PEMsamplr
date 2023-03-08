#' Generate training point summary
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#'
#'
#' @param out_dir  output directory  This defaults to the project's root directory OR where the RMD script is saved.
#' @param tpts  training points cleaned
#' @param trans processed transect lines
#' @keywords training data report
#' @export
#' @examples
#'
#'

trainingpt_report <- function(tpts, trans,
                           out_dir){
  # # # testing : GP
   #tpts = pts
   #out_dir = cleandat

  ## create destination folder
  ifelse(!dir.exists(file.path(out_dir)),                # if folder does not exist
         dir.create(file.path(out_dir)), FALSE)         # create it

  RMD <- system.file("rmd_template", "report.rmd", package ="PEMsamplr")

  rmarkdown::render(RMD,
                    params = list(tpts = tpts,
                                  trans = trans, ## parameters to send to rmarkdown
                                  out_dir = out_dir),
                    output_dir = out_dir)                ## where to save the report

  #file.rename(paste0(out_dir,"/", "trainingpt_report.html"), paste0(out_dir,"/","field_training_pt_report.html"))
  ## open the report
  browseURL(paste0(paste0(out_dir,"/","report.html")))
}
