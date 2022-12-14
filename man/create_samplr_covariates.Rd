% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_samplr_covariates.R
\name{create_samplr_covariates}
\alias{create_samplr_covariates}
\title{Generate landscape level covariate rasters for cLHS sample plan}
\usage{
create_samplr_covariates(
  dtm,
  rtemplate,
  SAGApath = "",
  output = "./cv-rasters",
  sieve_size = 10
)
}
\arguments{
\item{dtm}{is a 25m dtm raster object ideally cropped to the AOI watershed boundaries}

\item{rtemplate}{template of 25m raster to match final output to}

\item{SAGApath}{Is the location of SAGA on your system.  On linux systems with SAGA GIS installed Use \code{SAGApath = ""}}

\item{output}{Location of where rasters will be saved.}

\item{sieve_size}{Remove isolated clusters of below the threshold number of cells}
}
\description{
Generates base landscape covariates from a 25m TRIM DEM using SAGA GIS and converts to classes for use in the sample plan cLHS
}
\details{
This script has been tested with SAGA 8.4 on Windows
Depending on your system the path to \code{saga_cmd} may need to be specified.
}
\keyword{SAGA,}
\keyword{covariates,}
\keyword{predictors,}
\keyword{raster}
