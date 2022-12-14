% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_cost_penalty.R
\name{.assign_highcost}
\alias{.assign_highcost}
\title{Create cost penalty layer}
\usage{
.assign_highcost(shape, crs = 3005, costval, cost)
}
\arguments{
\item{costval}{Numeric value of assigned high cost}

\item{cost}{SpatRast or Raster of cost layer}

\item{vec_dir}{text string with folder location of base vector layers}

\item{dem}{SpatRast or Raster of cost layer}
}
\value{
SpatRast
}
\description{
Create cost penalty layer
}
\examples{
create_cost_penalty(vec_dir, dem, cost, costval = 3000)

}
