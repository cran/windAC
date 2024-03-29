% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getProportionAreaSearched.R
\name{getProportionAreaSearched}
\alias{getProportionAreaSearched}
\title{Create proportion of area searched table from spatial data}
\usage{
getProportionAreaSearched(
  turbinePoints,
  turbineName,
  turbinePlots,
  turbineMastRadius,
  maxDistance
)
}
\arguments{
\item{turbinePoints}{Spatial points object with with data frame indicating turbine names}

\item{turbineName}{Character, indicating the variable name for the turbine names in \code{turbinePoints} and plot names in \code{turbinePlots}}

\item{turbinePlots}{Spatial polygon objects indicating the search area around the turbine points}

\item{turbineMastRadius}{Integer of length 1. radius of the turbine mast}

\item{maxDistance}{Integer, indicating how far from the turbine that searches occurred}
}
\value{
Data frame of proportion of area searched for each annulus around each turbine point. \code{distanceFromTurbine} column represents outer radius of each annulus.
}
\description{
Calculate proportion of area searched around wind turbine based on turbine location data and polygons of search area.
}
\details{
The \code{\link[sf]{sf}} package is used to calculate overlapping areas between the searched area \code{turbinePlots} and one unit annulus around the \code{turbinePoints}. The annuli increase out to a distance of \code{maxDistance}.

Caution, the function does some basic checks on the spatial objects but it is assumed that the points and polygons do not have any boundary, geometry, or other issues.
}
\examples{


data(turbineSpatial)

propSearch <- getProportionAreaSearched(turbinePoints=turbineSpatial$turbinePoints,
turbineName='turbName',turbinePlots=turbineSpatial$turbinePlots,
 turbineMastRadius=2,maxDistance=10)


}
