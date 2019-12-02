#' @name hallingstad
#'
#' @title Data sets from Hallingstad et al. 2018
#'
#' @description List containing two data frames from Hallingstad et al. 2018
#'
#' @usage data(hallingstad)
#'
#' @details
#' A list of data frames, with two elements named \code{carcass} and \code{weight}.
#'
#'
#' @format The carcass element is a data frame with 26 rows and 3 variables:
#' \describe{
#'   \item{study}{The name of the study for that wind farm.}
#'   \item{species}{The species of the carcass found.}
#' \item{distanceFromTurbine}{The distance (meters) from the turbine of the location of the found carcass.}
#' }
#'
#' The weight element is a data frame with 2414 rows and 7 variables:
#' \describe{
#'   \item{study}{The name of the study for that wind farm.}
#' \item{distanceFromTurbine}{The distance (meters) from the turbine for the outer radius of a one meter ring or annulus.}
#' \item{detectionProbability}{The probability of detection, incorporating search efficiency and carcass persistence, for a given study and distance from turbine.}
#' \item{proportionAreaSearch}{The proportion of area searched within the ring (annulus).}
#' \item{scaledNumberTurbine}{The number of turbines at a study divided by the total number of turbines for all studies considered, does not change with distance from turbine. }
#' \item{scaledFatalityRate}{The fatality rate at a study divided by the sum of the fatality rates across all studies considered, does not change with distance from turbine.}
#' \item{finalWeight}{The product of
#' \code{detectionProbability*proportionAreaSearched*scaledNumberTurbine*scaledFatalityRate}.}
#' }
#'
#'
#'
#'
#' @references Hallingstad EC, Rabie PA, Telander AC, Roppe JA, Nagy LR (2018) Developing an efficient protocol for monitoring eagle fatalities at wind energy facilities. PLoS ONE 13(12): e0208700. https://doi.org/10.1371/journal.pone.0208700
#'
#'

"hallingstad"

