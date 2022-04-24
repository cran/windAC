
#' @name geometricRoadPadPropSearchTable
#'
#' @title Calculate the areas of intersection of a series of nested annuli with an idealized access road and turbine pad.
#'
#'
#' @description Calculate area of annulus bisected by 2 parallel lines (e.g. a
#'   road of a road/pad plot).
#'
#' @param padRadius Integer, radius of turbine pad from the center of the turbine.
#' @param roadWidth Integer, width of road leading to turbine pad.
#' @param maxSearchRadius Integer, maximum search distance from the center of turbine.
#' @param mastRadius Integer, radius of the turbine mast.
#' @param annulusWidth Integer, width of annulus, default is 1.
#' @param ... Currently ignored.
#'
#' @details Searches are conducted on the road and turbine pad around wind
#'   turbines for bird and bat fatalities. This function creates a data frame of
#'   proportion of area searched within each annulus ring on an idealized road and pad.
#' The turbine is assumed to be centered on a perfectly circular turbine pad with radius \code{padRadius}, and a perfectly straight access road of width \code{roadWidth} is oriented from the center of the circle away from the turbine.
#' (The resulting road and pad looks like rather like a lollipop.)
#'
#'   The \code{mastRadius} argument is to account for the area taken up by the turbine mast.
#'
#'   The arguments \code{padRadius}, \code{roadWidth}, \code{mastRadius}, and
#'   \code{annulusWidth} are all rounded to the nearest integer. The \code{maxSearchDistance}
#'   is rounded up (ceiling function) to an integer. If half units are needed, then
#'   convert to a smaller unit. See examples.
#'
#' @return Data frame of proportion of area searched for each annulus.  \code{distanceFromTurbine} column represents the outer radius of each annulus.
#'
#' @export geometricRoadPadPropSearchTable
#'
#' @seealso geometricRectanglePropSearchTable circleBoxInt
#'
#' @examples
#'
#' pad <- 10 #meters, turbine pad radius
#' road <- 4 #meters, width of the road to the turbine pad
#' maxDistance <- 100 #meters, max distance
#' mast <- 2 #meters, turbine mast radius
#'
#' ## proportion are area searched at each annulus
#' propSearch <- geometricRoadPadPropSearchTable(padRadius = pad,
#'                                               roadWidth = road,
#'                                               maxSearchRadius = maxDistance,
#'                                               mastRadius = mast)
#' head(propSearch, 20)
#'
#' ## if half meter annulus rings are desired:
#' convert <- 100 # meters * 100 = centimeters
#'
#' ## units in centimeters
#' propSearchHalfMeter <- geometricRoadPadPropSearchTable(padRadius = pad * convert,
#'                                               roadWidth = road*convert,
#'                                               maxSearchRadius = maxDistance * convert,
#'                                               mastRadius = mast * convert,
#'                                               annulusWidth = 50) ##50cm = half a meter
#' head(propSearchHalfMeter, 30)
#'
#' ## convert back to meters
#' propSearchHalfMeter$distanceFromTurbine <- propSearchHalfMeter$distanceFromTurbine/convert
#' head(propSearchHalfMeter, 30)
#'


geometricRoadPadPropSearchTable <- function(padRadius,roadWidth,maxSearchRadius,
                                            mastRadius,annulusWidth=1,...){
    ## ## for testing
    ## padRadius <- 10
    ## roadWidth <- 4
    ## maxSearchRadius <- 100
    ## annulusWidth <- 1
    ## mastRadius <- 2


    ## Check arguments.
    theseArgs <- list(padRadius=padRadius,roadWidth=roadWidth,maxSearchRadius=maxSearchRadius,
                      annulusWidth=annulusWidth,mastRadius=mastRadius)

    for(j in seq_along(theseArgs)){
        thisArg <- theseArgs[[j]]
        if(length(thisArg)!=1 || !is.numeric(thisArg) || ifelse(names(theseArgs)[j]=='mastRadius',any(thisArg<0),any(thisArg<=0))){
            stop('Argument ', names(theseArgs)[j], ' needs to be a single integer value greater than zero.')
        }#end if
    }#end for j


    ## Set variables.
    (pR <- padRadius)
    (rW <- roadWidth)
    (maxSearchDist <- maxSearchRadius)
    (aW <- annulusWidth)
    (mR <- mastRadius)
    (halfRoadWidth <- rW/2)

    ## this is a true radius so the annulus width must be discounted.
    (pR <- round(padRadius))

    (outerRadius <- unique(c(seq(mR,pR,by=aW),seq(pR,maxSearchDist,by=aW),maxSearchDist)))
    ##(outerRadius <- unique(c(seq(mR,maxSearchDist,by=aW),maxSearchDist,pR)))
    outerRadius <- sort(outerRadius)


    sectorArea <- c()
    for(i in 2:length(outerRadius)){
        sectorArea[i-1] <- 2*(circleBoxInt(S=halfRoadWidth,R=outerRadius[i],L=maxSearchDist)-circleBoxInt(S=halfRoadWidth,R=outerRadius[i-1],L=maxSearchDist))

    } # end for i


    ## put into data.frame with a distance column outerRadius
    propSearch <- data.frame(distanceFromTurbine=outerRadius[-1]-mR,
                             distanceFromTurbCenter=outerRadius[-1],
                             areaSearched=sectorArea,
                             annulusArea=(diff(outerRadius^2)*pi),
                             proportionAreaSearched=sectorArea/(diff(outerRadius^2)*pi))

    ## all of the pad is considered searched
    propSearch$proportionAreaSearched[propSearch$distanceFromTurbCenter<=pR] <- 1

    ## remove from output
    propSearch$distanceFromTurbCenter <- NULL

    return(propSearch)


} #end geometricRoadPadPropSearchTable function
