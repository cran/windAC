
#' @name geometricRoadPadPropSearchTable
#'
#' @title Create proportion of area searched table from a road and pad plot
#'
#' @description Calculate area of annulus bisected by 2 parallel lines (e.g. a
#'   road of a road/pad plot).
#'
#' @param padRadius Integer, radius of turbine pad.
#' @param roadWidth Integer, width of road leading to turbine pad.
#' @param maxSearchDistance Integer, maximum search distance from the turbine.
#' @param mastRadius Integer, radius of the turbine mast.
#' @param annuliWidth Integer, width of annuli, default is 1.
#' @param ... Currently ignored.
#'
#' @details Searches are conducted on the road and turbine pad around wind
#'   turbines for bird and bat fatalities. This function creates a data frame of
#'   proportion of area searched within each annulus ring.
#'
#'   The \code{mastRadius} argument is to account for the area taken up by the turbine mast.
#'
#'   The arguments \code{padRadius}, \code{roadWidth}, \code{mastRadius}, and
#'   \code{annuliWidth} are all rounded to the nearest integer. The \code{maxSearchDistance}
#'   is rounded up (ceiling function) to an integer. If half units are needed, then
#'   convert to a smaller unit. See examples.
#'
#' @return Data frame of proportion of area searched for each annulus.  \code{distanceFromTurbine} column represents the outer radius of the annuli.
#'
#' @export geometricRoadPadPropSearchTable
#'
#' @seealso geometricRectanglePropSearchTable
#'
#' @examples
#'
#' pad <- 10 #meters, turbine pad radius
#' road <- 4 #meters, width of the road to the turbine pad
#' maxDistance <- 100 #meters, max distance
#' mast <- 2 #meters, turbine mast radius
#'
#' ## proportion are area searched at each annulu
#' propSearch <- geometricRoadPadPropSearchTable(padRadius = pad,
#'                                               roadWidth = road,
#'                                               maxSearchDistance = maxDistance,
#'                                               mastRadius = mast)
#' head(propSearch, 20)
#'
#' ## if half meter annulus rings are desired:
#' convert <- 100 # meters * 100 = centimeters
#'
#' ## units in centimeters
#' propSearchHalfMeter <- geometricRoadPadPropSearchTable(padRadius = pad * convert,
#'                                               roadWidth = road*convert,
#'                                               maxSearchDistance = maxDistance * convert,
#'                                               mastRadius = mast * convert,
#'                                               annuliWidth = 50) ##50cm = half a meter
#' head(propSearchHalfMeter, 30)
#'
#' ## convert back to meters
#' propSearchHalfMeter$distanceFromTurbine <- propSearchHalfMeter$distanceFromTurbine/convert
#' head(propSearchHalfMeter, 30)


geometricRoadPadPropSearchTable <- function(padRadius,roadWidth,maxSearchDistance,
                                            mastRadius,annuliWidth=1,...){
    ## for testing
    ## padRadius <- 10.1
    ## roadWidth <- 18.4
    ## maxSearchDistance <- 200
    ## annuliWidth <- 3
    ## mastRadius <- 2


    ## Check arguments.
    theseArgs <- list(padRadius=padRadius,roadWidth=roadWidth,maxSearchDistance=maxSearchDistance,
                      annuliWidth=annuliWidth,mastRadius=mastRadius)

    for(j in seq_along(theseArgs)){
        thisArg <- theseArgs[[j]]
        if(length(thisArg)!=1 || !is.numeric(thisArg) || ifelse(names(theseArgs)[j]=='mastRadius',any(thisArg<0),any(thisArg<=0))){

            stop('Argument ', names(theseArgs)[j], ' needs to be a single integer value greater than zero.')

        }#end if
    }#end for j


    # Set variables.
    (pR <- round(padRadius))
    (rW <- round(roadWidth))
    (maxSearchDist <- ceiling(maxSearchDistance))
    (aW <- round(annuliWidth))
    (mR <- round(mastRadius))
    (halfRoadWidth <- rW/2)

    if(rW<1){
        warning('The roadWidth argument has been rounded to zero.')
    }#end if

    if(aW<1){
        stop('The annuliWidth argument needs to be an integer greater than zero.')
    }#end if

    (outerRadius <- unique(c(seq(mR,maxSearchDist+mR,by=aW),maxSearchDist+mR)))
    outerRadius <- sort(outerRadius)


    ## the area of the intersection of a rectangle of width 2d and the annulus
    ##   with outer radius r_2 and inner radius r_1. The rectangle's long edge is
    ##   assumed to be parallel with the x-axis and the rectangle is split in half
    ##   by the x-axis.
    ##
    ## ## this is the double integral to calculate the area:
    ## sectorArea = 2\int_{0}^{d}\int_{\sqrt{r_{1}^{2} - y^{2}}}^{\sqrt{r_{2}^{2} - y^{2}}} 1 dxdy
    ## = 2(\int_{0}^d \sqrt{r_{2}^{2} - y^{2}} dy - \int_{0}^d \sqrt{r_{1}^{2} - y^{2}} dy)
    ## ## This integral is
    ## \int_{0}^d \sqrt{r^{2} - y^{2}} dy = 1/2(y\sqrt{r^2-y^2} + r^2tan^{-1}{y/sqrt{r^2 - y^2}}) |_{0}^{d}
    ## = 1/2(d\sqrt{r^2 - d^2} + r^2tan^{-1}{d/\sqrt{r^2 - d^2}}) - 1/2(0 + 0)
    ## ## this is the circleBoxInt function




    sectorArea <- c()
    for(i in 2:length(outerRadius)){
        sectorArea[i-1] <- 2*(circleBoxInt(b2=halfRoadWidth,r=outerRadius[i])-circleBoxInt(b2=halfRoadWidth,r=outerRadius[i-1]))

    } # end for i


    ## put into data.frame with a distance column outerRadius
    propSearch <- data.frame(distanceFromTurbine=outerRadius[-1]-mR,proportionAreaSearched=sectorArea/(diff(outerRadius^2)*pi))

    ## all of the pad is considered searched
    propSearch$proportionAreaSearched[propSearch$distanceFromTurbine<=pR] <- 1


    return(propSearch)


} #end geometricRoadPadPropSearchTable function
