
#' @name geometricRectanglePropSearchTable
#'
#' @title Create proportion of area searched table for a rectangular full plot
#'
#' @description Calculate the areas of intersection of a series of nested annuli with a rectangle.
#'
#'
#'
#' @param side1 Numeric, length of the side of the rectangle.
#' @param side2 Numeric, length of the second side of the rectangle, default is
#'   \code{side1} which produces a square.
#' @param mastRadius Integer, radius of the turbine mast.
#' @param annulusWidth Integer, width of annulus, default is 1.
#' @param ... Currently ignored.
#'
#' @details Searches are conducted around a turbine within a rectangle for bird
#'   and bat carcasses. This function creates a data frame of proportion of area
#'   searched within each annulus ring. The turbine is assumed to be centered
#'   within the rectangle.
#'
#' @return Data frame of proportion of area searched for each annulus. \code{distanceFromTurbine} column represents the outer radius of each annulus.
#'
#' @export geometricRectanglePropSearchTable
#'
#' @seealso geometricRoadPadPropSearchTable circleBoxInt
#'
#' @examples
#'
#' ## square 50 x 50
#' propSearch <- geometricRectanglePropSearchTable(side1 = 50,
#'                                                 mastRadius = 2)
#'
#'
#' ## square 50 x 70
#' propSearch <- geometricRectanglePropSearchTable(side1 = 50,
#'                                                 side2 = 70,
#'                                                 mastRadius = 2)
#'
#'
#'


geometricRectanglePropSearchTable <- function(side1,side2=side1,mastRadius,annulusWidth=1,...){
    ## for testing
    ## side1 <- 50
    ## side2 <- 70
    ## mastRadius <- 2
    ## annuliWidth <- 1

    # Check arguments.
    theseArgs <- list(side1=side1,side2=side2,annulusWidth=annulusWidth,mastRadius=mastRadius)

    for(j in seq_along(theseArgs)){
        thisArg <- theseArgs[[j]]
        if(length(thisArg)!=1 || !is.numeric(thisArg) || ifelse(names(theseArgs)[j]=='mastRadius',any(thisArg<0),any(thisArg<=0))){

            stop('Argument ', names(theseArgs)[j], ' needs to be a single numeric value greater than zero')

        }#end if
    }#end for j


    # Set variables.
    (s1 <- (side1))
    (s2 <- (side2))

    (maxSearchDist <- ceiling(sqrt((s1/2)^2 + (s2/2)^2)))

    (aW <- (annulusWidth))
    (mR <- (mastRadius))
    (halfShortSide <- min(s1,s2)/2)
    (halfLongSide <- max(s1,s2)/2)
    (outerRadius <- unique(c(seq(mR,maxSearchDist+mR,by=aW),maxSearchDist+mR)))
    outerRadius <- sort(outerRadius)


    ## Using calculus this can be reduced to the formula assigned to sectorArea

    sectorArea <- c()
    for(i in 2:length(outerRadius)){

        sectorArea[i-1] <- 4*(circleBoxInt(R=outerRadius[i],S=halfShortSide,L=halfLongSide)-circleBoxInt(R=outerRadius[i-1],S=halfShortSide,L=halfLongSide))

    } # end for i


    ## put into data.frame with a distance column outerRadius
    propSearch <- data.frame(distanceFromTurbine=outerRadius[-1]-mR,areaSearched=sectorArea,annulusArea=(diff(outerRadius^2)*pi),proportionAreaSearched=sectorArea/(diff(outerRadius^2)*pi))
    return(propSearch)


} #end geometricRectanglePropSearchTable function
