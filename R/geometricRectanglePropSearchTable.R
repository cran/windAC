
#' @name geometricRectanglePropSearchTable
#'
#' @title Create proportion of area searched table for a rectangular full plot
#'
#' @description Calculate area of annulus bisected by 2 parallel lines to get the
#'   area of the rectangle that is searched
#'
#' @param side1 Numeric, length of the side of the rectangle
#' @param side2 Numeric, length of the second side of the rectangle, default is
#'   \code{side1} which produces a square.
#' @param mastRadius Integer, radius of the turbine mast.
#' @param annuliWidth Integer, width of annuli, default is 1
#' @param ... Currently ignored.
#'
#' @details Searches are conducted around a turbine within a rectangle for bird
#'   and bat carcasses. This function creates a data frame of proportion of area
#'   searched within each annulus ring. The turbine is assumed to be centered
#'   within the rectangle.
#'
#' @return Data frame of proportion of area searched for each annulus. \code{distanceFromTurbine} column represents the outer radius of the annuli.
#'
#' @export geometricRectanglePropSearchTable
#'
#' @seealso geometricRoadPadPropSearchTable
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


geometricRectanglePropSearchTable <- function(side1,side2=side1,mastRadius,annuliWidth=1,...){
    ## for testing
    ## side1 <- 50
    ## side2 <- 70
    ## mastRadius <- 2
    ## annuliWidth <- 1

    # Check arguments.
    theseArgs <- list(side1=side1,side2=side2,annuliWidth=annuliWidth,mastRadius=mastRadius)

    for(j in seq_along(theseArgs)){
        thisArg <- theseArgs[[j]]
        if(length(thisArg)!=1 || !is.numeric(thisArg) || ifelse(names(theseArgs)[j]=='mastRadius',any(thisArg<0),any(thisArg<=0))){

            stop('Argument ', names(theseArgs)[j], ' needs to be a single numeric value greater than zero')

        }#end if
    }#end for j


    # Set variables.
    (s1 <- (side1))
    (s2 <- (side2))

    (maxSearchDist <- sqrt((s1/2)^2 + (s2/2)^2))

    (aW <- (annuliWidth))
    (mR <- (mastRadius))
    (halfShortSide <- min(s1,s2)/2)
    (outerRadius <- unique(c(seq(mR,floor(maxSearchDist)+mR,by=aW),floor(maxSearchDist)+mR)))
    outerRadius <- sort(outerRadius)


    ## the area of the intersection of a rectangle of width 2d and the annulus
    ##   with outer radius r_2 and inner radius r_1. The rectangle's long edge is
    ##   assumed to be parallel with the x-axis and the rectangle is split in half
    ##   by the x-axis.
    ##
    ## this is the double integral to calculate the area:
    ## sectorArea = 4\int_{0}^{d}\int_{\sqrt{r_{1}^{2} - y^{2}}}^{\sqrt{r_{2}^{2} - y^{2}}} 1 dxdy
    ## = 4(\int_{0}^d \sqrt{r_{2}^{2} - y^{2}} dy - \int_{0}^d \sqrt{r_{1}^{2} - y^{2}} dy)
    ## ## This integral is
    ## \int_{0}^d \sqrt{r^{2} - y^{2}} dy = 1/2(y\sqrt{r^2-y^2} + r^2tan^{-1}{y/sqrt{r^2 - y^2}}) |_{0}^{d}
    ## = 1/2(d\sqrt{r^2 - d^2} + r^2tan^{-1}{d/\sqrt{r^2 - d^2}}) - 1/2(0 + 0)
    ## ## this is the circleBoxInt function



    ## Using calculus this can be reduced to the formula assigned to sectorArea

    sectorArea <- c()
    for(i in 2:length(outerRadius)){
        sectorArea[i-1] <- 4*(circleBoxInt(b2=halfShortSide,r=outerRadius[i])-circleBoxInt(b2=halfShortSide,r=outerRadius[i-1]))

    } # end for i


    ## put into data.frame with a distance column outerRadius
    propSearch <- data.frame(distanceFromTurbine=outerRadius[-1]-mR,proportionAreaSearch=sectorArea/(diff(outerRadius^2)*pi))


    return(propSearch)


} #end geometricRectanglePropSearchTable function
