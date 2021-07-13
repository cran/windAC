
#' @name triangleProb
#'
#' @title Calculate probabilities from a triangle distribution based on Hull and
#'   Muir (2010) maximum distance as proposed by Huso and Dalthorp (2014).
#'
#' @description Calculate the probabilities between one-unit increments of a right
#'   triangle distribution.
#'
#' @param hubHeight Numeric, turbine hub height.
#' @param bladeRadius Numeric, turbine blade radius.
#' @param lowerBound Numeric, default is zero, see Details.
#' @param upperBound Numeric, default is \code{Inf}, see Details.
#' @param ... Currently ignored.
#'
#' @details A right triangle is constructed with the 90 degree corner at the
#'   origin in the first quadrant of the cartesian plane. The \code{lowerBound}
#'   will move the left edge of the triangle to the right. The \code{upperBound} will truncate the
#'  triangle distribution at that value.
#'
#' The maximum horizontal distance is calculated using
#' \code{\link{hullMuirMaxDistance}}. This is typically not a
#'   whole number and the \code{\link{ceiling}} is used. The maximum
#'   vertical distance is such that the area under the hypotenuse edge of triangle
#'   integrates to one. This is done using the equation for the area of a triangle.
#'
#'   The two points that make up the hypotenuse are used to calculate the slope
#'   and intercept of the line. The area under the line in one-unit increments is
#'   calculated using
#' \deqn{\int_{x-1}^{x}mZ+b dZ = m(x-.5)+b}
#' where \code{m} is
#'   the slope, \code{b} is the intercept, and \code{x} is a distance. Integrating
#'   between \code{x-1} and \code{x} gives the probability between the one-unit
#'   increments.
#'
#'   All of this is done for three size classes (bats, small birds (SB), and large
#'   birds (LB)) separately. An additional size class (RAPTOR) is included and
#'   identical to the large bird result.
#'
#'   The \code{\link{floor}} function is applied to \code{lowerBound}.
#'
#'   It is assumed that \code{hubHeight} and \code{bladeRadius} have the same units.
#'
#'
#' @return List of two data frames: the first has distances in one-unit increments
#'   (the outer distance), the probabilities between the distances, and a column
#'   indicating size class; the second gives the maximum distance of each size class.
#'
#' @export triangleProb
#'
#' @references Hull, C. L., & Muir, S. (2010).
#'   Search areas for monitoring bird and bat carcasses at wind farms using a Monte-Carlo model.
#'   Australasian Journal of Environmental Management, 17(2), 77-87.
#'
#' @references Huso, M. & Dalthorp,D (2014).
#' Accounting for Unsearched Areas in Estimating Wind Turbine-Caused Fatality.
#' The Journal of Wildlife Management. 78. 10.1002/jwmg.663.
#'
#' @seealso \code{\link{hullMuirMaxDistance}}
#'
#' @examples
#'
#' triResult <- triangleProb(hubHeight = 100, bladeRadius = 50, lowerBound = 0)
#' names(triResult) ## list names
#' triResult$maxDist ## max distance for each size class
#' head(triResult$triDistProb)




triangleProb <- function(hubHeight,bladeRadius,lowerBound=0,upperBound=Inf,...){


    ## for testing
    ##hubHeight <- 87.5;bladeRadius <- 62.5;lowerBound<-0;upperBound <- 50


    ## user inputs
    input <- c(hubHeight,bladeRadius,lowerBound,upperBound)

    if(!is.numeric(input)||length(input)!=4||any(input<0)){
        stop('hubHeight, bladeRadius, lowerBound, and upperBound must each be a single number that is nonnegative.')
    } # end if


    ## here is the max distance
    maxDistance <- hullMuirMaxDistance(hubHeight,bladeRadius)
    maxDist <- as.data.frame(ceiling(t(maxDistance[,c('BAT','SB','LB')])))
    names(maxDist) <- 'maxDist'


    ## put into new data frame for slope/intercept calculations
    (triPoints <- maxDist)
    names(triPoints) <- 'x2' ## which is the max distance
    triPoints$size <- row.names(triPoints) ## make size a variable
    triPoints$x1 <- floor(lowerBound) ## left edge of the triangle

    triPoints$y1 <- with(triPoints,2/(x2-x1)) ## height of the triangle on the left
    triPoints$y2 <- 0 ## right corner of the triangle is on the x-axis

    ## calculate the slope
    triPoints$slope <- with(triPoints,(y2-y1)/(x2-x1))
    ## calculate the y-intercept
    triPoints$intcp <- with(triPoints,y2-slope*x2)


    ## include a row for raptors that is the same as LB
    raptRow <- subset(triPoints,triPoints$size=='LB')
    raptRow$size <- 'RAPTOR'
    triPoints <- rbind(triPoints,raptRow)


    ## Calculate probabilities.

    ## for debugging
    ##dat <- subset(triPoints,size=='BAT')

    getProb <- function(dat,upperBound){
        dist <- with(dat,(x1+1):x2) ## one unit increments
        ## The probabilities between the increments are given by
        ##\int_{x-1}^{x}mZ+b dZ = m(x-.5)+b
        ## m = slope
        ## b = intercept
        ## x = distance from turbine
        prop <- with(dat,slope*(dist-.5)+intcp) # probabilities between increments
        out <- data.frame(dist,prop)
        names(out) <- c('distanceFromTurbine','probability')

        ## include truncated probabilities as well
        out$truncProb <- out$probability
        out$truncProb[out$distanceFromTurbine>upperBound] <- 0
        out$truncProb <- out$truncProb/sum(out$truncProb)

        return(out)
    } # end getProb function

    ## for debugging
    ##print(triPoints)

    ##triDistProb <- plyr::ddply(triPoints,~size,getProb)
    triDistProb <- NULL
    for(s in sort(triPoints$size)){

        sizeProb <- getProb(dat=subset(triPoints,triPoints$size==s),upperBound=upperBound)
        sizeProb$size <- s
        triDistProb <- rbind(triDistProb,sizeProb[,c('size','distanceFromTurbine','probability','truncProb')])

    }# end for s


    return(list(triDistProb=triDistProb,maxDist=maxDist))

} # end triangleProb
