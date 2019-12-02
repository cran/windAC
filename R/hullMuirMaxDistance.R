
#' @name hullMuirMaxDistance
#'
#' @title Calculate the Hull and Muir (2010) maximum distance
#'
#' @description Calculate the maximum fall distance from a turbine using the
#'   regression model from Hull and Muir (2010).
#'
#' @param hubHeight Numeric, turbine hub height.
#' @param bladeRadius Numeric, turbine blade radius.
#' @param ... Currently ignored.
#'
#' @details Using the linear regression coefficients from Hull and Muir (2010), a
#'   maximum distance is calculated. This is done for three size classes (bats,
#'   small birds (SB), and large birds (LB)) separately.
#'
#'   It is assumed that \code{hubHeight} and \code{bladeRadius} have the same units.
#'
#' Note: Hull and Muir (2010) used the range of 65 m < \code{hubHeight} < 94 m  and  33 m < \code{bladeRadius} < 55 m.
#' Anything outside of this range is extrapolation and should only be done with care.
#'
## #' @usage hullMuirMaxDistance(hubHeight,bladeRadius)
#'
#' @return data frame of maximum distance by size class, \code{hubHeight}, and
#'   \code{bladeRadius}. Distance will be in the same units as were provided for \code{hubHeight} and \code{bladeRadius}
#'
#' @export hullMuirMaxDistance
#'
#'
#' @references Hull, C. L., & Muir, S. (2010).
#'   Search areas for monitoring bird and bat carcasses at wind farms using a Monte-Carlo model.
#'   Australasian Journal of Environmental Management, 17(2), 77-87.
#'
#' @examples
#'
#' hubHeights <- rnorm(10, mean = 87.5, sd = 10)
#' bladeRadii <- rnorm(10, mean = 62.5, sd = 10)
#'
#' hullMuirMaxDistance(hubHeight = hubHeights, bladeRadius = bladeRadii)



## for testing
## hubHeight <- rep(87.5,times=10)
## bladeRadius <- rep(62.5,times=10)



hullMuirMaxDistance <- function(hubHeight,bladeRadius,...){

    ## come directly from the Hull and Muir (2010)
    ## HMcoef <- data.frame(hub=c(BAT=.672,SB=.637,LB=.581),
    ##                      rad=c(BAT=.046,SB=.097,LB=.176),
    ##                      constant=c(BAT=15.9,SB=31.6,LB=70.6))

    ## come directly from the Hull and Muir (2010)
    HMcoef <- data.frame(BAT=c(constant=15.9,hub=.672,rad=.046),
                          SB=c(constant=31.6,hub=.637,rad=.097),
                          LB=c(constant=70.6,hub=.581,rad=.176))


    # Check arguments.
    if(length(hubHeight) != length(bladeRadius)){
        stop('The length of hubHeight and bladeRadius must be the same.')
    }# end if

    ## user inputs
    input <- c(hubHeight,bladeRadius)

    if(!is.numeric(input)|any(input<0)){
        stop('hubHeight and bladeRadius must each be a nonnegative values.')
    } # end if


    ## here is the max distance
    maxDist <- as.data.frame(cbind(1,hubHeight,bladeRadius)%*%as.matrix(HMcoef))

    maxDist[,'hubHeight'] <- hubHeight
    maxDist[,'bladeRadius'] <- bladeRadius


    return(maxDist)

}# end hullMuirMaxDistance function
