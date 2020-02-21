#'
#'
#' @name getDistributionSummary
#'
#' @title Summary statistics from the fitted distribution
#'
#' @description Summary statistics are calculated for the distribution with parameter estimates. Right now only the median is produced.
#'
#'
#'
#' @param distribution  String indicating which distribution to use.
#' @param paramVec Numeric vector for the parameters associated with distribution. Assumed to be in the same order as the function indicated by \code{distribution}.
#' @param truncBounds Numeric, indicating bounds for the area correction calculation, see details. Default is NULL, and the bounds are set to \code{c(0,Inf)}.
#' @param ... Additional arguments to \code{\link[stats]{integrate}}.
#'
#'
#' @details
#'   The \code{truncBounds} argument defaults to zero as a lower bound and infinity
#'   as the upper bound. If a single value is provided, it is assumed as the upper
#'   bound with zero as the lower bound. If two or more values are provided, the
#'   \code{max(truncBounds)} is the upper bound and \code{min(truncBounds)} is the
#'   lower bound.
#'
#'
#' @return Data frame with the summary statistics.
#'
#' @export
#'
#' @examples
#'
#' getDistributionSummary('norm',c(40,25),truncBounds=c(-Inf,Inf))
#'
#' getDistributionSummary('norm',c(40,25),truncBounds=NULL)
#'
#' getDistributionSummary('norm',c(40,25),truncBounds=c(0,30))
#'

## a <- 0
## b <- 30
## mu <- 40
## s <- 25

## qnorm((pnorm((a-mu)/s)+pnorm((b-mu)/s))/2)*s + mu


## distribution <- 'norm'
## paramVec <- c(100,10)
## truncBounds <- c(10,190)



getDistributionSummary <- function(distribution,paramVec,truncBounds=NULL,...){

    ## Check distribution.
    if(missing(distribution) || length(distribution)!=1){
        stop('argument distribution must be a single character string')
    }#end if

    ## for internal use
    distn <- tolower(distribution)


    ## Check parameters
    if(missing(paramVec) || !is.numeric(paramVec) || any(is.na(paramVec))){
        stop('argument paramVec needs to be numeric')
    } #end if

    ## for internal use
    param1 <- paramVec[1]
    param2 <- paramVec[2]

    ## Check bounds.
    if(!is.null(truncBounds)&&!is.numeric(truncBounds)){
        stop('argument truncBounds needs to be numeric')
    } #end if
    if(is.null(truncBounds)){
        tUp <- Inf
        tLow <- 0
    }else{
        tUp <- max(truncBounds,na.rm=TRUE)
        tLow <- min(truncBounds,na.rm=TRUE)
        if(length(truncBounds)==1){
            tLow <- 0
        }# end if
        if(length(truncBounds)>2){
            warning(paste('The smallest value and largest value of truncBounds',
                          'will be used as the bounds of the truncation.'),
                    immediate. = TRUE)
        }# end if
    } # end if else




    ## get the distribution functions

    pDist <- getDistributionFunction(type = "p", dist = distn)
    qDist <- getDistributionFunction(type = "q", dist = distn)
    dDist <- getDistributionFunction(type = "d", dist = distn)

    ## E[X] function
    expFun <- function(x,dDist,param1,param2,...){
        if(is.na(param2)){
            return(x*dDist(x,param1))
        }#end if

        return(x*dDist(x,param1,param2))
    }#end expFun




    ## get the median
    ## https://en.wikipedia.org/wiki/Truncated_distribution
    if(is.na(param2)){
        (cdfLow <- pDist(tLow,param1))
        (cdfUp <- pDist(tUp,param1))
        thisMedian <- (qDist((cdfLow+cdfUp)/2,param1))
    }else{
        (cdfLow <- pDist(tLow,param1,param2))
        (cdfUp <- pDist(tUp,param1,param2))
        thisMedian <- (qDist((cdfLow+cdfUp)/2,param1,param2))

    }# end if else



    thisMean <- stats::integrate(f=expFun,lower=tLow,upper=tUp,dDist=dDist,param1=param1,param2=param2,...)$value/(cdfUp - cdfLow)


    out <- data.frame(distribution=distn,param1=param1,param2=param2,mean=thisMean,median=thisMedian)

    return(out)


}#end getDistributionSummary function


