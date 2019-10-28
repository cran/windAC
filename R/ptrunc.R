

#'
#' @rdname truncatedDistribution
#'
#' @return \code{ptrunc} returns a vector of probabilities.
#'
#' @export ptrunc
#'
#'
#' @examples
#'
#' ## ptrunc
#' #not truncated
#' pgamma(2,shape=3,rate=2)
#' # truncated
#' ptrunc(2, distribution = 'gamma', tbound=c(1,5),shape=3,rate=2)
#'
#' ## upper tail
#' # not truncated
#' pgamma(2,shape=3,rate=2,lower.tail=FALSE)
#' # truncated
#' ptrunc(2,distribution='gamma',tbound=c(1,5),shape=3,rate=2,lower.tail=FALSE)



ptrunc <- function(q, distribution, tbound = c(-Inf, Inf),...,lower.tail=TRUE,log.p=NULL){

    ## for testing
    ##q <- c(2);distribution <- 'gamma';tbound <- c(1,5)
    ##q <- 100;distribution <- 'norm';tbound <- c(0,100);xbar <- -355;stdev <- 42


##############################################################
    ## argument checking

    if(!is.logical(lower.tail)||length(lower.tail)!=1){
        stop('Argument lower.tail must be a single logical value.')
    }#

    if(!is.numeric(q)){
        stop('Argument q must be numeric.')
    } #end if


    if(!is.character(distribution)||length(distribution)!=1){
        stop('Argument distribution must be a single character string.')
    }

    if(!is.numeric(tbound)){
        stop('Argument tbound must be numeric.')
    } #end if

############################################################

    ## truncation bounds
    (low <- min(tbound,na.rm=TRUE))
    (high <- max(tbound,na.rm=TRUE))


    if (low == high){
        stop("Argument tbound must be a vector of at least two elements that are not the same")
    }# end if

    pNonTrunc <- getDistributionFunction(type='p',dist=distribution)##get(paste("p", distribution, sep = ""), mode = "function")
    qNonTrunc <- getDistributionFunction(type='q',dist=distribution)##get(paste("q", distribution, sep = ""), mode = "function")

    ## for testing
    ##(pLow <- pnorm(low,mean=xbar,sd=stdev,lower.tail=FALSE))
    ## (pHigh <- pnorm(high,mean=xbar,sd=stdev,lower.tail=FALSE))
    ##pHigh <- pNonTrunc(high,mean=mean,sd=sd,lower.tail=FALSE)

    (pLow <- pNonTrunc(low,...))
    (pHigh <- pNonTrunc(high,...))

    (pCheck <- c(pLow,pHigh))
    if(any(!is.finite(pCheck))|| any(is.na(pCheck))){
        ## if pNonTrunc return NA, then return NA
        return(rep(NA,length(q)))
    }# end if


    ## if q > high then q = high
    ## if q < low then q= low
    ## for each element of q
    (qAdj <- pmax(pmin(q,high),low))


    ## if density is flat across truncation bounds
    ## return one or zero
    if(pHigh-pLow==0){
        warning('The probability density function within that truncation interval is practically flat. Returning either zeros or ones.')
        out <- rep(pLow,length=length(q))

    }else{
    ##
        out <- (pNonTrunc(qAdj,...) - pLow)/(pHigh-pLow)
    }# end if else


    if(!lower.tail){
        out <- 1-out
    }# end if


    return(out)

} # end function
