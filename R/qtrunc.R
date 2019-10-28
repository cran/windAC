

#'
#' @rdname truncatedDistribution
#'
#' @return \code{qtrunc} returns a vector of quantiles.
#'
#' @export qtrunc
#'
#'
#' @examples
#'
#' ## qtrunc
#' #not truncated
#' qnorm(p=.975)
#' # truncted
#' qtrunc(p=.975,distribution='norm',tbound=c(0,1))
#'
#' ## upper tail
#' # not truncted
#' qnorm(p=.975,lower.tail=FALSE)
#' # truncated
#' qtrunc(p=.975,distribution='norm',tbound=c(0,1),lower.tail=FALSE)


qtrunc <- function(p, distribution, tbound =c(-Inf, Inf), ..., lower.tail=TRUE,log.p=NULL){

###################################################
## argument checking
    if(!is.character(distribution)|length(distribution)!=1){
        stop('argument distribution must be a single character string')
    }#end if

    if(!is.numeric(tbound)){
        stop('argument tbound need to be numeric')
    } #end if

    if(!is.logical(lower.tail)|length(lower.tail)!=1){
        stop('Argument lower.tail must be a single logical value.')
    }#end if

    if(!is.numeric(p)){
        stop('Argument p must be numeric.')
    } #end if



###################################################

    ## account for vectors
    low <- min(tbound,na.rm=TRUE)
    high <- max(tbound,na.rm=TRUE)

    if(low==high){
        stop("argument tbound must be a vector of at least two elements that are not the same")
    }# end if


    ## non truncated functions
    pNonTrunc <- getDistributionFunction(type='p',dist=distribution) ##get(paste("p", distribution, sep = ""), mode = "function")
    qNonTrunc <- getDistributionFunction(type='q',dist=distribution) ##get(paste("q", distribution, sep = ""), mode = "function")

    (pLow <- pNonTrunc(low,...))
    (pHigh <- pNonTrunc(high,...))

    (pCheck <- c(pLow,pHigh))
    if(any(!is.finite(pCheck))| any(is.na(pCheck))){
        ## if pNonTrunc return NA, then return NA
        return(rep(NA,length(p)))
    }# end if

    ## reverse p for opposite tail
    if(!lower.tail){
        p <- 1-p
    }# end if


    ## 0<= p <=1, if not make NA
    p[p<0 | p>1] <- NA


    ## adjust p for the truncation
    (adjP <- pLow + p *(pHigh - pLow))

    (adjQ <- qNonTrunc(adjP,...))

    ## ensure quantile is inside the truncation bounds
    out <- pmin(high,pmax(low,adjQ))


    return(out)

} #end function
