#' @name getDistanceProbability
#'
#' @title Calculate probabilities within one unit increments.
#'
#' @description Probabilities are calculated between specified increments for a given distribution and parameter values.
#'
#' @param q Vector of quantiles.
#' @param distribution Character value; specifying the desired probability distribution, see \code{\link{ptrunc}}.
#' @param param1 Numeric; value of the first parameter of the specified distribution.
#' @param param2 Numeric; default is \code{NA}. Value of the second parameter of the specified distribution, if applicable.
#' @param tbound Numeric vector specifying the lower and upper truncation bounds. Default is \code{c(-Inf, Inf)}.
#' @param unitSize Numeric; either of length one or equal to \code{length(q)}, specific the desired width for the probability calculation. Default is 1.
#' @param ... Currently ignored.
#'
#'
#' @details
#' This is a wrapper function that uses the \code{\link{ptrunc}} function. The basic calculation is \code{ptrunc(q,...) - ptrunc(q-abs(unitSize),...)}
#'
#'
#' @return Vector of probabilities
#'
#' @export getDistanceProbability
#'
#'
#' @seealso ptrunc
#'
#' @examples
#' ## normal distribution
#' getDistanceProbability(q=8,distribution='norm',param1=10,param2=1)
#' pnorm(8,mean=10,sd=1)-pnorm(8-1,mean=10,sd=1)
#'
#' ## larger unitSize
#' getDistanceProbability(q=12,distribution='norm',param1=10,param2=1,unitSize=4)
#' pnorm(12,mean=10,sd=1)-pnorm(12-4,mean=10,sd=1)
#'


getDistanceProbability <- function(q,distribution,param1,param2=NA,tbound=c(-Inf,Inf),unitSize=1,...){



    if(!is.numeric(unitSize)){
        stop('argument unitSize must be numeric')
    }#end if


    if(!(length(unitSize)==1 || length(unitSize)==length(q))){
        stop('argument unitSize must either have length one or length equal to length(q)')
    }#end if


    truncBound <- tbound

    ## get the probabilities
    if(is.na(param2)){
        prob2 <- ptrunc(q=q,distribution=distribution,tbound=truncBound,param1,lower.tail=TRUE,log.p=NULL)
        prob1 <- ptrunc(q=q-abs(unitSize),distribution=distribution,tbound=truncBound,param1,lower.tail=TRUE,log.p=NULL)
    }else{
        prob2 <- ptrunc(q=q,distribution=distribution,tbound=truncBound,param1,param2,lower.tail=TRUE,log.p=NULL)
        prob1 <- ptrunc(q=q-abs(unitSize),distribution=distribution,tbound=truncBound,param1,param2,lower.tail=TRUE,log.p=NULL)
    }# end if else

    ## probability between the two values
    out <- prob2-prob1

    return(out)
} # end function getDistanceProbability
