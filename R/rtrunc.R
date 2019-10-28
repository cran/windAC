

#'
#' @rdname truncatedDistribution
#'
#'
#' @details The random variates are produced using the direct method (see Casella and Berger 2002).
#'
#'
#' @return \code{rtrunc} returns a vector of random variates.
#'
#' @export rtrunc
#'
#' @references
#' G. Casella and R. L. Berger. Statistical inference. Vol. 2. Duxbury Pacific Grove, CA, 2002.
#'
#' @examples
#'
#' ## rtrunc
#' rtrunc(n=5, distribution = 'gamma', tbound=c(2,5),shape=3,rate=2)


rtrunc <- function (n, distribution, tbound = c(-Inf,Inf), ...){

################################################
### argument checking
    if(!is.character(distribution)|length(distribution)!=1){
        stop('argument distribution must be a single character')
    }

    if(!is.numeric(tbound)){
        stop('argument tbound need to be numeric')
    } #end if


    if(!is.numeric(n)){
        stop('Argument n must be numeric.')
    } #end if


################################################

    ## account for vectors
    low <- min(tbound,na.rm=TRUE)
    high <- max(tbound,na.rm=TRUE)

    if (low >= high){
        stop("argument lowBound is greater than or equal to highBound")
    }# end if

    randUnif <- stats::runif(n, min = 0, max = 1)
    out <- qtrunc(p=randUnif, distribution=distribution, tbound=c(low, high), ...)


    return(out)
} #end function
