#########################################
## Jared Studyvin
## 8 Dec 2016
## pllog - documentation is in dllog
#########################################



#'
#' @rdname LogLogistic
#'
#'
#' @return \code{pllog} returns a vector of probabilities.
#'
#'
#' @export pllog
#'
#'
#'


pllog <- function(q, shape = 1, scale = 1, lower.tail = TRUE, log.p = FALSE,...){

################################################
### argument checking

    if(!is.logical(lower.tail)|length(lower.tail)!=1){
        stop('Argument lower.tail must be a single logical value.')
    }#end if


    if(!is.logical(log.p)|length(log.p)!=1){
        stop('Argument log.p must be a single logical value.')
    }#end if


    if(!is.numeric(q)){
        stop('Argument q must be numeric.')
    } #end if

    if(!is.numeric(shape)){
        stop('Argument shape must be numeric.')
    } #end if

    if(!is.numeric(scale)){
        stop('Argument scale must be numeric.')
    } #end if


#################################################

    ##support is 0 to Inf
    q[q<0] <- 0

    out <- stats::plogis(log(q), location = scale, scale = shape)

    ## convert probability
    if(!lower.tail){
        out <- 1 - out
    } ## end if

    ## log probability
    if(log.p){
        out <- log(out)
    } ## end if

    return(out)

} ## end pllog function




