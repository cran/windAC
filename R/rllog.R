###########################################
## Jared Studyvin
## 8 Dec 2016
## rllog
###########################################


#'
#' @rdname LogLogistic
#'
#' @return \code{rllog} returns a vector of random log-logistic variates.
#' @export rllog
#'



rllog <- function(n, shape = 1, scale = 1,...){

################################################
### argument checking

    if(!is.numeric(n)){
        stop('Argument n must be numeric.')
    } #end if

    if(!is.numeric(shape)){
        stop('Argument shape must be numeric.')
    } #end if

    if(!is.numeric(scale)){
        stop('Argument scale must be numeric.')
    } #end if


#################################################


    out <- exp(stats::rlogis(n, location = scale, scale = shape))

    return(out)
} ## end rllog function
