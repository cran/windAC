###########


#' @name getDistributionFunction
#'
#' @title Getting distribution functions
#'
#' @description Determines if the distribution functions are available. This is intended for internal use only.
#'
#' @param type Character, typically either 'r', 'q', 'p', or  'd'.
#' @param dist Character, typically something like 'norm', 'gamma', etc.
#' @param ... Currently ignored.
#'
#' @details It is determined that \code{paste0(type, dist)} is a function and returns that function.  The nature of the returned function is not verified.

#'
#' @return Function, the first function in the search path that matches the name \code{paste0(type, dist)}.
#'
#' @export getDistributionFunction
#'
#' @examples
#'
#' fun <- getDistributionFunction(type="q",dist="norm")
#'

getDistributionFunction <- function(type,dist,...){

    if(!is.character(type)){
        stop('argument type must be a character')
    }#end if

    if(!is.character(dist)){
        stop('argument dist must be a character')
    }#end if


    funName <- paste0(type, dist)
    fun <- tryCatch(
    {
        get(funName, mode = "function")
    },
    error=function(cond){
        return(NA)
    },finally={}) #end tryCatch

    if(!is.function(fun)){
        stop('The function ', funName,' is not available.\n',
        'Ensure the function ', funName,' is available before continuing.')
    }# end if

    return(fun)

} # end getDistributionFunction function
