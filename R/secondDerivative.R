#' @title secondDerivative
#'
#' @description Computes numeric second derivatives (hessian) of an
#' arbitrary multidimensional function at a particular location.
#'
#' @param loc The location (a vector) where the second derivatives
#' of \code{FUN} are desired.
#'
#' @param FUN An R function to compute second derivatives for.
#' This must be a function of the form FUN <- function(x, ...){...}
#' where x is the parameters of the function (e.g., location \code{loc}).
#' \code{FUN} must return a single value (scalar), the height of the
#' surface above \code{x}, i.e., \code{FUN} evaluated at \code{x}.
#'
#' @param ... Additional agruments to \code{FUN}.
#'
#' @param eps Radii argument, see details. Default is \code{10e-7}.
#'
#'
#' @details This function uses the "5-point" numeric second derivative
#' method advocated in numerous numerical recipie texts.  During computation
#' of the second derivative, FUN will be evaluated
#' capable of evaluating at locations within a hyper-elipsoid
#' with cardinal radii \code{2*loc*(eps)^0.25}.
#'
## If those radii don't work for your function, make eps a parameter to this function and change it, possibly varying by dimension.
#'
#' A handy way to use this function is to call an optimization routine
#' like \code{nlminb} with FUN, then call this function with the
#' optimized values (solution) and FUN.  This will yeild the hessian
#' at the solution rather than the hessian at the previous step of the
#' optimization.
#'

#'
#' @export secondDerivative
#'
#' @return Matrix
#'
#' @examples
#'
#' func <- function(x){-x*x} # second derivative should be -2
#' secondDerivative(0,func)
#' secondDerivative(3,func)
#'
#' func <- function(x){3 + 5*x^2 + 2*x^3} # second derivative should be 10+12x
#' secondDerivative(0,func)
#' secondDerivative(2,func)
#'
#' func <- function(x){x[1]^2 + 5*x[2]^2} # should be rbind(c(2,0),c(0,10))
#' secondDerivative(c(1,1),func)
#' secondDerivative(c(4,9),func)




secondDerivative <- function(loc, FUN, ..., eps=1e-7){


    x <- loc

    FUN <- match.fun(FUN)
    d <- length(x)   # number of dimensions
    hess <- matrix(NA, nrow=d, ncol=d)
    ##eps <- 10e-7

    ##print(hess)

    h <- ifelse(x==0, eps^0.25, (eps^(0.25))*x )

    for(i in 1:d){

        ei <- rep(0,d)
        ei[i] <- 1
        ## compute diagonal element
        hess[i,i] <- (-FUN(x+2*h*ei, ...) + 16*FUN(x+h*ei, ...) - 30*FUN(x, ...) +
                      16*FUN(x-h*ei, ...) - FUN(x-2*h*ei, ...)) / (12*h[i]*h[i])

        if((i+1) <= d){
            for(j in (i+1):d){
                ej <- rep(0,d)
                ej[j] <- 1
                ## compute off diagonal element
                hess[i,j] <- (FUN(x+h*ei+h*ej, ...) - FUN(x+h*ei-h*ej, ...) -
                              FUN(x-h*ei+h*ej, ...) + FUN(x-h*ei-h*ej, ...)) / (4*h[i]*h[j])
                ## Assume symetric
                hess[j,i] <- hess[i,j]
            }#end for j
        }#end if
    }#end for i

    return(hess)

}#end function secondDerivative
