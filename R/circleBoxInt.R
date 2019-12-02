
#' @name circleBoxInt
#'
#' @title Integration of the intersection of a rectangle and a circle
#'
#' @description Calculates the area of the intersection between a rectangle and and circle.
#'
#' @param R Numeric, circle radius.
#' @param S Numeric, short side of the rectangle
#' @param L Numeric, long side of the rectangle
#' @param ... Currently ignored.
#'
#' @details
#' The rectangle is defined with lower left corner being the origin and upper right corner at (L, S). The area returned is the intersection between the circle, centered at the origin, and the rectangle.
#'
#' If \eqn{R \leq S} then \eqn{(\pi R^2)/4} is returned.
#'
#' If \eqn{R \geq \sqrt{S^2 + L^2}} then \eqn{L*S} is returned.
#'
#' If \eqn{R \leq L} then \eqn{R^2*sin^{-1}(S/R)/2 + S*\sqrt(R^2-S^2)/2}
#' This is the area of a circle in the first quadrant between the horizontial line \eqn{y=S}
#'
#' if \eqn{R > L} and \eqn{R < \sqrt{S^2 + L^2}} then
#' \deqn{(R^2*sin^{-1}(S/R)/2 + S*\sqrt(R^2-S^2)/2) - (R^2*sin^{-1}(B/R)/2 + S*\sqrt(R^2-B^2)/2) + B*L}
#' where \eqn{B = \sqrt{R^2 - L^2}}. In this case the there is part of the circle to the right of the rectangle. First set of parenthesis is the area of the circle below \code{S}, the second set is the area below \code{B}. Substracting the two gives the area between \code{B} and \code{S}. The rectangle defined by \code{B} and \code{L} needs to be added back in.
#'
#'
#' @return Numeric value
#'
#' @export
#'
#'
#' @examples
#'
#' radius <- 115
#' short <- 80
#' long <- 100
#' circleBoxInt(R=radius,S=short,L=long)
#'
#' ## not run
#' ## the integral is the area inside the polygon
#' \donttest{
#'  x <- seq(0,max(radius,long),length=100)
#' outlineY <- function(x,R,S,L){
#'     suppressWarnings(y <- sqrt(R^2-x^2))
#'    y[x>R] <- 0
#'    y[x>L] <- 0
#'    y[y>=S] <- S
#'    return(y)
#' }
#' y <- outlineY(x=x,R=radius,S=short,L=long)
#' plot(x,y,type='l',ylim=c(-10,short))
#' text(long,0,label='L',pos=1)
#' text(0,short,label='S',pos=1)
#' text(long,sqrt(radius^2-long^2),label='B',pos=4)
#' }




circleBoxInt <- function(R,S,L,...){


     args <- c(R,S,L)
     if(!is.numeric(args) || length(args) !=3 || any(args<0)){
        stop('Arguments R, S, and L each need to be a single numeric value greater than zero.')
     }#end if

    if(any(args==0)){
        return(0)
    }#end if


    if(S>L){
        stop('S must be less or equal to L')
    }#end if



    if(R<=S){
        ## If the circle is completely inside the rectangle then the area is just that of the circle
        (A <- pi*(R^2)/4) ## note: only the area for the first quadrant
        return(A)
    }#end if

    farCorner <- sqrt(S^2+L^2)


    if(R>=farCorner){
        ## if the rectangle is completly inside the circle then the area is just the rectangle
        A <- L*S
        return(A)
    }#end if


    circInt <- function(a,b){
        ##a= radius
        ##b = height on y axis

        return(a^2*asin(b/a)/2 + b*sqrt(a^2-b^2)/2)
    }#end circInt


    if(R<=L){
        return(circInt(a=R,b=S))
    }#end if


    lowY <- sqrt(R^2 - L^2)

    ##
    A <- circInt(a=R,b=S) - circInt(a=R,b=lowY) + lowY*L

    return(A)



}#end circleBoxInt

