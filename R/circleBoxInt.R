
#' @name circleBoxInt
#'
#' @title Integration of the intersection of a rectangle and an annulus
#'
#' @description Calculate area of annulus bisected by 2 parallel lines to get the
#'   area of the rectangle that is searched
#'
#' @param b2 Numeric, upper integral bound.
#' @param r Numeric, circle radius.
#' @param b1 Numeric, lower integral bound, default is zero.
#' @param ... Currently ignored.
#' @details
#' The integral of a box bounded by the y-axis on the left, \code{b1} or x-axis on the bottom (which every is larger), \code{b2} on the top, and by a circle with radius \code{r} on the right.
#' \deqn{\int_{b1}^{b2} \sqrt{r^{2} - y^{2}} dy = 1/2(y\sqrt{r^2-y^2} + r^2tan^{-1}(y/\sqrt{r^2 - y^2})) |_{b1}^{b2}}
#'
#'
#' This is an internal function and intended for use in \code{\link{geometricRectanglePropSearchTable}} and \code{\link{geometricRoadPadPropSearchTable}}
#'
#'
#'
#' @return Numeric value
#'
#' @export
#'
#'
#' @examples
#'
#' b2 <- 5
#' b1 <- 1
#' r <- 10
#' circleBoxInt(b2=b2,r=r,b1=b1)
#'
## not run
## the integral is the area inside the polygon
## x <- seq(0,r,length=100)
## plot(x,sqrt(r^2-x^2),ylim=c(b1-1,b2+1),xlim=c(0,r+1),type='l',xaxs='i',yaxs='i')
## segments(x0=0,y0=b1,x1=sqrt(r^2-b1^2),y1=b1)
## segments(x0=0,y0=b2,x1=sqrt(r^2-b2^2),y1=b2)


## ## for debugging
## b2 <- 5
## b1 <- 1
## r <- 100
## x <- seq(0,r,length=100)
## ##the integral is the area inside the polygon
## plot(x,sqrt(r^2-x^2),ylim=c(b1-1,b2+1),xlim=c(0,r+1),type='l',xaxs='i',yaxs='i')
## segments(x0=0,y0=b1,x1=sqrt(r^2-b1^2),y1=b1)
## segments(x0=0,y0=b2,x1=sqrt(r^2-b2^2),y1=b2)
## circleBoxInt(b2=b2,r=r,b1=b1)


circleBoxInt <- function(b2,r,b1=0,...){

    args <- c(b2,r,b1)
    if(!is.numeric(args) || length(args) !=3){
        stop('Arguments b2, r, and b1 each need to be a single numeric value.')
    }#end if

    ## This integral is
    ## \int_{b1}^b2 \sqrt{r^{2} - y^{2}} dy = 1/2(y\sqrt{r^2-y^2} + r^2tan^{-1}(y/\sqrt{r^2 - y^2})) |_{b1}^{b2}
    ## this is the closed form solution
    closeForm <- function(y,a){
        if(a<=y){
            out <- pi*a^2/4
        }else{
            out <- .5*(y*sqrt(a^2-y^2) + a^2 *atan(y/sqrt(a^2-y^2)))
        }
        return(out)
    }#end function closeForm

    out <- closeForm(b2,r)-closeForm(b1,r)

    return(out)
}#end circleBoxInt function
