#' @name LogLogistic
#' @aliases pllog
#' @aliases dllog
#' @aliases pllog
#' @aliases rllog
#'
#' @title Log-Logistic Distribution
#'
#' @description The probability density function, cumulative density function, inverse cumulative density function, random generation for the log logistic distribution.
#'
#' @param x Vector of quantiles.
#' @param q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param shape Shape parameter.
#' @param scale Scale parameter.
#' @param log Logical; if TRUE, log densities are returned.
#' @param lower.tail Logical; if TRUE (default), probabilities are P(X <= x) otherwise, P(X > x).
#' @param log.p Logical; if TRUE, probabilities p are given as log(p).
#' @param ... Currently ignored.
#'
#'
#' @details
#' If X is a random variable distributed according to a logistic distribution, then Y = exp(X) has a log-logistic distribution.
#'
#' The log-logistic distribution with parameters \code{shape = a} and \code{scale = s} has density
#' \deqn{f(x) = \frac{(\frac{1}{a*exp(s))})(\frac{x}{\exp{s}})^{\frac{1}{a} - 1}}{(1+(\frac{x}{\exp{s}})^{1/a})^2}}
#' for \code{x >= 0}, \code{a > 1}, and \code{s > 0}.
#'
#' The median is \code{exp(s)}, mean is
#' \deqn{\frac{a\pi*exp(s)}{sin(a*\pi)}}
#' for \code{1/a > 1}. The variance is
#' \deqn{(exp(s))^2(\frac{2*\pi*a}{(sin(2*pi*a))}- \frac{(a*\pi)^2}{(sin^2(a*\pi))})}
#' for \code{1/a > 2}. The mode is
#' \deqn{exp(s)(\frac{(1/a) - 1}{(1/a) + 1})^{a}}
#' for \code{1/a > 1} otherwise it is zero.
#'
#' @return \code{dllog} returns vector of the densities.
#'
#' @seealso \code{\link[stats]{dlogis}} \code{\link[stats]{plogis}} \code{\link[stats]{qlogis}} \code{\link[stats]{rlogis}}
#'
#' @export dllog
#'
#' @examples
#'
#' y <- rllog(5,shape=1,scale=1/3)
#' dllog(x=y,shape=1,scale=1/3)
#' dlogis(x=log(y),location=1/3,scale=1)/y
#'
#' pllog(q=y,shape=1,scale=1/3)
#' qllog(p=seq(0,1,by=.25),shape=1,scale=1/3)
#'
#'

dllog <- function(x, shape = 1, scale = 1, log = FALSE,...){


    logX <- suppressWarnings(log(x))

    llx <- stats::dlogis(logX, location = scale, scale = shape, log = FALSE)/x

    ## support is 0 to Inf
    llx[x<=0] <- 0

    if (log){
        out <- log(llx)
    }else{
        out <- llx
    } # end else

    return(out)

} ## dllog function



