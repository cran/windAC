########################################################
## Jared Studyvin
## 14 June 2016
## get starting values for the weighted likelihood or weighted distribution
########################################################
#'
#'
#' @name getStartValue
#'
#' @title Calculate the start values for the to be passed to the optimizer.
#'
#' @description Calculate start values for \code{\link{weightedLikelihood}} or \code{\link{weightedDistribution}}.
#'
#'
#' @param x Numeric vector of the data observations.
#' @param distribution  String indicating which distribution to use.
#' @param w Numeric Vector of weights. This is assumed to be in the same order as \code{x}. Default is a vector of ones the same length as \code{x}.
#' @param ... Currently ignored.
#'
#'
#' @details This function is intended for internal purposes only and is called by \link{weightedLikelihood} and \link{weightedDistribution}.
#' The function calculates the weighted mean and weighted variance and performs a method of moments approach to obtain start values for the likelihood estimation.
#'
#'
#' @return Vector of estimated parameters for the distribution.
#'
#' @export getStartValue
#'
#' @examples
#'
#' x <- rnorm(100,10,5)
#'
#' getStartValue(x,'norm')
#' mean(x)
#' sd(x)
#'
#'

getStartValue <- function(x,distribution,w=rep(1,length(x)),...){
    ## for debugging
    ## x <- rbeta(100,2,17)
    ## hist(x)
    ## w <- rep(1,length(x))
    ## distribution <- 'beta'


    ## check the distribution
    distn <- tolower(distribution)


    allowedDist <- c('rayleigh','gamma','weibull','llog','norm','gompertz','beta','cauchy','chisq','exp','lnorm')
    if(!distn%in%allowedDist){
        stop(paste0('distribution must be one of the following: ',paste(allowedDist,collapse=', ')))
    }




    ## the w and x are assumed to be in the same order and the same length. Only the length can be checked
    if(length(w)!=length(x)){
        w <- rep(1,length(x))
        warning('The length of w is not same as the length of x and will be ignored in calculating the start values in the getStartValue function.')
    }#end if

    ## ensure numeric
    x <- as.numeric(x)
    w <- as.numeric(w)

    if(any(is.na(x)) || any(is.na(w))){
        stop('NA values are not allowed in x or w in the getStartValue function.')
    }#end if


    ## weighted summaries
    (wmean <- sum(w*x)/sum(w))
    (wvar <- sum(w*(x-wmean)^2)/(sum(w) - sum(w^2)/sum(w))) ## the weighted variance might not be needed, but I'm not sure
    (maxx <- max(w*x))


    ##
    gompertzStart <- function(xbar,varx){

        eulerConst <- -digamma(1) ## Euler-Mascheroni Constant
        g <- function(b,xbar,varx,eulerConst){
            ## approximate solution see Lenart 2012
            a.b <- pi^2/12 - b^2*varx/2
            1/b * exp(a.b)*(a.b - log(a.b) - eulerConst)-xbar
        }
        (maxg <- sqrt(pi^2/(6*varx))) ## max value

        low <- 1e-5;up <- maxg-1e-5

        ##y <- seq(low,up,length=100)
        ##g(y,eulerConst=eulerConst,xbar=xbar,varx=varx)


        suppressWarnings(b <- stats::nlminb(low,g,eulerConst=eulerConst,xbar=xbar,varx=varx,lower=1e-5)$par)

        ## find the root
        ##b <- stats::uniroot(g,interval=c(low,up),eulerConst=eulerConst,xbar=xbar,varx=varx)$root
        ## use b to find a
        a <- b*pi^2/12 - b^3/2 * varx
        ##c(b,a)
        return(c(b,a)) ## scale and shape

    } ## end gompertzStart function


    weibullStart <- function(xbar,varx,maxx){
        ## method of moments for weibull parameters
        wei <- function(b,varx,xbar){
            out <- exp(lgamma(1+2/b)-2*lgamma(1+1/b)) - 1 - varx/xbar^2
            return(out)
        }

        b <- tryCatch({
            stats::uniroot(wei,interval=c(1e-1,maxx),varx=varx,xbar=xbar)$root
        },error=function(cond){
            ## message(cond)
            suppressWarnings(b <- stats::nlminb(start=1e-1,objective=wei,varx=varx,xbar=xbar,lower=1e-5)$par)
            return(b)
        })
        a <- xbar/gamma(1+1/b)
        return(c(b,a))
    } ## end weibullStart function


    betaStart <- function(xbar,varx){
        ## method of moments for beta parameters
        if(!varx<xbar*(1-xbar)){
            ## method of moments does work under this condition
            return(c(1,1))
        }

        alpha <- xbar*(xbar*(1-xbar)/varx -1)
        beta <- (1-xbar)*(xbar*(1-xbar) /varx -1)
        return(c(alpha,beta))
    } ## end betaStart function



    lnormStart <- function(wx){
        meanLog <- -log(sum(wx^2))/2 + 2*log(sum(wx)) - 3*log(length(wx))/2
        sdLog <- sqrt(log(wx^2) - 2*log(sum(wx)) + log(length(wx)))
        return(c(meanLog,sdLog))
    }# end lnormStart




    ##c('rayleigh','gamma','weibull','llog','norm','gompertz','beta','cauchy','chisq','exp','lnorm')
    ## This function gets the starting values for the distribution of interest.
    ## the method of moments is a cheap way to get starting values for the likelihood
    out <- switch(distn,
                  rayleigh=c(wmean/sqrt(pi/2)), ## scale
                  gamma=c(wmean^2/wvar,wmean/wvar), ## shape and rate
                  weibull=weibullStart(xbar=wmean,varx=wvar,maxx=max(x)), ## shape and scale
                  llog=c(pi/sqrt(3*wvar),log(wmean)), ## shape and scale
                  norm=c(wmean,sqrt(wvar)), ## mean and sd
                  gompertz=gompertzStart(xbar=wmean,varx=wvar), ## scale and shape
                  beta=betaStart(xbar=wmean,varx=wvar), ## shape1 and shape2
                  cauchy=c(wmean,wvar), ## location and scale
                  chisq=c(wmean), ## degrees of freedom
                  exp=c(1/wmean), ## rate
                  lnorm=lnormStart(wx=w*x) ## meanlog and sdlog
                  ) #end switch
    return(out)
} #end function getStartValue

