#####################################################################
## Jared Studyvin
## 13 June 2016
## Weighted likelihood
#####################################################################


#'
#'
#' @name weightedLikelihood
#' @aliases weightedLikelihood
#' @aliases estTWL
#'
#' @title Truncated Weighted Likelihood Estimation
#'
#' @description Maximum likelihood estimation is done on a likelihood that is truncated (possibly) and weighted. The weights are associated with each observation.
#'
#'
#' @param fatDist Vector of fatality distances from the turbine.
#' @param fatW Vector of weights, to weight the likelihood for estimatation. This must be the same length as fatDist and is assumed to be in the same order as fatDist.
#' @param distribution Single character indicating the distribution for \code{weightedLikelihood} or vector for \code{estTWL}.
#' @param plotBounds Vector of length 1 or 2. If the length is 2 (or greater) the max value is used as the upper truncation bound and the min value is used as the lower truncation bound. If the length is 1 this value is taken as the upper truncation bound and zero is set as the lower truncation bound. The default is NULL, in which case the bounds are zero and positive infinity.
## #' @param varCov Logical, default is FALSE, if TRUE the estimated variances and covariance for the parameter estimates are also calculated.
#' @param ... Additional arguments passed to \code{\link[stats]{optim}}.
#'
#'
#'
#' @details  The truncated likelihood for a single observation is
#' \deqn{L^*(\theta|x_i) = \frac{f(x_i|\theta)}{\int_{a}^{b}f(y|\theta)dy}}
#'
#' Where \eqn{x_i} is \code{fatDist}, \eqn{\theta} is the vector of parameters to be estimated, \code{a} and \code{b} correspond to the \code{plotBounds} and \code{f()} is the \code{distribution} chosen.
#'
#' The truncated weighted likelihood is
#' \deqn{TWL(\theta|\underbar{x}) = \prod_{i=1}^{n}L^*(\theta|x_i)^{w_i}}
#' Where \code{n=length(fatDist)} and \eqn{w_i} is \code{fatW}.
#'
#' The truncated weighted likelihood is then estimated using standard maximum likelihood techniques.
#'
#' See \code{\link{estTWL}} for examples.
#'
#'
#'
#' @return Data frame of the parameter estimates for the distribution with fit statistics.
#'
#' @export weightedLikelihood
#'
#'
#' @seealso \code{\link{calcAC}}
#'
#' @examples
#' ## load the data
#' data(carcassDistance)
#' data(proportionAreaSearched)
#'
#' ## add proportion of area searched to each carcass
#' carcDist <- merge(carcassDistance,proportionAreaSearched,
#' by=c('plotType','distanceFromTurbine'),all.x=TRUE)
#'
#' ## create the weight for each carcass
#' carcDist$w <- with(carcDist,1/(proportionAreaSearched*probabilityDetection))
#'
#' twlOutput <- with(carcDist,estTWL(fatDist=distanceFromTurbine,fatW=w,plotBounds=c(0,100),
#' distribution=c('norm','weibull','gamma')))
#'





## ## for testing
## fatDist=carcDist$distanceFromTurbine
## fatW=carcDist$w
## plotBounds=c(0,100)
## distribution=c('weibull')


weightedLikelihood <- function(fatDist,fatW,distribution, plotBounds=NULL,...){


################################
    ## argument checking
    if(!is.character(distribution)||length(distribution)!=1){
        stop('argument distribution must be a single character string')
    }#end if

    if(!is.null(plotBounds)&&!is.numeric(plotBounds)){
        stop('argument plotBounds need to be numeric')
    } #end if

    if(!is.numeric(fatDist)){
        stop('argument fatDist need to be numeric')
    } #end if

    if(!is.numeric(fatW)){
        stop('argument fatW need to be numeric')
    } #end if


    if(length(fatW)!=length(fatDist)){
        fatW <- rep(1,length(fatDist))
        warning('The length of fatW is not same as the length of fatDist and will be ignored in calculating the weighted likelihood parameter estimates.')
    }# end if



################################
    (distn <- tolower(distribution))


    if(is.null(plotBounds)){
        maxDist <- Inf
        minDist <- 0
    }else{
        maxDist <- max(plotBounds)
        minDist <- min(plotBounds)
        if(length(plotBounds)==1){
            minDist <- 0
        }# end if
        if(length(plotBounds)>2){
            warning('The smallest value and largest value of plotBounds will be used as the bounds of the truncation.')
        }# end if
    } # end if else


    ##maxDist
    ##minDist


    ## The weighed likelihood
    wLogLik <- function(parm,xx,w,distn,low=-Inf,up=Inf,...){

        ## If nlminb passes NaN values to the function
        if(any(is.na(parm))){
            return(NA)
        }#end if

        loglik <- suppressWarnings(eval(parse(text=paste0('dtrunc(x=xx,distribution=distn,tbound=c(low,up),',paste0(parm,collapse=','),',log=TRUE)'))))

        wLL <- sum(w*loglik)

        return(-wLL)
    }#end wLogLik



    ## starting values for the optimization
    (startValue <- getStartValue(x=fatDist,distribution=distn,w=fatW))



###### first try optim with hessian
    fitOptim <- tryCatch({
        ##print('optim with hessian')
        out <- stats::optim(par=startValue,fn=wLogLik,xx=fatDist,distn=distn,w=fatW,low=minDist,up=maxDist, ...,hessian=TRUE)
        out$objective <- out$value ## match nlminb output
        if(out$convergence!=0){
            stop('optimization did not converge')
        }#end if

        out

    },error=function(cond){
        message(cond)


##### second try optim without hessian
        fitOptim2 <- tryCatch({
##print('optim without hessian')
            out <- stats::optim(par=startValue,fn=wLogLik,xx=fatDist,distn=distn,w=fatW,low=minDist,up=maxDist, ...,hessian=FALSE)
            out$objective <- out$value ## match nlminb output
            if(out$convergence!=0){
                stop('optimization did not converge')
            }#end if

            out

        },error=function(cond){
            message(cond)

##### third try nlminb
            fitOptim3 <- tryCatch({
##print('nlminb')
                out <- stats::nlminb(start=startValue,objective=wLogLik,xx=fatDist,distn=distn,w=fatW,low=minDist,up=maxDist, ...)
                out$hessian <- NA # to match optim
                out

            },error=function(cond){
                message(cond)
                return(return(list(objective=NA,par=NA,convergence=1,message='Error in optim when optimizing')))

            }) ## end tryCatch 3

            return(fitOptim3)

        }) ## end tryCatch 2


        return(fitOptim2)


    }) ## end tryCatch 1



    SOptim <- tryCatch({
        Sout <- solve(fitOptim$hessian)
        if(any(is.na(Sout))){
            stop('variance problem')
        }#end if
        Sout
    },error=function(cond){

        tryCatch({
            #try the second Derivative
            Sout <- solve(secondDerivative(fitOptim$par,FUN=wLogLik,xx=fatDist,distn=distn,w=fatW,low=minDist,up=maxDist))
            if(any(is.na(Sout))){
                stop('variance problem')
            }#end if
            Sout
        },error=function(cond){
            return(matrix(nrow=2,ncol=2))
        })# end tryCatch
    }) ## end tryCatch

    fit <- fitOptim
    S <- SOptim


    ## estimated values,make of length 2, if not already
    (fitParm <- c(fit$par,NA)[1:2])


    ## make length 3, if not already
    (varVec <- c(S[lower.tri(S,diag=TRUE)],NA,NA)[1:3])


    ## The world is not ready for a Kolmogorov-Smirnov fit statistic

    ## ## weighted empirical cdf
    ## ecdf <- function(q,data,w){
    ##     ## q <- 35:40
    ##     ## data <- rnorm(20,mean=40)
    ##     ## w <- stats::runif(length(data))


    ##     ## out <- plyr::aaply(q,1,function(t,d,w){

    ##     ##           sum(w*(d<=t))/sum(w)

    ##     ##       },d=data,w=w)


    ##     out <- sapply(X=q,FUN=function(t,d,w){
    ##               sum(w*(d<=t))/sum(w)
    ##           },d=data,w=w)
    ##     return(out)
    ## }



    ## ## For the Kolmogorov-Smirnov
    ## ksStat <- function(q,parm,distn,data,w,...){
    ##     ## need to switch CDF for qtrunc
    ##     -abs(CDF(q=q,parm=parm,distribution=distn,...)-ecdf(q=q,data=data,w=w))
    ## }

    ## ksFit <- stats::nlminb(start=mean(fatDist),ksStat,parm=fitParm,distn=distn,data=fatDist,w=fatW,low=minDist,up=maxDist)
    ## (KS <- -ksFit$objective)

    likeValue <- fit$objective


    ## calculate the AIC value
    (k <- length(startValue))
    (n <- length(fatDist))
    (aic <- 2*(k+likeValue)) ## AIC value

    if(k+1>=n){ ## accounts for small sample size relative to k, ie not divide by zero
        aicc <- aic
    }else{
        (aicc <- aic+2*k*(k+1)/(n-k-1)) ## corrected AIC value
    }#end else if
    ## I'm not sure this is the best way to compare between distributions


    (code <- fit$convergence)
    (message <- ifelse(is.null(fit$message),'',fit$message))


    out <- data.frame(distribution=distn,param1=fitParm[1],param2=fitParm[2],
                      var1 = varVec[1],
                      covar = varVec[2],
                      var2 = varVec[3],
                      code=code,message=message,
                      aic=aic,aicc=aicc,nFit=length(fatDist))

    return(out)

}  # end weighteLikelihood function
