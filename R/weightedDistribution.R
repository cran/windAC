################################################
## Jared Studyvin
## 8 June 2016
## get parameter estimates and AIC for the weighted distribution
################################################



#'
#' @name weightedDistribution
#' @aliases estWD
#' @aliases weightedDistribution
#'
#' @title Weighted Distribution Estimation
#'
#' @description Maximum likelihood is done on a weighted distribution.
#' The weighted distribution is a typical probability density distribution multiple by a weight function.
#'
#'
#'
#'
#' @param fatDist Vector of fatality distanes from the turbine.
#' @param weightFun R function that is multipled by the probability distribution, see details.
#' @param distribution Single character indicating the distribution for \code{weightedDistribution} or vector for \code{estWD}.
#' @param ... Additional arguments passed to \code{weightFun} or \code{\link[stats]{optim}}.
#'
#'
#' @details The function \code{estWD} is a convient wrapper function to \code{weightedDistribution}, for fitting multiple distributions.
#'
#' The weight function should return a (relative) probability of detection at every distance.
#' Typically this is the proportion of area searched.
#' The function \code{\link{weightFun}} is set up to take a table of proportion of area searched and return values in a function format.
#'
#' Let \eqn{h(x)} be the weight function (\code{weightFun}), \eqn{f(x|\theta)} be a probability density function (specified by \code{distribution}, \eqn{x} be the vector of carcass distances from the turbine (\code{fatDist}), and \eqn{\theta} be the parameter vector to be estimated.
#' The weighted distribution is
#' \deqn{f_{d}(x|\theta) = \frac{h(x)f(x|\theta)}{\int h(y)f(y|\theta)dy}}
#' The likelihood that is maximized is
#' \deqn{L_{d}(\theta|\underbar{x}) = \prod_{i=1}^{n}\frac{h(x_i)f(x_i|\theta)}{\int h(y)f(y|\theta)dy}}
#'
#'
#'
#' @return Data frame of the parameter estimates with fit statistics.
#'
#' @export weightedDistribution
#'
#' @seealso \code{\link{calcAC}}
#'
#' @seealso \code{\link{weightFun}}
#'
#'
#' @examples
#' ## load the data
#' data(carcassDistance)
#' data(proportionAreaSearched)
#'
#' ###############################################
#' ## fit for fall carcasses found on road and pad (RP)
#' distanceFallRP <- subset(carcassDistance,plotType=='RP'&season=='fall',
#' select=distanceFromTurbine,drop=TRUE)
#'
#' \donttest{
#' fallRPFit <- estWD(fatDist=distanceFallRP,weightFun=weightFun,
#' distribution=c('norm','gamma','weibull'),propTable=proportionAreaSearched,type='RP',
#' typeCol='plotType',distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',
#' maxDistance=100)
#' }
#'
#' ###############################################
#' ## fit for fall carcasses found on full plots
#' distanceFallFP <- subset(carcassDistance,plotType=='FULL'&season=='fall',
#' select=distanceFromTurbine,drop=TRUE)
#'
#' \donttest{
#' fallFPFit <- estWD(fatDist=distanceFallFP,weightFun=weightFun,
#' distribution=c('norm','gamma','weibull'),propTable=proportionAreaSearched,type='FULL',
#' typeCol='plotType',distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',
#' maxDistance=100)
#' }
#'




weightedDistribution <- function(fatDist,weightFun,distribution,...){


    distn <- tolower(distribution)
    ## allowedDist <- c('rayleigh','gamma','weibull','llog','norm','gompertz')
    ## if(!distn%in%allowedDist){
    ##     stop(paste0('distribution must be one of the following: ',paste(allowedDist,collapse=', ')))
    ## }

    ## for debugging
    ## print(distn)



    ##wFat <- weightFun(fatDist,propTable=proportionAreaSearched,type='RP',typeCol='plotType',distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',maxDistance=100)

    wFat <- weightFun(fatDist,...)

    ## starting values for the optimization
    (startValue <- getStartValue(fatDist,distn,w=1/wFat))


    toIntegrateFun <- function(x,parm,w,distn,...){

        ## This function is the weight function times the pdf
        ## This function is integrated in the likelihood for the constant
        out <- eval(parse(text=paste0('w(x,...)*dtrunc(x=x,distribution=distn,tbound=c(-Inf,Inf),',paste0(parm,collapse=','),',log=FALSE)')))

        return(out)
    } # end toIntegrateFun function

    ## for testing
    ##plot(fatDist,toIntegrateFun(x=fatDist,startValue,weightFun,distn))


    loglik <- function(parm,xx,distn,w,...){

        ## for debugging
        ##print(parm)

        ## get the correct log of f
        logf <- suppressWarnings(eval(parse(text=paste0('dtrunc(x=xx,distribution=distn,tbound=c(-Inf,Inf),',paste0(parm,collapse=','),',log=TRUE)'))))

        if(any(is.na(logf))){
            ## indicates bad parameter values
            return(NA)
        }#end if




        ## all dist have a lower bound of zero except the normal distribution
        lowLim <- -Inf #ifelse(distn=='norm',-Inf,0)
        upLim <- Inf ## all distribution have an upper bound of infinity

        ## integral value
        c <- tryCatch({
            stats::integrate(toIntegrateFun,lower=lowLim,upper=upLim,parm=parm,w=w,distn=distn,...)$value
        },error=function(cond){
            ##message(cond)
            ##message('Error with integration in the weighted distribution function.')
            ##message('The distribution is ',distn,', with parameter values: ',paste(parm,collapse=','))
            return(NA)

        },finally={}) #end tryCatch


        ##print(c)
        ##print(logf)
        ##print(sum(log(w(xx,...))))

        ## sum the liklihood
        loglik <- sum(logf-log(c)) + sum(log(w(xx,...)))

        ## return the negative because the optimizer is a minimizer
        return(-loglik)
    } # end function loglik

    ## for testing
    ##loglik(startValue,fatDist,distn,weightFun,subdivisions=1000)

    lowLim <- ifelse(distn=='norm',c(-Inf,1e-10),c(1e-10,1e-10))


    ## for testing
    ##fit <- stats::nlminb(start=startValue,objective=loglik,xx=fatDist,distn=distn,w=weightFun,propTable=proportionAreaSearched,type='RP',typeCol='plotType',distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',maxDistance=100,lower=lowLim)

    ##secondDerivative(fit$par,FUN=loglik,xx=fatDist,distn=distn,w=weightFun,propTable=proportionAreaSearched,type='RP',typeCol='plotType',distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',maxDistance=100,lower=lowLim)


    ##fit <- stats::optim(par=startValue,fn=loglik,xx=fatDist,distn=distn,w=weightFun,propTable=proportionAreaSearched,type='RP',typeCol='plotType',distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',maxDistance=100,hessian=TRUE)




    fitOptim <- tryCatch({
            ## next try optim
            ##print('trying optim')
            out <- stats::optim(par=startValue,fn=loglik,xx=fatDist,distn=distn,w=weightFun,...,hessian=TRUE)
            out$objective <- out$value
            out

    },error=function(cond){
        ##print(cond)
        return(list(objective=NA,par=NA,convergence=1,message='Error in optim when optimizing'))
    }) ## end tryCatch



    SOptim <- tryCatch({
        Sout <- solve(fitOptim$hessian)
        if(any(is.na(Sout))){
            stop('variance problem')
        }#end if
        Sout
    },error=function(cond){

        Sout <- tryCatch({
            Sout <- solve(secondDerivative(fitOptim$par,FUN=loglik,xx=fatDist,distn=distn,w=weightFun,...))
            if(any(is.na(Sout))){
                stop('variance problem')
            }#end if
            Sout
        },error=function(cond){
            return(matrix(nrow=2,ncol=2))
        })# end tryCatch

        return(Sout)

    }) ## end tryCatch

    fit <- fitOptim
    S <- SOptim



    ## estimated values,make of length 2, if not already
    (fitParm <- c(fit$par,NA)[1:2])
    ## ensure there are three values
    (varVec <- c(S[lower.tri(S,diag=TRUE)],NA,NA)[1:3])



    likeValue <- fit$objective


    ## calculate the AIC value
    k <- length(startValue)
    n <- length(fatDist)
    (aic <- 2*(k+likeValue)) ## AIC value
    if(k+1>=n){ ## accounts for small sample size relative to k, ie not divide by zero
        aicc <- aic
    }else{
        (aicc <- aic+2*k*(k+1)/(n-k-1)) ## corrected AIC value
    }
    ## I'm not sure this is the best way to compare between distributions



    ## I would like to calculate the kolmogorov-smirnov test statistic
    ## I'm not sure how to calculate the emipirical CDF


    fitMessage <- fit$message
    fitMessage <- ifelse(is.null(fitMessage),'',fitMessage)

    out <- data.frame(distribution=distn,param1=fitParm[1],param2=fitParm[2],
                      var1 = varVec[1],
                      covar = varVec[2],
                      var2 = varVec[3],
                      code=fit$convergence,message=fitMessage,aic=aic,aicc=aicc,
                      nFit=length(fatDist))


    return(out)

} # end weightedDistribution function
