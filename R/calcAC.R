
#' @name calcAC
#'
#' @title Calculate the area correction value(s) with confidence intervals
#'
#' @description Use a fitted carcass density distribution and data describing the search area to calculate area correction values and confidence intervals using a
#'   parametric bootstrap approach.
#'
#' @param distribution Character indicating the distribution, passed to
#'   \code{\link{getDistanceProbability}}.
#' @param paramVec Numeric vector for the parameters associated with distribution. Assumed to be in the same order as the function indicated by \code{distribution}.
#' @param varcovVec Numeric vector for the variances and covariances for
#'   \code{paramVec}, default is NULL, see details.
#' @param proportionSearchDF Data frame with at least two columns: distance of the outer edge of an annulus from
#'   turbine and proportion of area searched within each annulus.
#' @param distanceCol Character indicating the column name for the distance from
#'   turbine
#' @param proportionCol Character indicating the column name for the proportion of
#'   area searched.
#' @param additionalCol Character vector, default is NULL, indicating additional
#'   columns of how the area correction value should be calculated, see details and examples.
#' @param nBoot Integer, indicating the number of parametric
#'   bootstrap replicates to use. Default is NULL, and not confidence intervals are produced.
#' @param truncBounds Numeric, indicating bounds for the area
#'   correction calculation, see details. Default is NULL, and the bounds are set to \code{c(0,Inf)}.
#' @param ciLevel Numeric, default is 0.9, desired confidence level for the
#'   bootstrap confidence interval.
#' @param randomSeed Numeric value of random seed, default is NULL.
#' @param ... Additional arguments passed to \code{\link{getDistanceProbability}} and \code{\link[mvtnorm]{rmvnorm}}.
#'
#' @details The function \code{\link{getDistanceProbability}} is used to calculate
#'   the probability (fraction of carcasses) in the intervals between distances in \code{proportionSearchDF}.
#'
#'   The \code{truncBounds} argument defaults to zero as a lower bound and infinity
#'   as the upper bound. If a single value is provided, it is assumed as the upper
#'   bound with zero as the lower bound. If two or more values are provided, the
#'   \code{max(truncBounds)} is the upper bound and \code{min(truncBounds)} is the
#'   lower bound.
#'
#'   If \code{varcovVec} is NULL, then parametric bootstrapping is impossible and a confidence interval is not estimated. The
#'   \code{varcovVec} should be in such an order that correctly fills the lower
#'   triangle including the diagonal. The first column is filled, then the second,
#'   and so on. This forms the variance-covariance matrix for the parameters.
#'
#'   If \code{nBoot} is greater than zero, a parametric bootstrap is done.
#'   Bootstrap parameters are generated using the \code{\link[mvtnorm]{rmvnorm}}
#'   function.
#'
#' If the additionalCol argument is not NULL, separate area corrections are estimated for each unique value within the column.
#'
#' @return \code{windAC} object
#'
#' @export
#'
#'
#' @seealso \code{\link{weightedLikelihood}} \code{\link{weightedDistribution}} \code{\link{getDistanceProbability}}
#'
#'
#' @import mvtnorm
#'
#' @examples
#'
#' ## proportion of area searched data
#' data(proportionAreaSearched)
#'
#' ## no parametric bootstrap
#' noBootstrap <- calcAC(distribution = 'gamma',
#'                       paramVec = c(2.483323, 0.02495139),
#'                       varcovVec = NULL,
#'                       proportionSearchDF = proportionAreaSearched,
#'                       distanceCol = 'distanceFromTurbine',
#'                       proportionCol = 'proportionAreaSearched',
#'                       additionalCol = 'plotType')
#'
#' ## with a parametric bootstrap
#'
#' withBootstrap <- calcAC(distribution = 'gamma',
#'                         paramVec = c(2.483323, 0.02495139),
#'                         varcovVec = c(0.041189428, 0.0008825275, 2.118081e-05),
#'                         proportionSearchDF = proportionAreaSearched,
#'                         distanceCol = 'distanceFromTurbine',
#'                         proportionCol = 'proportionAreaSearched',
#'                         additionalCol = 'plotType',
#'                         nBoot = 10)
#'



## ## for debuggin
## distribution= 'gamma'
## paramVec=c(2.483323,0.02495139)
## varcovVec = c(0.041189428, 0.0008825275, 2.118081e-05)
## proportionSearchDF=proportionAreaSearched
## distanceCol = 'distanceFromTurbine'
## proportionCol= 'proportionAreaSearched'
## additionalCol = 'plotType'
## nBoot=10
## truncBounds=NULL
## ciLevel=.9
## randomSeed <- NULL




calcAC <- function(distribution,paramVec,varcovVec=NULL,proportionSearchDF,distanceCol,
                   proportionCol,additionalCol=NULL,nBoot=NULL,truncBounds=NULL,
                   ciLevel=0.9,randomSeed=NULL,...){



    ## Check distribution.
    if(missing(distribution) || length(distribution)!=1){
        stop('argument distribution must be a single character string')
    }#end if

    ## for internal use
    distn <- tolower(distribution)


    ## Check parameters
    if(missing(paramVec) || !is.numeric(paramVec) || any(is.na(paramVec))){
        stop('argument paramVec needs to be numeric')
    } #end if

    ## for internal use
    param <- paramVec

    ## Check variances and covariances.
    if(!is.null(varcovVec)){
        if(missing(varcovVec) || !is.numeric(varcovVec) || any(is.na(varcovVec))){
            stop('argument varcovVec needs to be numeric or NULL')
        } #end if
    }#end if


    ## Check number of bootstraps and confidence level.
    if(!is.null(nBoot)&&!is.numeric(nBoot)){
        stop('argument nBoot needs to be numeric')
    } #end if

    ## null to zero value
    nBoot <- ifelse(is.null(nBoot),0,nBoot)

    ## set to zero if negative
    numBoot <- ifelse(nBoot<0,0,ceiling(nBoot))

    ## cannot do bootstrap without varcovVec
    if(is.null(varcovVec)){
        numBoot <- 0
    }#end if



    if(numBoot>0){ # ciLevel argument only matters if numBoot >0
        if(!is.numeric(ciLevel)){
            stop('argument ciLevel needs to be numeric')
        } #end if
        if(ciLevel>1|ciLevel<0){
            stop('argument ciLevel needs to be between 0 and 1')
        } #end if
    }#end if



    if(numBoot>0){
        S <- matrix(nrow=length(param),ncol=length(param))
        S[lower.tri(S,diag=TRUE)] <- varcovVec
        S[upper.tri(S)] <- S[lower.tri(S)]

        if(any(is.na(S))| any(diag(S)<=0)){
            stop('Problem creating the variance-covariance matrix from varcovVec')
        }#end if

    }else{
        S <- NULL
    }#end else if

    ## Check data and columns.
    if(missing(proportionSearchDF) || !is.data.frame(proportionSearchDF)){
        stop('proportionSearchDF must be a data.frame')
    }#end if

    if(missing(distanceCol) || length(distanceCol)!=1){
        stop('distanceCol must be a single string')
    }#end if

    if(missing(proportionCol) || length(proportionCol)!=1){
        stop('proportionCol must be a single string')
    }#end if

    propCols <- c(distanceCol,proportionCol,additionalCol)
    if(!is.character(propCols)){
        stop('distanceCol, proportionCol, and additionalCol must be class character')
    }#end

    unknownCols <- propCols[!propCols%in%names(proportionSearchDF)]
    if(length(unknownCols)>0){
        stop('The following columns are not in proportionSearchDF: ',paste0(unknownCols,collapse=', '))
    }#end if

    ## ## I'm not merging any more
    ## ## rounded for merging later
    ## ##proportionSearchDF[,distanceCol] <- round(proportionSearchDF[,distanceCol])
    ## if(any(duplicated(proportionSearchDF[,c(distanceCol,additionalCol)]))){
    ##     stop('There are duplicated distances within the combinations of the additional columns')
    ## }#end if


    ## name for probabilities later
    probCol <- 'prob'
    ## ensure it is not already in use
    while(probCol%in%propCols){
        probCol <- paste0(probCol,'Z')
    }#end if
    probCol

    ## name for ac later
    acCol <- 'pointEst'
    ## ensure it is not already in use
    while(acCol%in%propCols){
        acCol <- paste0(acCol,'Z')
    }#end if
    acCol


    ## name for rep later
    repCol <- 'rep'
    ## ensure it is not already in use
    while(repCol%in%propCols){
        repCol <- paste0(repCol,'Z')
    }#end if
    repCol


    ## Check bounds.
    if(!is.null(truncBounds)&&!is.numeric(truncBounds)){
        stop('argument truncBounds needs to be numeric')
    } #end if
    if(is.null(truncBounds)){
        tUp <- Inf
        tLow <- 0
    }else{
        tUp <- max(truncBounds,na.rm=TRUE)
        tLow <- min(truncBounds,na.rm=TRUE)
        if(length(truncBounds)==1){
            tLow <- 0
        }# end if
        if(length(truncBounds)>2){
            warning(paste('The smallest value and largest value of truncBounds',
                          'will be used as the bounds of the truncation.'),
                    immediate. = TRUE)
        }# end if
    } # end if else


    ## Check random seed.
    if (!is.null(randomSeed)) {
      if (!is.numeric(randomSeed)) {
        stop('argument randomSeed needs to be numeric')
      }#end if
    }# end if


    ## parameter matrix
    ## if bootstrap then first row is the original paramters
    if(numBoot>0){

        ## to catch bad arguments in dots for mvtnorm::rmvnorm
        dotify = function(fn, ...){
            do.call(fn, as.list(match.call()[names(match.call()) %in% names(formals(fn))]))
        }#end dotify function

        set.seed(randomSeed)
        ##rparam <- mvtnorm::rmvnorm(n=numBoot,mean=param,sigma=S)
        rparam <- dotify(fn=mvtnorm::rmvnorm,n=numBoot,mean=param,sigma=S,...)

        ## first row is the original parameters
        paramMatrix <- rbind(param,rparam)

    }else{
        paramMatrix <- matrix(param,ncol=length(param))
    }#end else if

    ## for indexing purposes
    if(ncol(paramMatrix)<2){
        paramMatrix <- cbind(paramMatrix,NA)
    }#end if



    if(!is.null(additionalCol)){
        agFormula <- stats::formula(paste0(acCol,'~',paste0(additionalCol,collapse='+')))
    }else{
        agFormula <- stats::formula(paste0(acCol,'~',1))
    }



    ## Calculate area correction.
    acValues <- data.frame()
    for(i in 1:nrow(paramMatrix)){
        allDat <- proportionSearchDF[,propCols]

        ## probabilities from the distribution
        allDat[,probCol] <- getDistanceProbability(q=allDat[,distanceCol],distribution = distn,
                                        param1 = paramMatrix[i, 1],
                                        param2 = paramMatrix[i, 2],
                                        tbound = c(tLow, tUp),...)

        allDat[,acCol] <- allDat[,proportionCol]*allDat[,probCol]

        thisRep <- stats::aggregate(formula=agFormula,FUN=sum,data=allDat)
        thisRep[,repCol] <- i-1

        acValues <- rbind(acValues,thisRep)

    }#end for i


    ## Set point estimates and confidence intervals.
    pointEst <- acValues[acValues[,repCol]==0,]
    pointEst[,repCol] <- NULL
    bootReps <- acValues[acValues[,repCol]>0,]

    if(nrow(bootReps)==0){
        bootReps <- NULL
    }#end if

    if(numBoot>0){

        bootConf <- stats::aggregate(formula=agFormula,FUN=stats::quantile,data=bootReps,probs=c((1-ciLevel)/2,1-(1-ciLevel)/2))
        bootBounds <- as.data.frame(bootConf[,acCol])
        names(bootBounds) <- paste0(c('L','U'),ciLevel*100)
        bootCI <- cbind(bootConf[,additionalCol,drop=FALSE],bootBounds)
        outSummary <- merge(pointEst,bootCI,by=additionalCol)

    }else{
        outSummary <- pointEst
    }#end else if


    # Create return object.
    out <- list()
    out$summary <- outSummary
    out$distribution <- distn
    out$parameters <- param
    out$paramVarCov <- S
    out$bootstrap <- bootReps

    class(out) <- c('windAC','list')
    ##attr(out,'hidden') <- c('bootstrap','distribution','parameters','paramVarCov')


    return(out)
}#end calcAC function
