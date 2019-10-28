
#' @name hullMuirAreaCorrection
#'
#' @title Calculate an area correction based on the Hull and Muir (2010) maximum
#'   distance
#'
#' @description Calculate the maximum fall distance from a turbine using the
#'   regression model from Hull and Muir (2010). Calculate the proabilities
#'   between one-unit increments of a right triangle distribution. Use the
#'   probabilities and proportion of area searched to calculate an area correction
#'   value.
#'
#' @param hubHeight Numeric, turbine hub height.
#' @param bladeRadius Numeric, turbine blade radius.
#' @param lowerBound Numeric, default is zero, see \code{\link{triangleProb}}.
#' @param proportionSearchDF Data frame with at least two columns: distance from
#'   turbine and proportion of area searched at each distance.
#' @param distanceCol Character string indicating the distance column in
#'   \code{proportionSearchDF}.
#' @param proportionCol Character string indicating the proportion column in
#'   \code{proportionSearchDF}.
#' @param additionalCol Character vector, default is NULL, indicating additional
#'   columns of how the area correction value should be calculated, see examples.
#' @param ... Currently ignored.
#'
#'
#' @details The maximum Hull and Muir distances are calculated using
#'   \code{\link{hullMuirMaxDistance}} and the probabilities are calculated using
#'   \code{\link{triangleProb}}. The probabilites are multipled by the proportion
#'   of area searched from \code{proportionSearchDF} by distance. The products are
#'   summed by size class and \code{additionalCol}.
#'
#'   The distances in the \code{distanceCol} will be rounded to the nearest
#'   integer for matching up with the probabilities. The distances, \code{hubHeight}, and \code{bladeRadius} are assumed to be in the same units.
#'
#'
#' @return Data frame of size class, \code{additionalCol} columns, and area
#'   correction
#'
#' @export hullMuirAreaCorrection
#'
#'
#' @references Hull, C. L., & Muir, S. (2010).
#'   Search areas for monitoring bird and bat carcasses at wind farms using a Monte-Carlo model.
#'   Australasian Journal of Environmental Management, 17(2), 77-87.
#'
#' @examples
#'
#' ## proportion of area searched data
#' data(proportionAreaSearched)
#'
#' hullMuirAreaCorrection(hubHeight = 87.5, bladeRadius = 62.5,
#'                        proportionSearchDF = proportionAreaSearched,
#'                        distanceCol = 'distanceFromTurbine',
#'                        proportionCol = 'proportionAreaSearched',
#'                        additionalCol = 'plotType')
#'
#' ## without additional columns but must separate the proportion of area searched
#' ## data frame
#' hullMuirAreaCorrection(hubHeight = 87.5, bladeRadius = 62.5,
#'                        proportionSearchDF = subset(proportionAreaSearched, plotType == 'RP'),
#'                        distanceCol = 'distanceFromTurbine',
#'                        proportionCol = 'proportionAreaSearched')
#'
#' hullMuirAreaCorrection(hubHeight = 87.5, bladeRadius = 62.5,
#'                        proportionSearchDF = subset(proportionAreaSearched, plotType == 'FULL'),
#'                        distanceCol = 'distanceFromTurbine',
#'                        proportionCol = 'proportionAreaSearched')



## hubHeight <- 87.5;bladeRadius <- 62.5;lowerBound<-0
## proportionSearchDF=proportionAreaSearched
## distanceCol = 'distanceFromTurbine'
## proportionCol= 'proportionAreaSearched'
## additionalCol = 'plotType'




hullMuirAreaCorrection <- function(hubHeight,bladeRadius,lowerBound=0,
                                   proportionSearchDF,distanceCol,proportionCol,
                                   additionalCol=NULL,...){


    # Check arguments.
    if(!is.data.frame(proportionSearchDF)){
        stop('proportionSearchDF must be a data.frame')
    }#end if

    if(length(distanceCol)!=1){
        stop('distanceCol must be a single string')
    }#end if

    if(length(proportionCol)!=1){
        stop('proportionCol must be a single string')
    }#end if

    propCols <- c(distanceCol,proportionCol,additionalCol)

    if(!is.character(propCols)){
        stop('distanceCol, proportionCol, and additionalCol must be class character')
    }#end

    unknownCols <- propCols[!propCols%in%names(proportionSearchDF)]
    if(length(unknownCols)>0){
        stop('The following columns are not found in proportionSearchDF: ',paste0(unknownCols,collapse=', '))
    }#end if

    proportionSearchDF[,distanceCol] <- round(proportionSearchDF[,distanceCol])

    if(any(duplicated(proportionSearchDF[,c(distanceCol,additionalCol)]))){
        stop('There are duplicated distances within the combinations of the additional columns')
    }#end if


    acCol <- 'pointEst'
    ## ensure it is not already in use
    while(acCol%in%propCols){
        acCol <- paste0(acCol,'Z')
    }#end if
    acCol


    ## hull and muir probabilities
    probResult <- triangleProb(hubHeight=hubHeight,bladeRadius=bladeRadius,
                               lowerBound=lowerBound)$triDistProb

    allDat <- merge(x=proportionSearchDF,y=probResult,
                    by.x=distanceCol,by.y='distanceFromTurbine',
                    all.x=TRUE,sort=FALSE)

    ## propSearch is beyond the max distance, make probabilities zero
    allDat$probability[is.na(allDat$probability)] <- 0


    ## calc AC at each distance
    allDat[,acCol] <- allDat[,proportionCol]*allDat$probability


    if(!is.null(additionalCol)){
        agFormula <- stats::formula(paste0(acCol,'~',paste0(c('size',additionalCol),collapse='+')))
    }else{
        agFormula <- stats::formula(paste0(acCol,'~','size'))
    }#end else if


    out <- stats::aggregate(formula=agFormula,FUN=sum,data=allDat)


    return(out)

}#end hullMuirAreaCorrection functin
