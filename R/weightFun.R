####################################################################
## Jared Studyvin
## 9 June 2016
## This is a weight function for testing purposes
####################################################################





#'
#' @name weightFun
#'
#' @title weight function
#'
#' @description Generic weight function for use with \code{\link{estWD}}.
#'
#'
#' @param x Numeric vector.
#' @param propTable Data frame contain the proportion of area searched by distance and plot type.
#' @param type Character, indicating which plot type to subset \code{propTable}.
#' @param typeCol Character, column name of the plot type in \code{propTable}.
#' @param distanceCol Character, column name of the distance in \code{propTable}.
#' @param propCol Character, column name of the proportion of area searched in \code{propTable}.
#' @param maxDistance Numeric, default is \code{NULL}. If a value is given then \code{propTable} is subsetted to where \code{propTable[,distanceCol] <= maxDistance}.
#' @param xFun Function, default is \code{\link[base]{ceiling}}, see details.
#' @param ... Additional arguments passed to \code{xFun}.
#'
#'
#' @return Numeric vector of weights with length equal to \code{length(x)}, and with a 1:1 relationship to the values in \code{x}.
#'
#' @export weightFun
#'
#' @seealso \code{\link{estWD}}
#'
#' @details The \code{\link{weightedDistribution}} function requires the weights be described using a function.
#' This allows integration to happen.
#'
#' Typically \code{propTable} has integer values for the distances, but the function needs to take in any numeric values, the \code{xFun} function is how any numeric value can be matched up to the values in \code{propTable}.
#' If the distances in \code{propTable} correspond to the outer radius of the annuli, for calculating proportion of area searched, then the \code{\link[base]{ceiling}} is appropriate.
#' If the distances in \code{propTable} correspond to the inner radius of the annuli then the \code{\link[base]{floor}} might be more appropriate.
#'
#'
#'
#'
#'
#' @examples
#'
#' data(proportionAreaSearched)
#'
#' d <- c(-300.23,14.3,16,75)
#'
#' ## RP proportion of area searched
#' weightFun(x=d,propTable=proportionAreaSearched,type='RP',typeCol='plotType',
#' distanceCol='distanceFromTurbine',propCol='proportionAreaSearched')
#' #[1] 0.00000000 0.08896480 0.08308577 0.01709869
#'
#' ## FULL plot proportion of area searched
#' weightFun(x=d,propTable=proportionAreaSearched,type='FULL',typeCol='plotType',
#' distanceCol='distanceFromTurbine',propCol='proportionAreaSearched')
#' # [1] 0 1 1 1
#'
#' ### with a max distance restriction
#' ## RP proportion of area searched
#' weightFun(x=d,propTable=proportionAreaSearched,type='RP',typeCol='plotType',
#' distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',maxDistance=40)
#' # [1] 0.00000000 0.08896480 0.08308577 0.00000000
#'
#' ## FULL plot proportion of area searched
#' weightFun(x=d,propTable=proportionAreaSearched,type='FULL',typeCol='plotType',
#' distanceCol='distanceFromTurbine',propCol='proportionAreaSearched',maxDistance=40)
#' # [1] 0 1 1 0
#'
#'



weightFun <- function(x,propTable, type,typeCol,distanceCol,propCol,xFun=ceiling,maxDistance=NULL,...){


###########################
    ## argument checking

    if(!is.numeric(x)){
        stop('agument x must be numeric')
    }# end if

    cols <- c(typeCol,distanceCol,propCol)

    if(!is.character(cols)|| length(cols) != 3){
        stop('arguments typeCol, distanceCol, propCol each need to be a single character string')
    }# end if

    if(!is.data.frame(propTable) || !all(cols%in%names(propTable))){
        stop(' argument propTable needs to be data.frame with column names that match the arguments typeCol and distanceCol')
    } # end if

    if(!is.character(type)||length(type)!=1||!all(type%in%unique(propTable[,typeCol]))){
        stop('argument type must be a single character and it must be value of the typeCol in propTable')
    }# end if

    maxDist <- NULL
    if(!is.null(maxDistance)){
        if(!is.numeric(maxDistance)){
            stop('argument maxDistance needs to be numeric')
        }#end if
        maxDist <- max(maxDistance,na.rm=TRUE)
    }# end if


###########################

    formalize <- function(f){

        ## if the function already has ...
        if(any(grepl('...',names(formals(f))))){
            return(f)
        }#end if

        ## add ... to formals
        formals(f) <- c(formals(f), alist(...=))
        ## release the ... in the local environment
        body(f)    <- substitute({x;y},list(x = quote(list2env(list(...))),y = body(f)))
        return(f)
    }#end formalize


    ## turn the weight function into a step function
    if(is.null(names(formals(xFun)))){
        x <- xFun(x)
    }else{
        xFun <- formalize(xFun) ## allow the function to handle ... if it can't
        x <- xFun(x,...)
    }#end if else

    prop <- propTable[propTable[,typeCol] == type,]

    if(!is.null(maxDist)){
        prop <- prop[prop[,distanceCol] <= maxDist,]
    }# end if

    ## if the subsetting elimates all rows
    if(nrow(prop)<1){
        return(rep(0,length(x)))
    }#end if


    ## ## Merge distances x with the prop to get the proporatin of area search for each value of x
    ## ##out <- dplyr::left_join(data.frame(d=x),prop,by=c('d'='d'))$prop

    out <- prop[match(x,prop[,distanceCol]),propCol]



    ## ## na values indicate distances outside the interval (0,maxDist] and the proportion of area searched is zero
    out[is.na(out)] <- 0

    return(out)
} # end weightFun function
