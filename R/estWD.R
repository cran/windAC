
#'
#' @rdname weightedDistribution
#'
#'
#' @export estWD
#'


estWD <- function(fatDist,weightFun,distribution,...){

    fitDist <- tolower(distribution)

    ## estimate the truncated weighted likelihood for each distribution
    outList <- lapply(fitDist,weightedDistribution,fatDist=fatDist,weightFun=weightFun,...)
    ## put into a data.frame
    out <- do.call(rbind,outList)
    ## reorder for convenience
    out <- out[with(out,order(aicc)),]

    return(out)
} ## end estTWL function
