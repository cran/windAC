
#'
#'
#'
#'
#' @rdname weightedLikelihood
#'
#' @export estTWL
#'

estTWL <- function(fatDist,fatW,distribution,plotBounds=NULL,...){

    ## estiamte the truncated weighted likelihood for each distribution
    outList <- lapply(distribution,weightedLikelihood,fatDist=fatDist,fatW=fatW,plotBounds=plotBounds,...)
    ## put into a data.frame
    out <- do.call(rbind,outList)
    ## reorder for convenience
    out <- out[with(out,order(aicc)),]

    return(out)
} ## end estTWL function
