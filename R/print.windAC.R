#'
#'
#'
#' @title Print \code{windAC} object
#'
#' @description Print \code{windAC} object.
#'
#' @param x A \code{windAC} object.
#' @param ... Currently ignored.
#'
#' @details see \code{\link{calcAC}}
#'
#' @export
#'
#' @return Print for windAC object
#'
#'
#'
print.windAC <- function(x, ...){
  print(x[['summary']])
}#end print.windAC function
