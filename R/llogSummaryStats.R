#'
#' @rdname LogLogistic
#'
#'




llogSummaryStats <- function(shape,scale){

    a <- shape
    s <- scale

    out <- data.frame(median=exp(s))

    out$mean <- ifelse(1/a > 1, (exp(s)*pi*a)/(sin(pi*a)), NA)

    out$mode <- ifelse(1/a > 1, exp(s)*((1/a - 1)/(1/a + 1))^a,0)

    out$var <- ifelse(1/a > 2, (exp(s)^2)*(2*pi*a/sin(2*pi*a) - (a*pi/sin(a*pi))^2),NA)

    return(out)

}#end llogSummaryStats
