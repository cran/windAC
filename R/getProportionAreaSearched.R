#' @name getProportionAreaSearched
#'
#' @title Create proportion of area searched table from spatial data
#'
#' @description Calculate proportion of area searched around wind turbine based on turbine location data and polygons of search area.
#'
#'
#' @param turbinePoints Spatial points object with with data frame indicating turbine names
#' @param turbineName Character, indicating the variable name for the turbine names in \code{turbinePoints} and plot names in \code{turbinePlots}
#' @param turbinePlots Spatial polygon objects indicating the search area around the turbine points
#' @param turbineMastRadius Integer, radius of the turbine mast
#' @param maxDistance Integer, indicating how far from the turbine that searches occured
#'
#' @details The \code{\link[sf]{sf}} package is used to calculate overlapping areas between the searched area \code{turbinePlots} and one unit annulus around the \code{turbinePoints}. The annuli increase out to a distance of \code{maxDistance}.
#'
#' Caution, the function does some basic checks on the spatial objects but it is assumed that the points and polygons do not have any boundry, geometry, or other issues.
#'
#' @return Data frame of proportion of area searched for each annulus around each turbine point. \code{distanceFromTurbine} column represents outer radius of each annulus.
#'
#' @export getProportionAreaSearched
#'
#' @import sf
#'
#'
#' @examples
#'
#'
#' data(turbineSpatial)
#'
#' propSearch <- getProportionAreaSearched(turbinePoints=turbineSpatial$turbinePoints,
#' turbineName='turbName',turbinePlots=turbineSpatial$turbinePlots,
#'  turbineMastRadius=2,maxDistance=10)
#'
#'



## Notes:
## overlapping polygon example
## a single search plot is made up of several polygons
## several polygons that are not touching
## search plot from different turbines are overlapping
## read in the spatial data as as sp object



## ## for testing
## library(sf)
## data(turbineSpatial)

## turbinePoints <- turbineSpatial$turbinePoints
## turbinePlots <- turbineSpatial$turbinePlots
## maxDistance <- 100
## turbineMastRadius <- 2
## turbineName <- 'turbName'


getProportionAreaSearched <- function(turbinePoints,turbineName,turbinePlots,turbineMastRadius,maxDistance){


    ## Check to see if the package version is updated.
    if(getRversion() < "3.6.0") {
        warning("R Version should be updated to 3.6.0 or above. Errors may occur in earlier verions of R.")
    }#end if


    ## convert sp object to sf object
    if('sp' %in% class(turbinePoints)) {
        turbinePoints <- sf::st_as_sf(turbinePoints)
    }#end if


    if('sp' %in% class(turbinePlots)) {
        turbinePlots <- sf::st_as_sf(turbinePlots)
    }#end if

    # ensure correct geometry type from inputs
    if (!any(sf::st_geometry_type(turbinePlots) %in% c("POLYGON", "MULTIPOLYGON"))) {
        stop("'turbinePlots' input must be of sf geometry type 'POLYGON' or 'MULTIPOLYGON'")
    }
    
    if (!any(sf::st_geometry_type(turbinePoints) %in% c("POINT", "MULTIPOINT"))) {
        stop("'turbinePoints' input must be of sf geometry type 'POINT' or 'MULTIPOINT'")
    }
        
    ## Check to see if the data is longitude/latitude data.
    if(isTRUE(sf::st_is_longlat(turbinePoints))) {
        stop(paste("sf::st_is_longlat detects that turbinePoints uses a longitude/latitude coordinate system.",
                   "use st_transform to convert to a projected coordinate system."))
    }#end if

    if(isTRUE(sf::st_is_longlat(turbinePlots))) {
        stop(paste("sf::st_is_longlat detects that turbinePlots uses a longitude/latitude coordinate system.",
                   "use st_transform to convert to a projected coordinate system."))
    }#end if



    if(!is.character(turbineName) || length(turbineName)!=1 || !turbineName%in%names(turbinePoints) || !turbineName%in%names(turbinePlots)){
        stop('turbineName must be a single string of a name of a variable in turbinePoints and turbinePlots')
    }#end if



    if(!is.numeric(turbineMastRadius)){
        stop('turbineMastRadius must be numeric')
    }#end if


    ## Detect invalid geometries such as polygons that overlap themselves.
    if(!all(sf::st_is_valid(turbinePoints))) {
        invalid <- !sf::st_is_valid(turbinePoints)
        stop(paste("sf::st_is_valid found invalid Geometries on point(s)",
                   paste(which(invalid),collapse=", "),
                   "with errors:",
                   paste(sf::st_is_valid(turbinePoints,reason=TRUE),collapse=", ")))
    }#end if

    ## Detect invalid geometries such as polygons that overlap themselves.
    if(!all(sf::st_is_valid(turbinePlots))) {
        invalid <- !sf::st_is_valid(turbinePlots)
        stop(paste("sf::st_is_valid found invalid Geometries on polygon(s)",
                   paste(which(invalid),collapse=", "),
                   "with errors:",
                   paste(sf::st_is_valid(turbinePlots,reason=TRUE),collapse=", ")))
    }#end if


    ## for internal use
    turbPoints <- turbinePoints
    turbName <- turbineName
    ## make non-zero integer
    mastRad <- max(floor(min(turbineMastRadius)),0)
    ## distances will be convert to be measured from the edge of the turbine mast
    maxDist <- ceiling(max(maxDistance)) + mastRad

    ## ensure same projections for use later
    turbPlots <- sf::st_transform(turbinePlots,crs=sf::st_crs(turbPoints))


    ## trying to get this done without looping over the turbines
    useR <- (mastRad+1):maxDist
    annuli <- NULL
    for(R in useR){

        ## sf::st_buffer can take a vector - we could explore this logic to increase speed


        ## buffer with radius R
        cirR <- sf::st_sf(sf::st_buffer(sf::st_geometry(turbPoints),dist=R,nQuadSegs=1000))
        names(cirR)[1] <- "geometry"
        sf::st_geometry(cirR) <- "geometry"
        cirR$R <- R
        cirR$cirName <- as.data.frame(sf::st_drop_geometry(turbPoints))[,turbName]

        ## buffer with radius R-1
        cirRminus1 <- sf::st_sf(sf::st_buffer(sf::st_geometry(turbPoints),dist=R-1,nQuadSegs=1000))
        names(cirRminus1)[1] <- "geometry"
        sf::st_geometry(cirRminus1) <- "geometry"
        cirRminus1$Rminus1<- R-1
        cirRminus1$cirNameMinus1<- as.data.frame(sf::st_drop_geometry(turbPoints))[,turbName]

        ## creating annuli

        ## create annuli for each meter band out from turbine mast edge
        annuliExtra <- do.call("rbind",
                               lapply(1:nrow(turbinePlots), function(x,fxn_cirR,fxn_cirRminus1) {
                                   tempCirR <- fxn_cirR[x, ]
                                   tempCirRminus1 <- fxn_cirRminus1[x, ]
                                   tempAnnuliExtra <- suppressWarnings(sf::st_difference(tempCirR, tempCirRminus1))
                                   return(tempAnnuliExtra)
                               },fxn_cirR = cirR,fxn_cirRminus1 = cirRminus1))


        ## columns not needed any more
        annuliExtra[,c('Rminus1','cirNameMinus1')] <- NULL

        annuli <- rbind(annuli,annuliExtra)

    }#end for R


    ## ## test plotting
    ## xlim <- sf::st_bbox(turbPoints[1,])[c(1,3)]+c(-10,10)
    ## ylim <- sf::st_bbox(turbPoints[1,])[c(2,4)]+c(-10,10)
    ## plot(sf::st_geometry(turbPoints[1,]),xlim = xlim,ylim=ylim)
    ## plot(sf::st_geometry(annuli[annuli$cirName==1,]),add=TRUE)
    ## plot(sf::st_geometry(annuli[annuli$R%%2==0 && annuli$cirName==1,]),add=TRUE,col='green')
    ## plot(sf::st_geometry(annuli[annuli$R%%2==1 && annuli$cirName==1,]),add=TRUE,col='red')


    ## over lap of annuli and plots
    overlapAll <- suppressWarnings(sf::st_intersection(annuli,turbPlots))

    ## only care about areas within a turbine
    overlap <- overlapAll[as.data.frame(sf::st_drop_geometry(overlapAll))[,turbName]==as.data.frame(sf::st_drop_geometry(overlapAll))[,'cirName'],]


    overlap$areaSearched <- as.numeric(sf::st_area(overlap))


    ## ensure all distances go out to the max distance
    turbAnnuli <- data.frame()
    for(sL in unique(overlap$cirName)){
        thisTurb <- sf::st_drop_geometry(subset(overlap,overlap$cirName==sL))

        thisMaxR <- max(thisTurb$R)
        needR <- (thisMaxR+1):maxDist
        if(thisMaxR<maxDist){

            newTurbR <- thisTurb[rep(1,times=length(needR)),]
            newTurbR$R <- needR
            newTurbR$areaSearched <- 0

            turbAnnuli <- rbind(turbAnnuli,rbind(thisTurb,newTurbR))

        }else{
            turbAnnuli <- rbind(turbAnnuli,thisTurb)
        }#end else if

    }#end for sL

    row.names(turbAnnuli) <- NULL

    ## changes distances to be from the turbine mast edge
    turbAnnuli$distanceFromTurbine <- turbAnnuli$R-mastRad
    ## total area of the annulus
    turbAnnuli$annulusArea <- with(turbAnnuli,R^2-(R-1)^2)*pi
    ## prop area searched
    turbAnnuli$proportionAreaSearched <- with(turbAnnuli,areaSearched/annulusArea)

    ## removes names for internal use
    turbAnnuli[,c('R','cirName')] <- NULL


    return(turbAnnuli)


}#end getProportionAreaSearched function
