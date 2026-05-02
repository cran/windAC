#' @name getProportionAreaSearched
#'
#' @title Create proportion of area searched table from spatial data
#'
#' @description Calculate the proportion of area searched around wind turbine based 
#' on turbine location data and polygons of search area.
#'
#' @param turbinePoints sf data.frame of point geometries and a \code{turbineName} 
#' column indicating the turbine names
#' @param turbineName Character, indicating the variable name for the turbine 
#' names in \code{turbinePoints} and plot names in \code{turbinePlots}
#' @param turbinePlots sf data.frame of polygon geometries indicating the search area around 
#' the turbine points and a \code{turbineName} column indicating the turbine names
#' @param turbineMastRadius Integer of length 1. The radius of the turbine mast
#' @param maxDistance Integer indicating how far from the turbine that searches 
#' occurred
#' @inheritParams sf::st_buffer
#'
#' @details The \code{\link[sf]{sf}} package is used to calculate overlapping 
#' areas between the searched area \code{turbinePlots} and one unit annulus around 
#' the \code{turbinePoints}. The annuli increase out to a distance of 
#' \code{maxDistance}. The \code{nQuadSegs} argument is passed directly to 
#' \code{\link[sf:st_buffer]{sf::st_buffer}}
#' 
#' Caution: the function does some basic checks on the spatial objects but it is 
#' assumed that the points and polygons do not have any boundary, geometry, or 
#' other issues.
#' 
#' @return Data frame of proportion of area searched for each annulus around each 
#' turbine point. \code{distanceFromTurbine} column represents outer radius of 
#' each annulus.
#'
#' @export 
#' 
#' @examples
#' data(turbineSpatial)
#' 
#' propSearch <- getProportionAreaSearched(
#'   turbinePoints = turbineSpatial$turbinePoints,
#'   turbineName = 'turbName',
#'   turbinePlots = turbineSpatial$turbinePlots,
#'   turbineMastRadius = 2,
#'   maxDistance = 10, 
#'   nQuadSegs = 100
#' )
#' 
getProportionAreaSearched <- function(
    turbinePoints, 
    turbineName, 
    turbinePlots, 
    turbineMastRadius, 
    maxDistance, 
    nQuadSegs = 1000
) {
  
  stopifnot(is.character(turbineName))
  stopifnot(length(turbineName) == 1)
  stopifnot(inherits(turbinePoints, 'sf'))
  stopifnot(inherits(turbinePoints, 'data.frame'))
  stopifnot(inherits(turbinePlots, 'sf'))
  stopifnot(inherits(turbinePlots, 'data.frame'))
  stopifnot(turbineName %in% names(turbinePoints))
  stopifnot(turbineName %in% names(turbinePlots))
  stopifnot(is.numeric(maxDistance))
  stopifnot(is.numeric(turbineMastRadius))
  stopifnot(length(maxDistance) == 1)
  stopifnot(length(turbineMastRadius) == 1)
  stopifnot(turbineMastRadius > 0)
  stopifnot(maxDistance > 0)
  stopifnot(nQuadSegs > 0)
  stopifnot(length(nQuadSegs) == 1)
  
  # ensure correct geometry type from inputs
  if (!any(sf::st_geometry_type(turbinePlots) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("'turbinePlots' input must be of sf geometry type 'POLYGON' or 'MULTIPOLYGON'")
  }
  
  if (!any(sf::st_geometry_type(turbinePoints) %in% c("POINT", "MULTIPOINT"))) {
    stop("'turbinePoints' input must be of sf geometry type 'POINT' or 'MULTIPOINT'")
  }
  
  ## Check to see if the data is longitude/latitude data.
  if (isTRUE(sf::st_is_longlat(turbinePoints))) {
    stop(paste("sf::st_is_longlat detects that turbinePoints uses a longitude/latitude coordinate system.",
               "use sf::st_transform to convert to a projected coordinate system."))
  }
  
  if (isTRUE(sf::st_is_longlat(turbinePlots))) {
    stop(paste("sf::st_is_longlat detects that turbinePlots uses a longitude/latitude coordinate system.",
               "use sf::st_transform to convert to a projected coordinate system."))
  }
  
  result <- purrr::map(
    .x = seq_len(nrow(turbinePlots)), 
    .f = function(.rowIndex) {
      thisPoly <- turbinePlots[.rowIndex, ]
      thisSl <- thisPoly[[turbineName]]
      thisPoint <- turbinePoints[turbinePoints[[turbineName]] == thisSl, ]
      
      thisPropSearch <- getProportionAreaSearchedForTurbine(
        tp = thisPoint, 
        sp = thisPoly, 
        maxR = maxDistance,
        turbineMastRadius = turbineMastRadius,
        turbineName = turbineName, 
        nQuadSegs = nQuadSegs
      )
      
      return(thisPropSearch)
    },
    .progress = "Computing the proportion of area searched table."
  ) %>%
    purrr::list_rbind()
  
  
  result$turbineMastRadius <- turbineMastRadius
  
  return(result)
  
}



#' @name getProportionAreaSearchedForTurbine
#' @noRd
getProportionAreaSearchedForTurbine <- function(
    tp, sp, maxR, turbineMastRadius, turbineName, nQuadSegs = 1000
){
  
  stopifnot(inherits(sp, 'sf'))
  stopifnot(inherits(sp, 'data.frame'))
  stopifnot(inherits(tp, 'sf'))
  stopifnot(inherits(tp, 'data.frame'))
  stopifnot(is.numeric(maxR))
  stopifnot(is.numeric(turbineMastRadius))

  if (!identical(sf::st_crs(tp), sf::st_crs(sp))) {
    stop("CRS mismatch between 'tp' (points) and 'sp' (polygons). ",
         "Transform one to match the other (e.g., sf::st_transform).")
  }
  
  rMax <- as.integer(round(maxR + turbineMastRadius))
  stopifnot(rMax >= 1)
  r <- seq_len(rMax)
  
  details <- sf::st_drop_geometry(sp)
  
  # we only care about geometry
  tp <- tp %>% dplyr::select('geometry')
  
  # replicate for r number of annuli around the turbine
  tps <- tp[rep(1, length(r)), ] 
  
  # create discs
  outer <- sf::st_buffer(tps, dist = r, nQuadSegs = nQuadSegs)
  inner <- sf::st_buffer(tps, dist = c(0, r[-length(r)]), nQuadSegs = nQuadSegs)
  
  # combine to get the rings 
  annuli <- sf::st_sfc(
    purrr::map2(sf::st_geometry(outer), sf::st_geometry(inner), sf::st_difference),
    crs = sf::st_crs(tp)
  )
  # to sf data.frame
  annuli <- sf::st_sf(distanceFromCenter = r, geometry = annuli)   
  
  # compute annuli area
  annulusArea <- annuli %>% dplyr::mutate(
    annulusArea = as.numeric(sf::st_area(.data$geometry))
    ) %>% sf::st_drop_geometry()
  
  # intersect rings with polygon
  # supress warning: attribute variables are assumed to be spatially constant 
  #   throughout all geometries which is of no concern for us
  poly <- suppressWarnings(
    sf::st_intersection(annuli, sp) 
  )
  
  # sum area across rings
  poly <- poly %>%
    dplyr::mutate(areaSearched = as.numeric(sf::st_area(.data$geometry))) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data$distanceFromCenter) %>%
    dplyr::summarise(areaSearched = sum(.data$areaSearched), .groups = "drop")
  
  
  result <- annulusArea %>% dplyr::left_join(
    poly, by = "distanceFromCenter"
  ) %>% dplyr::mutate(
    areaSearched = tidyr::replace_na(.data$areaSearched, 0),
    proportionAreaSearched = .data$areaSearched / .data$annulusArea
  ) %>% dplyr::arrange(
    .data$distanceFromCenter
  )
  
  # we don't want the area from "inside" the turbine mast
  result$distanceFromTurbine = result$distanceFromCenter - turbineMastRadius
  result <- result[result$distanceFromTurbine > 0, ]

  # add the details back on
  result <- result %>% dplyr::mutate(!!turbineName := unique(sp[[turbineName]])) %>%
    dplyr::left_join(details, by = turbineName)

  return(result)
  
}