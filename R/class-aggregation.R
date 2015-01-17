#' The SpatialAggregation class and constructor function
#'
#' SpatialAggregation: a Class for Spatial Aggregations (Area data)
#'
#' A class to store spatial aggregations, such as population densities or grid maps;
#' spatial aggregations are (single) values that have reference to an area, e.g.
#' the maximum, average, or sum over an area.
#'
#' Aggregation values are values that refer to an area (polygon, grid cell), or line,
#' as a whole: the values cannot be interpreted as the value at any given point in
#' an area or on a line; these individual values may be recovered by estimation
#' (area-to-point interpolation), but in principle got lost by aggregation. Aggregation
#' may be integral to observation (e.g., remote sensing pixels refer to values aggregated
#' over an area similar to the pixel), or a result from spatially aggregating data, 
#' e.g. from using \link{aggregate}.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{observations}}{object of a subclass of \link[sp]{Spatial}; needs to be
#'    anything but \code{SpatialPoints}}
#'  }
#'
#' @usage SpatialAggregation(observations)
#' @param observations object of one of the sublasses of \link{Spatial},
#'    anything but \code{SpatialPoints}
#'
#' @return object of class \link{SpatialAggregation-class}

#' @name SpatialAggregation-class
#' @rdname SpatialAggregation-class
#' @aliases $,SpatialAggregation-method spplot,SpatialAggregation-method [,SpatialAggregation-method [[,SpatialAggregation,ANY,missing-method [[<-,SpatialAggregation,ANY,missing-method SpatialAggregation-class SpatialAggregation
#' @exportClass SpatialAggregation
#' @export SpatialAggregation
#' @author Edzer Pebesma
#' @seealso \link{SpatialEntities}, \link{SpatialField}
#' @note The domain for spatial aggregations is identical to the geometry covered by its geometry. The support of the aggregation is identical to its features (grid cells/pixels, polygons). 
#'
#' @examples
#' library(maptools)
#' fname = system.file("shapes/sids.shp", package="maptools")[1]
#' nc = readShapePoly(fname, proj4string=CRS("+proj=longlat +datum=NAD27"))
#' sa = SpatialAggregation(nc)
#' # has aggregated quantities, except for CNTY_, CNTY_ID, FIPS, FIPSNO, NAME
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' try(x <- SpatialAggregation(meuse))
#' plot(SpatialAggregation(meuse.area))
setClass("SpatialAggregation", slots = c(observations = "Spatial"),
	validity = function(object) { 
		if (class(object@observations) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
			stop("SpatialAggregation need to have as geometry: lines, grid, or polygons")
		return(TRUE)
	}
)
SpatialAggregation = function(observations) {
	new("SpatialAggregation", observations = observations)
}
