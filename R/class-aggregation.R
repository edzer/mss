#' The SpatialAggregation class and constructor function
#'
#' SpatialAggregation: a Class for Spatial Aggregations (Area data)
#'
#' A class to store spatial aggregations, such as population densities or grid maps;
#' spatial aggregations are (single) values that have reference to an area, e.g.
#' the maximum, average, or sum over an area.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{sp}}{object of a subclass of \link[sp]{Spatial}; needs to be
#'    anything but \code{SpatialPoints}}
#'  }
#'
#' @usage SpatialAggregation(...)
#' @param ... named arguments, according to the following list:
#'  \describe{
#'    \item{sp}{object of one of the sublasses of \link{Spatial}; see above}
#'  }
#'
#' @return object of class \link{SpatialAggregation-class}

#' @name SpatialAggregation-class
#' @rdname SpatialAggregation-class
#' @aliases $,SpatialAggregation-method spplot,SpatialAggregation-method [,SpatialAggregation-method SpatialAggregation-class
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
SpatialAggregation = setClass("SpatialAggregation", slots = c(sp = "Spatial"),
	validity = function(object) { 
		stopifnot(!(class(object@sp) %in% c("SpatialPoints", "SpatialPointsDataFrame")))
		return(TRUE)
	}
)
