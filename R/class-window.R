#' The SExtent class and Constructor Function: Domain and Observed SExtent
#'
#' Class for defining domain of a field, or observed window for entities
#'
#' In practice, spatial fields are defined for a limited domain. Spatial
#' entities can form an exhaustive set for an area called the observed
#' window. For both, the SExtent class is used to specify this area.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{area}}{object of a subclass of \link[sp]{Spatial}}
#'  }
#'
#' @usage SExtent(...)
#' @param ... named parameter, containing 
#'  \describe{
#'    \item{\code{area}}{object of a subclass of \link[sp]{Spatial}}
#'  }
#' @return object of class \link{SExtent}
#'
#' @note if \code{area} is not of (a subclass of) class \link[sp]{SpatialPolygons}, \link[sp]{SpatialPixels}, or \link[sp]{SpatialGrid}, an error results.
#' @name SExtent
#' @rdname SExtent
#' @aliases SExtent-class SExtentOrNULL-class initialize,SExtent-method
#' @exportClass SExtent SExtentOrNULL
#' @export SExtent
#' @author Edzer Pebesma
SExtent = setClass("SExtent", slots = c(area = "Spatial"))
setMethod("initialize", "SExtent", 
	function(.Object, area) {
		.Object <- callNextMethod()
		.Object@area = area
		.Object
	}
)
setClassUnion("SExtentOrNULL", c("SExtent", "NULL"))
