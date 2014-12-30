#' The Window class and Constructor Function: Domain and Observed Window
#'
#' Class for defining domain of a field, or observed window for entities
#'
#' In practice, spatial fields are defined for a limited domain. Spatial
#' entities can form an exhaustive set for an area called the observed
#' window. For both, the Window class is used to specify this area.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{area}}{object of a subclass of \link[sp]{Spatial}}
#'  }
#'
#' @usage Window(...)
#' @param ... named parameter, containing 
#'  \describe{
#'    \item{\code{area}}{object of a subclass of \link[sp]{Spatial}}
#'  }
#' @return object of class \link{Window}
#'
#' @note if \code{area} is not of (a subclass of) class \link[sp]{SpatialPolygons}, \link[sp]{SpatialPixels}, or \link[sp]{SpatialGrid}, an error results.
#' @name Window
#' @rdname Window
#' @aliases Window-class
#' @exportClass Window
#' @export Window
#' @author Edzer Pebesma
Window = setClass("Window", slots = c(area = "Spatial"))
#' initializes (creates) Window objects
#'
#' @param .Object (ignore)
#' @param area object of one of the sublasses of \link[sp]{Spatial}
#'
#' @return object of class \link{Window-class}
#' @note this function should not be called directly, use \link{Window} instead
#' 
#' @export
#' @docType methods
#' @rdname initialize-Window-methods
setMethod("initialize", "Window", function(.Object, area) {
	.Object <- callNextMethod()
	if (missing(area))
		stop("area argument needs to be supplied")
	stopifnot(is(area, "Spatial")) # should be automatic
	if (!(gridded(area) || is(area, "SpatialPolygons")))
		stop("Window object needs to reflect an area, as grid or polygons")
	.Object@area = area
	.Object
})
