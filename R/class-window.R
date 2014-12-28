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
#'    \item{\code{sp}}{object of a subclass of \link[sp]{Spatial}}
#'  }
#'
#' @usage Window(...)
#' @param ... named parameter, containing 
#'  \describe{
#'    \item{\code{sp}}{object of a subclass of \link[sp]{Spatial}}
#'  }
#' @return object of class \link{Window}
#'
#' @note if \code{sp} is not of (a subclass of) class \link[sp]{SpatialPolygons}, \link[sp]{SpatialPixels}, or \link[sp]{SpatialGrid}, an error results.
#' @name Window
#' @rdname Window
#' @aliases Window-class
#' @exportClass Window
#' @export Window
#' @author Edzer Pebesma
Window = setClass("Window", slots = c(sp = "Spatial"))
#' initializes (creates) Window objects
#'
#' @param .Object (ignore)
#' @param sp object of one of the sublasses of \link[sp]{Spatial}
#'
#' @return object of class \link{Window-class}
#' @note this function should not be called directly, use \link{Window} instead
#' 
#' @export
#' @docType methods
#' @rdname initialize-Window-methods
setMethod("initialize", "Window", function(.Object, sp) {
	.Object <- callNextMethod()
	if (missing(sp))
		stop("sp argument needs to be supplied")
	stopifnot(is(sp, "Spatial")) # should be automatic
	if (!(gridded(sp) || is(sp, "SpatialPolygons")))
		stop("Window object needs to reflect an area, as grid or polygons")
	.Object@sp = sp
	.Object
})
