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
#' @aliases Window-class WindowOrNULL-class initialize,Window-method
#' @exportClass Window WindowOrNULL
#' @export Window
#' @author Edzer Pebesma
Window = setClass("Window", slots = c(area = "Spatial"))
setMethod("initialize", "Window", 
	function(.Object, area) {
		.Object <- callNextMethod()
		.Object@area = area
		.Object
	}
)
setClassUnion("WindowOrNULL", c("Window", "NULL"))
