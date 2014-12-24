#' The SpatialEntities class and constructor function
#'
#' SpatialEntities: a Class for Spatial Entities (Objects)
#'
#' A class to store spatial entities, such as trees, buildings and so on,
#' along with their support (or footprint) and window for which the set of
#' entities is exhaustive
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{sp}}{object of a subclass of \link[sp]{Spatial}}
#'    \item{\code{support}}{object of class \link{Support}}
#'    \item{\code{window}}{object of class \link{Window}}
#'  }
#'
#' @usage SpatialEntities(...)
#' @param ... named arguments from the following list:
#'  \describe{
#'    \item{\code{sp}}{object of one of the sublasses of \link{Spatial}}
#'    \item{\code{support}}{object of class \link{Support}, or character}
#'    \item{\code{window}}{object of class \link{Window} or \link{Spatial}}
#'  }
#' @return object of class \link{SpatialEntities-class}

#' @name SpatialEntities-class
#' @rdname SpatialEntities-class
#' @aliases SpatialEntities-class
#' @exportClass SpatialEntities
#' @author Edzer Pebesma
#' @seealso \link{SpatialField}
#' @note If no window is supplied, the window is set to the collection of features, and a warning is issued. If no support is specified, the support is set to that of the features (points, polygons, lines, grid cells) in the \code{sp} object, and a warning is issued.
#'
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' m = SpatialEntities(meuse, "point", meuse.area)
SpatialEntities = setClass("SpatialEntities",
	slots = c(sp = "Spatial", support = "Support", window = "Window"),
	validity = function(object) {
		# check all features are inside window:
		stopifnot(any(is.na(over(object@sp, geometry(object@window)))))
		return(TRUE)
	}
)
#' SpatialEntities initialize function
#'
#' initializes (creates) SpatialEntities objects
#'
#' @param .Object object to initialize
#' @param sp object of one of the sublasses of \link{Spatial}
#' @param support object of class \link{Support}
#' @param window object of class \link{Window}
#'
#' @return object of class \link{SpatialEntities-class}
#' 
#' @seealso \link{SpatialField}
#' @note this function is not called directly
#' 
#' @export
#' @export SpatialEntities
#' @docType methods
#' @rdname initialize-SpatialEntities-methods
setMethod("initialize", "SpatialEntities", function(.Object, 
			sp, support, window) {
	.Object <- callNextMethod()
	if (missing(support))
		support = Support()
	else if (is.character(support))
		support = Support(support)
	if (missing(window)) {
		warning("window set to the observation data features", call. = FALSE)
		window = Window(sp)
	} else { # window specified
		if (is(window, "Spatial"))
			window = Window(window)
		if (any(is.na(over(sp, window@sp))))
			warning("some observations are outside the window", call. = FALSE)
	}
	.Object@sp = sp
	.Object@support = support
	.Object@window = window
	.Object
})
