#' The SpatialEntities class and constructor function
#'
#' SpatialEntities: a Class for Spatial Entities (Objects)
#'
#' A class to store spatial entities, such as trees, buildings and so on,
#' along with the observed window, the area for which the set of
#' entities is exhaustive. It is assumed that for all points in the 
#' observed window where there are no objects, the space is known to be empty.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{observations}}{object of a subclass of \link[sp]{Spatial}}
#'    \item{\code{window}}{object of class \link{WindowOrNULL-class}}
#'  }
#'
#' @usage SpatialEntities(observations, window)
#' @param observations object of one of the sublasses of \link{Spatial}
#' @param window object of class \link{Window} or \link{Spatial}
#' @return object of class \link{SpatialEntities-class}

#' @name SpatialEntities-class
#' @rdname SpatialEntities-class
#' @aliases SpatialEntities SpatialEntities-class [,SpatialEntities-method [[,SpatialEntities,ANY,missing-method [[<-,SpatialEntities,ANY,missing-method $,SpatialEntities-method
#' @exportClass SpatialEntities
#' @author Edzer Pebesma
#' @seealso \link{SpatialField}
#' @note If no window is supplied, the window is set to the collection of features, and a warning is issued. Support is assumed to be that of the features (points, polygons, lines, grid cells) in the \code{observations} object.
#'
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' m = SpatialEntities(meuse, meuse.area)
setClass("SpatialEntities",
	slots = c(observations = "Spatial", window = "WindowOrNULL"),
	validity = function(object) {
		# check all features are inside window:
		if (!is.null(object@window) &&
			any(is.na(over(object@observations, geometry(object@window@area)))))
			stop("one or more features are outside the observation window")
		return(TRUE)
	}
)
SpatialEntities = function(observations, window) {
	if (missing(window))
		new("SpatialEntities", observations, NULL)
	else if (is(window, "Spatial"))
		new("SpatialEntities", observations, Window(window))
	else
		new("SpatialEntities", observations, window)
}
#' SpatialEntities initialize function
#'
#' initializes (creates) SpatialEntities objects
#'
#' @param .Object object to initialize
#' @param observations object of one of the sublasses of \link{Spatial}
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
setMethod("initialize", "SpatialEntities", function(.Object, observations, window) {
	.Object <- callNextMethod()
	if (missing(window)) {
		warning("window set to the observation data features", call. = FALSE)
		window = NULL
	} else # window specified
		if (any(is.na(over(observations, window@area))))
			warning("some observations are outside the window", call. = FALSE)
	.Object@observations = observations
	.Object@window = window
	.Object
})
