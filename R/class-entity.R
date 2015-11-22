#' The SObjects class and constructor function
#'
#' SObjects: a Class for Spatial Entities (Objects)
#'
#' A class to store spatial entities, such as trees, buildings and so on,
#' along with the observed window, the area for which the set of
#' entities is exhaustive. It is assumed that for all points in the 
#' observed window where there are no objects, the space is known to be empty.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{observations}}{object of a subclass of \link[sp]{Spatial}}
#'    \item{\code{window}}{object of class \link{SExtentOrNULL-class}}
#'  }
#'
#' @usage SObjects(observations, window)
#' @param observations object of one of the sublasses of \link{Spatial}
#' @param window object of class \link{SExtent} or \link{Spatial}
#' @return object of class \link{SObjects-class}

#' @name SObjects-class
#' @rdname SObjects-class
#' @aliases SObjects SObjects-class [,SObjects-method [[,SObjects,ANY,missing-method [[<-,SObjects,ANY,missing-method $,SObjects-method
#' @exportClass SObjects
#' @author Edzer Pebesma
#' @seealso \link{SField}
#' @note If no window is supplied, the window is set to the collection of features, and a warning is issued. Support is assumed to be that of the features (points, polygons, lines, grid cells) in the \code{observations} object.
#'
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' m = SObjects(meuse, meuse.area)
setClass("SObjects",
	slots = c(observations = "Spatial", window = "SExtentOrNULL"),
	validity = function(object) {
		# check all features are inside window:
		if (!is.null(object@window) &&
				any(is.na(over(object@observations, geometry(object@window@area)))))
			stop("one or more features are outside the observation window")
		return(TRUE)
	}
)
SObjects = function(observations, window) {
	if (missing(window))
		new("SObjects", observations)
	else if (is(window, "Spatial"))
		new("SObjects", observations, SExtent(window))
	else
		new("SObjects", observations, window)
}
#' SObjects initialize function
#'
#' initializes (creates) SObjects objects
#'
#' @param .Object object to initialize
#' @param observations object of one of the sublasses of \link{Spatial}
#' @param window object of class \link{SExtent}
#'
#' @return object of class \link{SObjects-class}
#' 
#' @seealso \link{SField}
#' @note this function is not called directly
#' 
#' @export
#' @export SObjects
#' @docType methods
#' @rdname initialize-SObjects-methods
setMethod("initialize", "SObjects", function(.Object, observations, window) {
	.Object <- callNextMethod()
	if (missing(window)) {
		warning("window set to the observation data features", call. = FALSE)
		window = NULL
	} 
	if (!is.null(window) && any(is.na(over(observations, window@area))))
		warning("some observations are outside the window", call. = FALSE)
	.Object@observations = observations
	.Object@window = window
	.Object
})
