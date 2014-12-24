#' The Support class and constructor function: spatial support
#'
#' Class for defining support of observations or predictions
#'
#' (Spatial) Support is the physical size (length, area, volume) corresponding
#' to measurements, or predictions. This class informs objects of the subclasses
#' of \link[sp]{Spatial} about their support.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{what}}{character; one of: \code{"point"}, \code{"feature"}, \code{"function"}}
#'    \item{\code{fn}}{function, (optionally) computing support}
#'  }
#'
#' @usage Support(what, fn)
#' @param what character: one of: \code{"point"}, \code{"feature"}, \code{"function"}; if omitted, \code{"features"} is chosen an a warning is given
#' @param fn function computing the support
#' @return object of class \link{Support}
#'
#' @note a function computing the support should provide as input value the feature (point, line, or polygon) for which a support is to be computed.
#' @name Support
#' @rdname Support
#' @aliases Support-class
#' @exportClass Support
#' @export Support
#' @author Edzer Pebesma
setClass("Support", slots = c(what = "character", fn = "function"),
	validity = function(object) {
		stopifnot(all(object@what %in% c("point", "feature", "function")))
		return(TRUE)
	})
#setMethod("initialize", "Support", function(.Object, what, fn) {
Support = function(what, fn) {
	if (missing(what)) {
		if (!missing(fn))
			what = "function"
		else {
			warning("Support(): assuming the default, which is feature",
				call. = FALSE)
			what = "feature"
		}
	}
	if (what == "function") {
		if (missing(fn))
			stop("please provide a function")
	}
	if (missing(fn))
		fn = function(x) x # identity
	new("Support", what = what, fn = fn)
}
