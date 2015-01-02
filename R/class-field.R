#' The SpatialField class and constructor function
#'
#' SpatialField: a Class for Spatial Fields (Objects)
#'
#' A class to store spatial fields, such as temperatures, elevation,
#' land use, and so on, which have point support (footprint), along with the
#' domain for which the set observation are valid e.g. to make interpolations.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{observations}}{object of a subclass of \link[sp]{Spatial}}
#'    \item{\code{domain}}{object of class \link{Window}}
#'    \item{\code{observations_equal_domain}}{logical; are the \code{observations} identical to the \code{domain}?}
#'  }
#'
#' @usage SpatialField(...)
#' @param ... named arguments, according to the following list:
#'  \describe{
#'    \item{observations}{object of one of the sublasses of \link[sp]{Spatial}}
#'    \item{domain}{object of class \link{Window}, or of a subclass of \link[sp]{Spatial} (typically: \link[sp]{SpatialPolygons}, \link[sp]{SpatialPixels} or \link[sp]{SpatialGrid})}
#'    \item{observations_equal_domain}{logical; indicates whether the
#'    \code{observations} are identical to the domain}
#'  }
#'
#' @return object of class \link{SpatialField-class}

#' @name SpatialField-class
#' @rdname SpatialField-class
#' @aliases $,SpatialField-method spplot,SpatialField-method [,SpatialField-method [[,SpatialField,ANY,missing-method [[<-,SpatialField,ANY,missing-method SpatialField-class over,SpatialField,SpatialField-method over,SpatialField,SpatialAggregation-method 
#' @exportClass SpatialField
#' @export SpatialField
#' @author Edzer Pebesma
#' @seealso \link{SpatialEntities}
#' @note If no domain is supplied, the domain is set to the collection of features. A warning is issued if one or more of the features are (completely) outside the domain.
#'
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' m = SpatialField(meuse, meuse.area)

SpatialField = setClass("SpatialField",
	slots = c(observations = "Spatial", domain = "Window", 
		observations_equal_domain = "logical"))

##############################################################
#' SpatialField initialize function
#'
#' initializes (creates) SpatialField objects
#'
#' @param .Object (ignore)
#' @param observations object of one of the sublasses of \link[sp]{Spatial}
#' @param domain object of class \link{Window}, or subclass of \link[sp]{Spatial}
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @seealso \link{SpatialEntities}
#' 
#' @export
#' @docType methods
#' @rdname initialize-SpatialField-methods
setMethod("initialize", "SpatialField", function(.Object, observations, domain) {
	.Object <- callNextMethod()
	# stopifnot("data" %in% slotNames(sp)) # need attribute values
	if (missing(domain)) {
		domain = Window(observations)
		.Object@observations_equal_domain = TRUE
		mss("domain was set to the observations' geometry")
	} else {
		.Object@observations_equal_domain = FALSE
		if (is(domain, "Spatial"))
			domain = Window(domain)
		else stopifnot(is(domain, "Window"))
#		check all features are inside domain here?
		if (any(is.na(over(observations, domain@area))))
			not_meaningful("having observations outside the domain")
	}
	.Object@observations = observations
	.Object@domain = domain
	.Object
})
