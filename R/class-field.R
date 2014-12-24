#' The SpatialField class and constructor function
#'
#' SpatialField: a Class for Spatial Fields (Objects)
#'
#' A class to store spatial fields, such as temperatures, elevation,
#' land use, and so on, along with their support (or footprint) and 
#' domain for which the set observation are valid and interpolation makes sense.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{sp}}{object of a subclass of \link[sp]{Spatial}}
#'    \item{\code{support}}{object of class \link{Support}}
#'    \item{\code{domain}}{object of class \link{Window}}
#'  }
#'
#' @usage SpatialField(...)
#' @param ... named arguments, according to the following list:
#'  \describe{
#'    \item{sp}{object of one of the sublasses of \link{Spatial}}
#'    \item{support}{object of class \link{Support}, or one of \code{"point"}, \code{"feature"}}
#'    \item{domain}{object of class \link{Window}, or of a subclass of \link[sp]{Spatial} (typically: \link[sp]{SpatialPolygons}, \link[sp]{SpatialPixels} or \link[sp]{SpatialGrid})}
#'  }
#'
#' @return object of class \link{SpatialField-class}

#' @name SpatialField-class
#' @rdname SpatialField-class
#' @aliases $,SpatialField-method spplot,SpatialField-method [,SpatialField-method SpatialField-class
#' @exportClass SpatialField
#' @author Edzer Pebesma
#' @seealso \link{SpatialEntities}
#' @note If no domain is supplied, the domain is set to the collection of features. If no support is specified, the support is set to that of the features (points, polygons, lines, grid cells) in the \code{sp} object, and a warning is issued. A warning is issued if one or more of the features are (completely) outside the domain.
#'
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' m = SpatialEntities(meuse, "point", meuse.area)

SpatialField = setClass("SpatialField",
	slots = c(sp = "Spatial", support = "Support", domain = "Window"))

##############################################################
#' SpatialField initialize function
#'
#' initializes (creates) SpatialField objects
#'
#' @param .Object (ignore)
#' @param sp object of one of the sublasses of \link[sp]{Spatial}
#' @param support object of class \link{Support}, or one of "point" and "feature"
#' @param domain object of class \link{Window}, or subclass of \link[sp]{Spatial}
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @seealso \link{SpatialEntities}
#' 
#' @export
#' @docType methods
#' @rdname initialize-SpatialField-methods
setMethod("initialize", "SpatialField", function(.Object, sp, support, domain) {
	.Object <- callNextMethod()
	stopifnot("data" %in% slotNames(sp)) # need attribute values
	if (missing(support))
		support = Support()
	else if (is.character(support))
		support = Support(support)
	if (missing(domain))
		domain = Window(sp)
	else {
		if (is(domain, "Spatial"))
			domain = Window(domain)
#		check all features are inside domain here?
		if (any(is.na(over(sp, domain@sp))))
			warning("some observations are outside the domain", call. = FALSE)
	}
	.Object@sp = sp
	.Object@support = support
	.Object@domain = domain
	.Object
})
#' compute area of a Spatial* object
#'
#' compute the area of an object of one of the subclasses of \link[sp]{Spatial}
#'
#' area of gridded or polygon objects deriving from \link[sp]{Spatial}, zero
#' for other classes (points, lines)
#'
#' @param x object of a subclass of \link[sp]{Spatial}
#' @return the area of the features (grid cells or polygons) in the object
#' @export
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' getArea(meuse)
#' getArea(meuse.area)
getArea = function(x) {
	stopifnot(is(x, "Spatial"))
	if (gridded(x))
		return(areaSpatialGrid(x))
	getAreaSP = function(x) { # copied from sp/R/spsample.R:
    		getAreaPolygons = function(x) {
        		holes = unlist(lapply(x@Polygons, function(x) x@hole))
        		areas = unlist(lapply(x@Polygons, function(x) x@area))
        		area = ifelse(holes, -1, 1) * areas
        		area
    		}
    		sum(unlist(lapply(x@polygons, getAreaPolygons)))
	}
	if (is(x, "SpatialPolygons"))
		return(getAreaSP(x))
	# lines, points:
	return(0.0)
}
#' assess whether function has complete coverage over the domain
#' 
#' fields with point support, defined for all areas in the domain are completely known; this function verifies that this is the case
#' 
#' @param x object of class \link{SpatialField-class}
#' @return logical; TRUE if support is "point" and area size of the observations is identical to that of the domain; this implies we have a coverage with knowledge of all points in the domain.
#' @note what the function does is not sufficient: identical area size guaranties not that areas are identical
#' @export
is.complete = function(x) {
	stopifnot(is(x, "SpatialField"))
	x@support@what == "point" && 
		(gridded(x@domain@sp) || is(x@domain@sp, "SpatialPolygons")) &&
		isTRUE(all.equal(getArea(x@sp), getArea(x@domain@sp))) # allows for numerical fuzz
}
