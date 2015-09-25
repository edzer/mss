#' The SpatialField class and constructor function
#'
#' SpatialField: a Class for Spatial Fields (Objects)
#'
#' A class to store spatial fields, such as temperatures, elevation,
#' land use, and so on, which have point support (footprint), along with the
#' domain for which the set observation are valid e.g. to make interpolations.
#'
#' Fields are representations of continuous phenomena: at every point in continuous
#' space, a value exists. Observations on fields are, by necessity, discrete and
#' countable when they refer to points, but may also consist of areas in which case
#' an infinite number of points with constant value in the area is represented.
#'
#' SpatialField objects can be generated from points, lines, polygons or grids. For
#' all classes except points, the attribute values are assumed to be constant and
#' identical for all points along the line (lines), or over the area (polygons, grids).
#'
#' The domain of a SpatialField object is the area for which the observations are
#' considered relevant, e.g. by affording interpolation or aggregation.
#'
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{observations}:}{object of a subclass of \link[sp]{Spatial}}
#'    \item{\code{domain}:}{object of class \link{WindowOrNULL-class}}
#'    \item{\code{cellsArePoints}}{logical; do grid cell values reflect point values at the grid cell centre (TRUE) or constant point values throughout the whole grid cell (FALSE)?}
#'  }
#'
#' @usage SpatialField(observations, domain, cellsArePoints = NA)
#' @param observations object of one of the sublasses of \link[sp]{Spatial}
#' @param domain object of class \link{Window}, or of a subclass of \link[sp]{Spatial}; if missing, it is assumed to be identical to \code{observations}
#' @param cellsArePoints logical; do grid cell values reflect point values at the grid cell centre (TRUE) or constant point values throughout the whole grid cell (FALSE)?
#' @return object of class \link{SpatialField-class}

#' @name SpatialField-class
#' @rdname SpatialField-class
#' @aliases SpatialField SpatialField-class $,SpatialField-method spplot,SpatialField-method [,SpatialField-method [[,SpatialField,ANY,missing-method [[<-,SpatialField,ANY,missing-method SpatialField-class over,SpatialField,SpatialField-method over,SpatialField,SpatialAggregation-method 
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

setClass("SpatialField",
	slots = c(observations = "Spatial", domain = "WindowOrNULL", cellsArePoints = "logical"))

SpatialField = function(observations, domain, cellsArePoints = NA) {
	if (missing(domain))
		new("SpatialField", observations, cellsArePoints = cellsArePoints)
	else if (is(domain, "Spatial"))
		new("SpatialField", observations, Window(domain), cellsArePoints = cellsArePoints)
	else
		new("SpatialField", observations, domain, cellsArePoints = cellsArePoints)
}

##############################################################
#' SpatialField initialize function
#'
#' initializes (creates) SpatialField objects
#'
#' @param .Object (ignore)
#' @param observations object of one of the sublasses of \link[sp]{Spatial}
#' @param domain object of class \link{Window}, or subclass of \link[sp]{Spatial}
#' @param cellsArePoints logical; are grid cells to be taken as point (support) values?
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @seealso \link{SpatialEntities}
#' 
#' @export
#' @docType methods
#' @rdname initialize-SpatialField-methods
setMethod("initialize", "SpatialField", 
	function(.Object, observations, domain, cellsArePoints = NA) {
		.Object <- callNextMethod()
		if (missing(domain)) {
			domain = NULL
			mss("missing domain: assuming it equals the observations' geometry")
		} else {
#			check all features are inside domain here?
			if (any(is.na(over(observations, domain@area))))
				not_meaningful("having observations outside the domain")
		}
		#if (! ("data" %in% slotNames(observations))) # need attribute values
		#	stop("Spatial* object needs to have attributes")
		if (gridded(observations) && is.na(cellsArePoints))
			stop("for gridded observations, cellsArePoints needs to be specified")
		# new("SpatialField", observations, domain, cellsArePoints)
		.Object@cellsArePoints = cellsArePoints
		.Object@observations = observations
		.Object@domain = domain
		.Object
	}
)
