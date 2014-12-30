#' Method interpolate
#' @name interpolate
#' @rdname interpolate-methods
#' @exportMethod interpolate
if (!isGeneric("interpolate"))
	setGeneric("interpolate", function(formula, data, newdata, ...)
		standardGeneric("interpolate"))
#' SpatialField interpolation method
#'
#' interpolate SpatialField data
#'
#' @param formula specifing which variable should be interpolated, see also \link[gstat]{krige}
#' @param data object of class \link{SpatialField-class}
#' @param newdata target locations; if missing, points are chosen from the domain of \code{data}
#' @param ncells in case no newdata is provided and point support interpolations are computed over a polygon area, the approximate number of grid cells (default 5000)
#' @param ... passed on to \link[gstat]{krige}
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @seealso \link[gstat]{krige}
#' 
#' @export
#' @docType methods
#' @rdname interpolate-methods
setMethod("interpolate", c("formula", "SpatialField", "SpatialField"),
	function(formula, data, newdata, ...) {
		if (any(is.na(over(newdata@observations, data@domain@area))))
			not_meaningful("interpolation outside the observation domain")
		if (full.coverage(data)) {
			stopifnot(is(newdata@observations, "SpatialPoints"))
			SpatialField(addAttrToGeom(newdata@observations, 
					over(newdata@observations, data@observations), TRUE), 
				data@domain)
		} else { # point kriging
			if (!requireNamespace("gstat", quietly = TRUE))
				stop("package gstat required")
			SpatialField(gstat::krige(formula, data@observations, 
				newdata@observations, ...), data@domain)
		}
	}
)
setMethod("interpolate", c("formula", "SpatialField", "missing"),
	function(formula, data, newdata, ..., ncells = 5000) {
		newdata = data@domain@area
		if (!gridded(newdata)) {
			newdata = spsample(newdata, ncells, "regular")
			gridded(newdata) = TRUE
		}
		interpolate(formula, data, SpatialField(newdata), ...)
})
setMethod("interpolate", c("formula", "SpatialField", "SpatialAggregation"),
  	function(formula, data, newdata, ..., ncells = 5000) {
		if (any(is.na(over(newdata@observations, data@domain@area))))
			not_meaningful("interpolation outside the data domain")
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		if (gridded(newdata@observations)) {
			block = gridparameters(newdata@observations)$cellsize
			SpatialAggregation(gstat::krige(formula, data@observations, 
				newdata@observations, block = block, ...))
		} else
			SpatialAggregation(gstat::krige(formula, data@observations, 
				newdata@observations, ...))
})
setMethod("interpolate", c("formula", "SpatialAggregation", "SpatialField"),
	function(formula, data, newdata, ...) {
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		var1.pred = gstat::krige0(formula, data@observations, newdata@observations,
			vgm_area, ...)
		nd = addAttrToGeom(newdata@observations, data.frame(var1.pred), FALSE)
		SpatialField(nd, domain = newdata@domain)
	}
)
setMethod("interpolate", c("formula", "SpatialAggregation", "SpatialAggregation"),
	function(formula, data, newdata, ...) {
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		var1.pred = gstat::krige0(formula, data@observations, newdata@observations,
			vgm_area, ...)
		newdata = addAttrToGeom(newdata@observations, data.frame(var1.pred), FALSE)
		SpatialAggregation(newdata)
	}
)
setMethod("spplot", "SpatialField", function(obj,...) spplot(obj@observations, ...))
setMethod("$", "SpatialField", function(x, name) x@observations[[name]])
setMethod("[", "SpatialField", 
	function(x, i, j, ..., drop = TRUE) x@observations[i, j, ..., drop = drop])
setMethod("$", "SpatialAggregation", function(x, name) x@observations[[name]])
setMethod("spplot", "SpatialAggregation", function(obj,...) spplot(obj@observations, ...))
setMethod("[", "SpatialAggregation", 
	function(x, i, j, ..., drop = TRUE) x@observations[i, j, ..., drop = drop])
double_bracket = function(x, i, j, ...) {
	if (!("data" %in% slotNames(x@observations)))
		stop("no [[ method for object without attributes")
	x@observations@data[[i]]
}
double_bracket_repl = function(x, i, j, value) {
	if (!("data" %in% slotNames(x@observations)))
		stop("no [[ method for object without attributes")
	if (is.character(i) && 
			any(!is.na(match(i, dimnames(coordinates(x@observations))[[2]]))))
		stop(paste(i, "is already present as a coordinate name!"))
	x@observations@data[[i]] <- value
	x
}
setMethod("[[", c("SpatialField", "ANY", "missing"), double_bracket)
setMethod("[[", c("SpatialEntities", "ANY", "missing"), double_bracket)
setMethod("[[", c("SpatialAggregation", "ANY", "missing"), double_bracket)
setReplaceMethod("[[", c("SpatialField", "ANY", "missing", "ANY"), double_bracket_repl)
setReplaceMethod("[[", c("SpatialEntities", "ANY", "missing", "ANY"), double_bracket_repl)
setMethod("[[", c("SpatialAggregation", "ANY", "missing"), double_bracket)

#' assess whether function has complete coverage over the domain
#' 
#' fields with point support, defined for all areas in the domain are completely known; this function verifies that this is the case
#' 
#' @param x object of class \link{SpatialField-class}
#' @return logical; TRUE if support is "point" and area size of the observations is identical to that of the domain; this implies we have a coverage with knowledge of all points in the domain.
#' @note what the function does is not sufficient: identical area size does not guarantee that areas are identical
#' @export
full.coverage = function(x) {
	stopifnot(is(x, "SpatialField"))
	if (x@observations_equal_domain)
		return(TRUE)
	(gridded(x@domain@area) || is(x@domain@area, "SpatialPolygons")) &&
		isTRUE(all.equal(getArea(x@observations), 
			getArea(x@domain@area))) # allows for numerical fuzz
}
#' compute area of a Spatial* object
#'
#' compute the area of an object of one of the subclasses of \link[sp]{Spatial}
#'
#' area of gridded or polygon objects deriving from \link[sp]{Spatial}, zero
#' for other classes (points, lines)
#' @usage getArea(x)
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
#' plot method for SpatialField objects
#' 
#' plot method for SpatialField objects
#' 
#' @usage plot(x, ..., bg = grey(0.7))
#' @param x object of class \link{SpatialField}
#' @param bg background colour for domain
#' @export
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' plot(SpatialField(meuse, meuse.grid))
#' plot(SpatialField(meuse, meuse.area))
plot.SpatialField = function(x,..., bg = grey(0.7)) {
	if (gridded(x@domain@area))
		image(x@domain@area, col = bg)
	else
		plot(x@domain@area, col = bg)
	plot(x@observations, add = TRUE, ...)
}
